{-# LANGUAGE TemplateHaskell #-}
module AdventOfCode (
  module Control.Arrow,
  module Control.Monad,
  module Control.Monad.Logic,
  module Control.Monad.Reader,
  module Control.Monad.State,
  module Control.Monad.Trans.Maybe,
  module Control.Monad.State.Lazy,
  module Data.Bits,
  module Data.Char,
  module Data.Either,
  module Data.Maybe,
  module Data.Text,
  module Data.Tuple,
  module Debug.Trace,
  module Lens.Micro.Platform,

  minimumBy,
  sortBy,

  module Graph,
  module Grid,
  -- module LogicProgramming,
  -- module NumberTheory,
  module OptParse,
  module Parser,
  prettifyResult,
  makePrintResultForDay,
) where

import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.State.Lazy
import Data.Bits
import Data.Char
import Data.Either
import Data.List (minimumBy, sortBy)
import Data.Maybe
import Data.Text (pack, Text)
import Data.Tuple (swap)
import Debug.Trace
import Language.Haskell.TH
import Lens.Micro.Platform

import qualified Data.Map as M
import qualified Data.Set as S

import Graph
import Grid
-- import LogicProgramming
-- import NumberTheory
import OptParse
import Parser


prettifyResult :: (Show e, Show a, Show b) => Either e (a, b) -> String
prettifyResult (Left e) = "AoC Error: " ++ (show e)
prettifyResult (Right (a, b)) = "a: " ++ (show a) ++ "\nb: " ++ (show b)

padDay :: Int -> String
padDay d = if d < 10 then "0" ++ show d else show d

inputFilePath :: Int -> Int -> String
inputFilePath y d = "data/Year" ++ (show y) ++ "/day" ++ padDay d ++ ".txt"

makePrintResultForDay :: Int -> [Int] -> Q [Dec]
makePrintResultForDay  year days = do
  let name = mkName "printResultForDay"
  let sig = SigD name (
              AppT (AppT ArrowT (ConT ''String)) (AppT (ConT ''IO) (TupleT 0)))

  clauses <- forM days $ \d -> do
      let fnPath = "Day" ++ padDay d ++ ".solve"
          param = inputFilePath year d

      maybeFnName <- lookupValueName fnPath

      case maybeFnName of
        Just fnName -> clause [litP (StringL $ show d)]
          (normalB [| readFile param >>=
                      putStrLn . prettifyResult . $(varE fnName) |])
          []
        Nothing -> fail $ "Could not resolve " ++ fnPath

  fallback <- clause [wildP] (normalB [| putStrLn "Invalid day" |]) []
  pure [sig, FunD name (clauses ++ [fallback]) ]
