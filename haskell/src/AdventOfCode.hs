{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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

  maybeToRight,
  minimumBy,
  sortBy,

  module Count,
  module Graph,
  module Grid,
  module Hash,
  -- module LogicProgramming,
  -- module NumberTheory,
  module OptParse,
  module Parser,
  notImplementedMsg,
  prettifyResult,
  inputFilePath,
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
import Data.Either.Combinators (maybeToRight)
import Data.List (dropWhileEnd, minimumBy, sortBy)
import Data.Maybe
import Data.Text (pack, Text)
import Data.Tuple (swap)
import Debug.Trace
import Language.Haskell.TH
import Lens.Micro.Platform

import Count
import Graph
import Grid
import Hash
-- import LogicProgramming
-- import NumberTheory
import OptParse
import Parser

notImplementedMsg :: String -> String
notImplementedMsg d = "Day " ++ d ++ " not implemented."

-- Define Display typeclass in order to avoid having newlines escaped if a
-- solution contains a string with linebreaks
class Display a where
  display :: a -> String

instance {-# OVERLAPPABLE #-} (Show a) => Display a where
  display = show

instance {-# OVERLAPPING #-} Display String where
  display = id

prettifyResult :: (Display e, Display a, Display b) => Either e (a, b) -> String
prettifyResult (Left e) = "AoC Error: " ++ (display e)
prettifyResult (Right (a, b)) = "a: " ++ (display a) ++ "\nb: " ++ (display b)

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
          (normalB [| dropWhileEnd (== '\n') <$> readFile param >>=
                      putStrLn . prettifyResult . $(varE fnName) |])
          []
        Nothing -> fail $ "Could not resolve " ++ fnPath

  fallback <- clause [wildP] (normalB [| putStrLn "Invalid day" |]) []
  pure [sig, FunD name (clauses ++ [fallback]) ]
