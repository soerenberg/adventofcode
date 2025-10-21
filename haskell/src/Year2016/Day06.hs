module Year2016.Day06 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.List

import AdventOfCode


solve :: String -> Either String (String, Int)
solve t = do
    let x = map mostCommon' . L.transpose . lines $ t
    pure (x, 0)
