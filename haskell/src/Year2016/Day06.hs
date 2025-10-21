module Year2016.Day06 (solve) where

import qualified Data.List as L

import AdventOfCode


solve :: String -> Either String (String, String)
solve t = do
    let tlines = L.transpose . lines $ t
    pure (map mostCommon' tlines, map leastCommon' tlines)
