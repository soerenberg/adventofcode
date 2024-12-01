{-# LANGUAGE TemplateHaskell #-}
module Year2024.Day01 where

import Data.List (sort)
import AdventOfCode


totalDistance :: [(Int, Int)] -> Int
totalDistance = sum . (map dist) . (uncurry zip) . (over both sort) . unzip
  where dist (a,b) = abs (a-b)

line :: Parser (Int, Int)
line = (,) <$> digits <* whitespace <*> digits <* eol

solve :: IO (Int, Int)
solve = do input <- pack <$> readFile "data/Year2024/day01.txt"
           let ps = fromRight [] $ parse (many line) "" input
           return (totalDistance ps, 0)
