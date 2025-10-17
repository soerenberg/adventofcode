module Year2022.Day02 (toInt, score, solve) where

import AdventOfCode

toInt :: String -> Int
toInt s
  | s == "B" || s == "Y" = 1
  | s == "C" || s == "Z" = 2
  | otherwise            = 0

score :: Bool -> [String] -> Int
score isPartA = (s isPartA) . (map toInt)
  where s True [l,r] = 3 * ((r - l + 1) `mod` 3) + r + 1
        s False [l,r] = r * 3 + (l + r - 1) `mod` 3 + 1
        s _ _ = 0

solve :: String -> Either String (Int, Int)
solve t = do
   let ws = map words . lines $ t
   let partA = sum . (map $ score True) $ ws
   let partB = sum . (map $ score False) $ ws
   return (partA, partB)
