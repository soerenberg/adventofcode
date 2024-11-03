module Year2022.Day02 (toInt, score, solve) where

toInt :: String -> Int
toInt s
  | s == "B" || s == "Y" = 1
  | s == "C" || s == "Z" = 2
  | otherwise            = 0

score ::  [String] -> Int
score = (s isPartA) . (map toInt)
  where s [l,r] = 3 * ((r - l + 1) `mod` 3) + r + 1
        s _ = 0

solve :: IO (Int, Int)
solve = do input <- readFile "data/Year2022/day02.txt"
           let ws = map words . lines $ input
           let partA = sum . (map $ score True) $ ws
           let partB = 0
           return (partA, partB)
