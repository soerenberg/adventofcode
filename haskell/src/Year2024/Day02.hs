module Year2024.Day02 where

import AdventOfCode


countSafeLvls :: [[Int]] -> Int
countSafeLvls = length . filter id . map isSafe

countSafeLvls' :: [[Int]] -> Int
countSafeLvls' = length . (filter or) . (map $ map isSafe). map subsets
  where subsets ys = [[y | (i,y) <- zip [1..] ys, i /= k] | k <- [1..length ys]]

isSafe :: [Int] -> Bool
isSafe [] = True
isSafe xs = let ds = zipWith (-) (tail xs) xs
                f x = 1 <= x && x <= 3 in
            all f ds || all (f . negate) ds

line :: Parser [Int]
line = digits `sepBy` whitespace <* eol

solve :: IO (Int, Int)
solve = do input <- pack <$> readFile "data/Year2024/day02.txt"
           let ps = fromRight [] $ parse (many line) "" input
           return (countSafeLvls ps, countSafeLvls' ps)
