{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Year2022.Day03 (solve) where

import AdventOfCode

import qualified Data.Set as S

priority :: Char -> Int
priority c = let n = ord c in if n < 91 then n - 38 else n - 96

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (length xs `div` 2) xs

common :: Ord a => ([a], [a]) -> [a]
common (l, r) = S.toList $ sl `S.intersection` sr
  where sl = S.fromList l
        sr = S.fromList r

rucksackPriority :: [Int] -> Int
rucksackPriority = sum . common . splitHalf

groupPriority :: ([Int], [Int], [Int]) -> Maybe Int
groupPriority (a, b, c) = S.lookupMax cut
  where cut = foldr1 S.intersection (map S.fromList [a, b, c])

threeTups :: [a] -> [(a, a, a)]
threeTups (x:y:z:xs) = (x, y, z) : threeTups xs
threeTups _ = []

solve :: IO (Int, Int)
solve = do inputLines <- lines <$> readFile "data/Year2022/day03.txt"
           let ps = map (map priority) inputLines
           let partI = sum . map rucksackPriority $ ps
           let partII = sum . catMaybes . map groupPriority . threeTups $ ps
           return (partI, partII)
