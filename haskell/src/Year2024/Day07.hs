module Year2024.Day07 where
import qualified Data.Map as M
import qualified Data.Set as S

import AdventOfCode


check :: [Int -> Int -> Int] -> Int -> Int -> [Int] -> [Int]
check _ r a [] = guard (r == a) >> return r
check ops r a (x:xs) = do op <- ops
                          let a' = a `op` x
                          guard $ a' <= r
                          check ops r a' xs

cal :: [Int -> Int -> Int] -> [(Int, [Int])] -> Int
cal ops = foldr (\(r, xs) n -> n + (f $ check ops r (head xs) (tail xs))) 0
  where f xs = if null xs then 0 else head xs

line :: Parser (Int, [Int])
line = do r <- digits <* char ':' <* whitespace
          xs <- digits `sepBy` whitespace <* eol
          return (r, xs)

solve :: IO (Int, Int)
solve = do input <- pack <$> readFile "data/Year2024/day07.txt"
           let ps = fromRight [] $ parse (many line) "" input
           return (cal [(+),(*)] ps, 0)
