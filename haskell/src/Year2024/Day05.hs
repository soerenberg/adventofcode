module Year2024.Day05 where
import qualified Data.Map as M
import qualified Data.Set as S

import AdventOfCode

makeCmp :: S.Set (Int, Int) -> Int -> Int -> Ordering
makeCmp t x y = if (x,y) `S.member` t then LT else GT

mid :: [a] -> a
mid xs = xs !! (length xs `div` 2)

partA :: (Int -> Int -> Ordering) -> [[Int]] -> Int
partA c = foldr f 0
  where f a b = b + if isOrdered c a then mid a else 0

isOrdered :: (a -> a -> Ordering) -> [a] -> Bool
isOrdered c = all (==LT) . (zipWith c <*> tail)

partB :: (Int -> Int -> Ordering) -> [[Int]] -> Int
partB c = foldr f 0
  where f a b = b + if isOrdered c a then 0 else mid . sortBy c $ a

file :: Parser (S.Set (Int,Int), [[Int]])
file = do rs <- many $ (,) <$> digits <* char '|' <*> digits <* eol
          _ <- eol
          us <- many $ digits `sepBy` char ',' <* eol
          return (S.fromList rs, us)

solve :: IO (Int, Int)
solve = do input <- pack <$> readFile "data/Year2024/day05.txt"
           let (rs, us) = fromRight (S.empty,[]) $ parse file "" input
           let cmp = makeCmp rs
           return (partA cmp us, partB cmp us)
