module Year2024.Day05 where
import qualified Data.Map as M
import qualified Data.Set as S

import AdventOfCode

lookupFromRules :: [(Int,Int)] -> M.Map Int (S.Set Int)
lookupFromRules = foldr update M.empty
 where update (x,y) t = M.insertWith S.union x (S.singleton y) t

makeCmp :: M.Map Int (S.Set Int) -> Int -> Int -> Ordering
makeCmp t x y = let s = M.findWithDefault S.empty x t in
                if y `S.member` s then LT else GT

mid :: [a] -> a
mid xs = xs !! (length xs `div` 2)

partA :: (Int -> Int -> Ordering) -> [[Int]] -> Int
partA c = sum . map mid . filter (isOrdered c)

isOrdered :: (a -> a -> Ordering) -> [a] -> Bool
isOrdered c = all (==LT) . (zipWith c <*> tail)

partB :: (Int -> Int -> Ordering) -> [[Int]] -> Int
partB c = sum . map (mid . sortBy c) . filter (not . isOrdered c)

file :: Parser ([(Int,Int)], [[Int]])
file = do rs <- many $ (,) <$> digits <* char '|' <*> digits <* eol
          _ <- eol
          us <- many $ digits `sepBy` char ',' <* eol
          return (rs, us)

solve :: IO (Int, Int)
solve = do input <- pack <$> readFile "data/Year2024/day05.txt"
           let (rs, us) = fromRight ([],[]) $ parse file "" input
           let cmp = makeCmp . lookupFromRules $ rs
           return (partA cmp us, partB cmp us)
