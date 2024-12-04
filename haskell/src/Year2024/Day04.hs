module Year2024.Day04 where
import qualified Data.Map as M
import qualified Data.Set as S
import AdventOfCode

countXMASat :: Grid Char -> Z2 -> Char -> Int -> Int
countXMASat g k _ n = (+n) . sum . map (\ds -> isXMASinDir k ds g) $ dirs
  where dirs = [[(p*i,q*i) | i<-[0..3]] | p<-[-1..1], q<-[-1..1], p+q /= p*q]

isXMASinDir :: Z2 -> [Z2] -> Grid Char -> Int
isXMASinDir k ds g' = fromEnum . (=="XMAS") . lookupSeqAt k ds $ g'

solve :: IO (Int, Int)
solve = do g <- fromLines id . lines <$> readFile "data/Year2024/day04.txt"
           let partA = M.foldrWithKey (countXMASat g) 0 g
           let partB = 0
           return (partA, partB)