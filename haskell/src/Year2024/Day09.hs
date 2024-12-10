module Year2024.Day09 where
import qualified Data.Set as S

import AdventOfCode

data Single = N Int | Spc deriving (Eq, Show)

toSingles :: [Int] -> [Single]
toSingles = toSingles' True 0
  where toSingles' _ _ [] = []
        toSingles' True n (0:xs) = toSingles' False n xs
        toSingles' True n (x:xs) = (N n) : (toSingles' True n ((x-1):xs))
        toSingles' False n (0:xs) = toSingles' True (n+1) xs
        toSingles' False n (x:xs) = Spc : (toSingles' False n ((x-1):xs))

moveSingles :: [Single] -> [Int]
moveSingles ls = moveSingles' (length zs) ls zs
  where zs = filter (/=Spc) . reverse $ ls
        moveSingles' 0 _ _ = []
        moveSingles' n ((N m):xs) ys = m:(moveSingles' (n-1) xs ys)
        moveSingles' n (Spc:xs) (Spc:ys) = moveSingles' n xs (tail ys)
        moveSingles' n (Spc:xs) ((N m):ys) = m:(moveSingles' (n-1) xs ys)

checkSum :: [Int] -> Int
checkSum = sum . zipWith (*) [0..]


solve :: IO (Int, Int)
solve = do input <- map digitToInt . init <$> readFile "data/Year2024/day09.txt"
           return $ over both checkSum (
             moveSingles . toSingles $ input, [])
