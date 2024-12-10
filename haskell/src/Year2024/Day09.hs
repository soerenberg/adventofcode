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

data Block = B Int Int | BSpc Int deriving (Eq, Show)

toBlocks :: [Int] -> [Block]
toBlocks = toBlocks' True . (zip [0..])
  where toBlocks' _ [] = []
        toBlocks' True ((i,x):xs) = (B x (i `div` 2)):(toBlocks' False xs)
        toBlocks' False ((_,x):xs) = (BSpc x):(toBlocks' True xs)

moveBlocks :: [Block] -> [Block]
moveBlocks xs = foldr moveBlock xs xs

moveBlock :: Block -> [Block] -> [Block]
moveBlock (BSpc _) xs = xs
moveBlock b@(B l i) (bb@(B l' i'):xs) = if b == bb then (B l i):xs
                                        else (B l' i'):(moveBlock (B l i) xs)
moveBlock b@(B l i) ((BSpc ls):xs)
  | l == ls = (B l i):xs
  | l < ls = (B l i):(BSpc (ls-l)):xs
  | otherwise = (BSpc ls):(moveBlock b xs)

fromBlocks :: [Block] -> [Int]
fromBlocks = fromBlocks' S.empty
  where fromBlocks' :: S.Set Int -> [Block] -> [Int]
        fromBlocks' _ [] = []
        fromBlocks' s ((BSpc l):xs) = (replicate l 0) ++ fromBlocks' s xs
        fromBlocks' s ((B l i):xs) =
          if i `S.member` s then (replicate l 0) ++ fromBlocks' s xs
          else (replicate l i) ++ (fromBlocks' (S.insert i s) xs)

solve :: IO (Int, Int)
solve = do input <- map digitToInt . init <$> readFile "data/Year2024/day09.txt"
           return $ over both checkSum (
             moveSingles . toSingles $ input,
             fromBlocks . moveBlocks . toBlocks $ input)
