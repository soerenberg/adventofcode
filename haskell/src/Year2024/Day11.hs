module Year2024.Day11 where
import qualified Data.Map as M
import AdventOfCode

blink :: Int -> [Int]
blink x
  | x == 0 = [1]
  | even . length . show $ x = g . over both read . splitHalf . show $ x
  | otherwise = [x * 2024]
  where splitHalf ys = splitAt (length ys `div` 2) ys
        g (a,b) = [a,b]

blinkAll :: M.Map Int Int -> M.Map Int Int
blinkAll m =  M.fromListWith (+) $ M.foldrWithKey f [] m
  where f k n xs = (++xs) . map (, n) . blink $ k

blinkNTimes :: M.Map Int Int -> Int -> Int
blinkNTimes m n = M.foldr (+) 0 . foldr (\_ l -> blinkAll l) m $ [1..n]

solve :: IO (Int, Int)
solve = do input <- map read . words <$> readFile "data/Year2024/day11.txt"
           let m = M.fromList . map (,1) $ input
           return $ over both (blinkNTimes m) (25, 75)
