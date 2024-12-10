{-# LANGUAGE TemplateHaskell #-}
module Year2024.Day10 where
import qualified Data.Map as M
import qualified Data.Set as S

import AdventOfCode

findEnds :: Grid Int -> Int -> Z2 -> [Z2]
findEnds g n z = do let isN = (==n) <$> M.lookup z g
                    guard $ fromMaybe False isN
                    if n == 9 then [z]
                    else do nxt <- dirs4 z
                            findEnds g (n+1) nxt

solve :: IO (Int, Int)
solve = do g <- fromLines digitToInt . lines <$> readFile "data/Year2024/day10.txt"
           let ends = map (findEnds g 0) (M.keys g)
           return (sum . map (S.size . S.fromList) $ ends, 0)
