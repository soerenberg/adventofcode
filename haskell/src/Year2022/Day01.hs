{-# LANGUAGE TemplateHaskell #-}
module Year2022.Day01 where

import qualified Data.Map as M

import AdventOfCode


count :: String -> (Int, Int)
count = count' 0 0 1
  where count' n m _ [] = (n, m)
        count' n m c (x:xs) = let n' = n + toNum x
                                  m' = if n' == -1 && m ==0 then c else m in
                              count' n' m' (c+1) xs
        toNum '(' = 1
        toNum ')' = -1
        toNum _ = 0


solve :: IO (Int, Int)
solve = do input <- readFile "data/Year2022/day01.txt"
           return $ count input
