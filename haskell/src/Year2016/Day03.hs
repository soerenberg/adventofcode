module Year2016.Day03 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import AdventOfCode

countTris :: [(Int,Int,Int)] -> Int
countTris = length . filter isValid
  where isValid (a,b,c) = a+b>c && a+c>b && b+c>a

toB :: [(Int,Int,Int)] -> [(Int,Int,Int)]
toB [] = []
toB ((a,b,c):(d,e,f):(g,h,i):xs) = (a,d,g):(b,e,h):(c,f,i):(toB xs)
toB _ = [] -- should never happen

solve :: String -> Either ParseError (Int, Int)
solve t = do
    ts <- parse (many line3Digits) "" (pack t)
    pure (countTris ts, countTris . toB $ ts)
