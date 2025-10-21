module Year2016.Day03 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import AdventOfCode

isValidTri :: (Int,Int,Int) -> Bool
isValidTri (a,b,c) = a+b>c && a+c>b && b+c>a

solve :: String -> Either ParseError (Int, Int)
solve t = do
    input <- parse (many line3Digits) "" (pack t)
    let x = length $ filter isValidTri input
    pure (x,1)
