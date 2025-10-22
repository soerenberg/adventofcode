module Year2016.Day05 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import AdventOfCode

isValid :: String -> Bool
isValid = (== "00000") . take 5

srch :: String -> String
srch s = take 8 [h !! 5 | i <- [0..], let h = strMD5 (s ++ (show i)), isValid h]

solve :: String -> Either String (String, String)
solve t = do
    pure (srch t, "")
