module Year2016.Day07 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import AdventOfCode

import Debug.Trace (trace)

exampleInput :: String
exampleInput = """abba[mnop]qrst
abcd[bddb]xyyx
aaaa[qwer]tyui
ioxxoj[asdfgh]zxcvbn"""

isAbba :: (Char, Char, Char, Char) -> Bool
isAbba (a,b,c,d) = a == d && b == c && a /= b

hasAbba :: String -> Bool
hasAbba (a:b:c:d:xs) = isAbba (a,b,c,d) || hasAbba (b:c:d:xs)
hasAbba _ = False

hasTLS :: ([String], [String]) -> Bool
hasTLS (ls,rs) = any hasAbba ls && (not . any hasAbba $ rs)

splitAltRev :: [a] -> ([a], [a])
splitAltRev xs = splitAlt' xs False ([], [])
  where splitAlt' [] _ t = t
        splitAlt' (x:ys) False (l,r) = splitAlt' ys True (x:l, r)
        splitAlt' (x:ys) True (l,r) = splitAlt' ys False (l, x:r)

line :: Parser ([String], [String])
line = do
  xs <- letters `sepBy1` (char '[' <|> char ']')
  _ <- eolf
  pure . splitAltRev $ xs

solve :: String -> Either ParseError (Int, Int)
solve t = do
    ls <- parse (many line) "" (pack t)
    let n = length . filter hasTLS $ ls
    pure (n,2)
