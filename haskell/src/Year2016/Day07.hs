module Year2016.Day07 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import AdventOfCode

import Debug.Trace (trace)

isAbba :: (Char, Char, Char, Char) -> Bool
isAbba (a,b,c,d) = a == d && b == c && a /= b

hasAbba :: String -> Bool
hasAbba (a:b:c:d:xs) = isAbba (a,b,c,d) || hasAbba (b:c:d:xs)
hasAbba _ = False

hasTLS :: [String] -> Bool
hasTLS xs = hasTLS' True False xs
  where hasTLS' True b (x:xs) = hasTLS' False (b || hasAbba x) xs
        hasTLS' False b (x:xs) = (not . hasAbba $ x) && hasTLS' True b xs
        hasTLS' _ b [] = b

line :: Parser [String]
line = do
  xs <- letters `sepBy1` (char '[' <|> char ']')
  _ <- eolf
  pure xs

solve :: String -> Either ParseError (Int, Int)
solve t = do
    ls <- parse (many line) "" (pack t)
    let n = length . filter hasTLS $ ls
    pure (n, 0)
