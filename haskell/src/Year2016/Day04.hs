module Year2016.Day04 (solve, sortPairs, shiftLt) where

import Text.Parsec (endBy)
import Data.Ord  (comparing, Down(..))

import AdventOfCode

line :: Parser ([String], Int, String)
line = do
    xs <- letters `endBy` (char '-')
    n <- digits
    s <- char '[' >> letters <* char ']' <* eol
    return (xs, n, s)

sortPairs :: [(Char, Int)] -> [(Char, Int)]
sortPairs = sortBy (comparing (Down . snd) <> comparing fst)

isReal :: ([String], Int, String) -> Bool
isReal (xs, _, s) = ys == s
  where ys = map fst . take 5 . sortPairs . frequencies . mconcat $ xs

shiftLt :: Int -> Char -> Char
shiftLt i c = chr $ ((ord c - start + i) `mod` m) + start
  where (start, end) = (ord 'a', ord 'z')
        m = end-start+1

decrypt :: ([String], Int, String) -> String
decrypt (xs, n, _) = map (shiftLt n) . mconcat $ xs

isNorthPole :: String -> Bool
isNorthPole xs = take (length s) xs == s
  where s = "northpole"

solve :: String -> Either ParseError (Int, Int)
solve t = do
    input <- parse (many line) "" (pack t)
    let x = sum . map sec . filter isReal $ input
    let z = sum . map sec . filter (isNorthPole . decrypt) $ input
    pure (x, z)
  where sec = \(_,n,_) -> n
