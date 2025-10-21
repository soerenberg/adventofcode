module Year2016.Day04 (solve, sortPairs) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Text.Parsec (endBy)
import Data.Ord  (comparing, Down(..))

import AdventOfCode

exampleInput :: String
exampleInput = """aaaaa-bbb-z-y-x-123[abxyz]
a-b-c-d-e-f-g-h-987[abcde]
not-a-real-room-404[oarel]
totally-real-room-200[decoy]\n"""

line :: Parser ([String], Int, String)
line = do
    xs <- letters `endBy` (char '-')
    -- xs <- manyTill (letters <* char '-') (lookAhead digits)
    -- char '-'
    n <- digits
    s <- char '[' >> letters <* char ']' <* eol
    return (xs, n, s)

sortPairs :: [(Char, Int)] -> [(Char, Int)]
sortPairs = sortBy (comparing (Down . snd) <> comparing fst)

isReal :: ([String], Int, String) -> Bool
isReal (xs, _, s) = ys == s
  where ys = map fst . take 5 . sortPairs . frequencies . mconcat $ xs

solve :: String -> Either ParseError (String, Int)
solve t = do
    input <- parse (many line) "" (pack t)
    let x = sum . map (\(_,n,_) -> n) . filter isReal $ input
    pure (show $ input, x)
