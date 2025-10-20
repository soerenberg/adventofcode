module Year2016.Day02 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import AdventOfCode

direction :: Parser Z2
direction = (char 'U' >> pure (0,1))
            <|> (char 'D' >> pure (0,-1))
            <|> (char 'L' >> pure (-1,0))
            <|> (char 'R' >> pure (1,0))

isOOB :: Z2 -> Bool
isOOB (x,y) = if (x > 2 || x < 0 || y > 2 || y < 0) then True else False

move :: Z2 -> Z2 -> Z2
move p d = let q = addZ2 p d in
    if isOOB q then p else q

moves :: (Z2, [Int]) -> [Z2] -> (Z2, [Int])
moves (p,ns) [] = (p, ns++[toNumPadKey p])
moves (p,ns) (x:xs) = moves (move p x, ns) xs

toNumPadKey :: Z2 -> Int
toNumPadKey (x,y) = (2-y) * 3 + x + 1

solve :: String -> Either ParseError (String, Int)
solve t = do
    input <- parse (manyTill (many1 direction <* eol) eof) "" (pack t)
    let (_, ns) = foldl moves ((1,1),[]) input
    return (map intToDigit ns, length input)
