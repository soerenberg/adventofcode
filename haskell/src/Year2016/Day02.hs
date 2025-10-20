module Year2016.Day02 (solve) where

import AdventOfCode

direction :: Parser Z2
direction = (char 'U' >> pure (0,1))
            <|> (char 'D' >> pure (0,-1))
            <|> (char 'L' >> pure (-1,0))
            <|> (char 'R' >> pure (1,0))

isOOB :: Bool -> Z2 -> Bool
isOOB False (x,y) = if (x > 2 || x < 0 || y > 2 || y < 0) then True else False
isOOB True (x,y) = if (x+y>6 || x-y>2 || y-x>2 || -x-y> -2) then True else False

move :: Bool -> Z2 -> Z2 -> Z2
move isB p d = let q = addZ2 p d in
    if isOOB isB q then p else q

moves :: Bool -> (Z2, String) -> [Z2] -> (Z2, String)
moves isB (p,ns) [] = (p, ns++[toNumPadKey isB p])
moves isB (p,ns) (x:xs) = moves isB (move isB p x, ns) xs

toNumPadKey :: Bool -> Z2 -> Char
toNumPadKey False (x,y) = intToDigit $ (2-y) * 3 + x + 1
toNumPadKey True (2,4) = '1'
toNumPadKey True (x,3) = intToDigit $ x+1
toNumPadKey True (x,2) = intToDigit $ x+5
toNumPadKey True (x,1) = toUpper $ intToDigit $ x+9
toNumPadKey True _ = 'D'

solve :: String -> Either ParseError (String, String)
solve t = do
    input <- parse (manyTill (many1 direction <* eol) eof) "" (pack t)
    let (_, ns) = foldl (moves False) ((1,1),[]) input
    let (_, ns') = foldl (moves True) ((0,2),[]) input
    return (ns, ns')
