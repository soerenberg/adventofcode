module Year2016.Day09 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import Text.Parsec (count)
import Debug.Trace (trace)

import AdventOfCode

countLength :: Bool -> Parser Int
countLength isB = sum <$> many1 (l <|> marker)
  where l = length <$> letters
        marker = do m <- char '(' >> digits <* char 'x'
                    n <- digits <* char ')'
                    xs <- count m anyChar
                    if isB
                    then do let ys = concat . replicate n $ xs
                            let z = parse (countLength isB) "" (pack ys)
                            pure . fromRight 0 $ z
                    else pure $ n * m

solve :: String -> Either ParseError (Int, Int)
solve t = do
    a <- parse (countLength False) "" (pack t)
    b <- parse (countLength True) "" (pack t)
    pure (a, b)
