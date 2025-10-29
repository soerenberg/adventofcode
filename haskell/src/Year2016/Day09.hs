module Year2016.Day09 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import Text.Parsec (count)
import Debug.Trace (trace)

import AdventOfCode

p :: Parser String
p = concat <$> many1 (letters <|> marker)
  where marker = do m <- char '(' >> digits <* char 'x'
                    n <- digits <* char ')'
                    xs <- count m anyChar
                    pure . concat . replicate n  $ xs

solve :: String -> Either ParseError (Int, Int)
solve t = do
    t <- parse p "" (pack t)
    pure (length t,0)
