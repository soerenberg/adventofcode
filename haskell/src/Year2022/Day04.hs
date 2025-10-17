{-# LANGUAGE OverloadedStrings #-}
module Year2022.Day04 (solve) where

import AdventOfCode


type Range = (Int, Int)

range :: Parser Range
range = (,) <$> digits <* char '-' <*> digits

line :: Parser (Range, Range)
line = (,) <$> range <* char ',' <*> range <* char '\n'

covers :: (Range, Range) -> Bool
covers (x, y) = x `contains` y || y `contains` x
  where contains (a, b) (c, d) = a >= c && b <= d

overlaps :: (Range, Range) -> Bool
overlaps (x, y) = x `overlapsAtTail` y || y `overlapsAtTail` x
  where overlapsAtTail (a, b) (c, _) = a <= c && c <= b

countWith :: ((Range, Range) -> Bool) -> [(Range, Range)] -> Int
countWith f xs = sum $ map (fromEnum . f) xs

solve :: String -> Either ParseError (Int, Int)
solve input = do
  x <- parse (many line) "" (pack input)
  return (countWith covers x, countWith overlaps x)
