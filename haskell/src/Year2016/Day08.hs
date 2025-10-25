module Year2016.Day08 (solve) where

import Data.Bool (bool)

import AdventOfCode

data T = RotC Int Int | RotR Int Int | Rect Int Int deriving (Eq, Show)
type FGrid = (Z2 -> Bool)

printFGrid :: Int -> Int -> FGrid -> [String]
printFGrid w h g = [ [bool '.' '#' . g $ (x,y) | x<-[0..(w-1)]] |  y<-[0..(h-1)]]

width :: Int
width = 50

height :: Int
height = 6

applyInstr :: T -> (FGrid -> FGrid)
applyInstr (Rect w h) = (\g -> (\(x,y) -> if x<w && y<h then True else g (x,y)))
applyInstr (RotC rx n) = (\g -> (\(x,y) -> if x==rx then g (x, (y-n) `mod` height) else g (x,y)))
applyInstr (RotR cy n) = (\g -> (\(x,y) -> if y==cy then g ((x-n) `mod` width,y) else g (x,y)))

line :: Parser T
line = (rotc <|> rotr <|> rect) <* eolf
  where rotr = uncurry RotR <$> ((try $ string "rotate row") >> any2Digits)
        rotc = uncurry RotC <$> ((try $ string "rotate column") >> any2Digits)
        rect = uncurry Rect <$> ((try $ string "rect") >> any2Digits)

solve :: String -> Either ParseError (Int, String)
solve t = do
    ls <- parse (many line) "" (pack t)
    let final =  foldl (flip ($)) (const False) $ map applyInstr ls
    let nLit = length. filter final $ [(x,y) | x<-[0..(width-1)], y<-[0..(height-1)]]
    pure (nLit, "")
