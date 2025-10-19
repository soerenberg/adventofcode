module Year2016.Day01 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List

import AdventOfCode


data Instr = L Int | R Int deriving (Eq, Show)

data S = S { _dir :: Z2, _pos :: Z2} deriving (Eq, Show)
makeLenses ''S

instructions :: Parser [Instr]
instructions = instr `sepBy` char ','
  where instr = whitespace >> (l <|> r)
        l = L <$> (char 'L' >> digits)
        r = R <$> (char 'R' >> digits)

sim :: [Instr] -> State S Int
sim [] = do (x,y) <- use pos
            return $ abs x + abs y
sim (x:xs) = do
  let (rotFn, n) = case x of
                     (L i) -> (rot90ccw, i)
                     (R i) -> (rot90cw, i)
  newdir <- dir <%= rotFn
  pos %= (addZ2 $ smulZ2 n newdir)
  sim xs

solve :: String -> Either ParseError (Int, Int)
solve t = do
    is <- parse instructions "" (pack t)
    let c = evalState (sim is) (S (0,1) (0,0))
    pure (c, 0)
