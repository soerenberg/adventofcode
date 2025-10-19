module Year2016.Day01 (solve) where

import qualified Data.Set as S

import AdventOfCode


data Instr = L Int | R Int | F Int deriving (Eq, Show)

data S = S
  { _dir :: Z2         -- direction
  , _pos :: Z2         -- position
  , _his :: S.Set Z2   -- history of visited positions
  , _dfv :: Maybe Int   -- dist to first revisited position
  } deriving (Eq, Show)
makeLenses ''S

instructions :: Parser [Instr]
instructions = instr `sepBy` char ','
  where instr = whitespace >> (l <|> r)
        l = L <$> (char 'L' >> digits)
        r = R <$> (char 'R' >> digits)

sim :: [Instr] -> State S Int
sim [] = manZ2 <$> use pos
sim ((L n):xs) = (dir %= rot90ccw) >> (sim $ (F n):xs)
sim ((R n):xs) = (dir %= rot90cw) >> (sim $ (F n):xs)
sim ((F 0):xs) = sim xs
sim ((F n):xs) = do
  d <- use dir
  p <- pos <%= addZ2 d
  use his >>= (\h -> if p `S.member` h
                     then dfv %= (`mplus` (Just $ manZ2 p))
                     else his %= (p `S.insert`))
  sim $ (F $ n-1):xs

solve :: String -> Either ParseError (Int, Maybe Int)
solve t = do
    is <- parse instructions "" (pack t)
    let initS = S (0,1) (0,0) S.empty Nothing
    let (c, s) = runState (sim is) initS
    pure (c, _dfv s)
