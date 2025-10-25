module Zn (
  addZ2
, smulZ2
, manZ2
, addZ4
, dirs4
, dirs4From
, dirs8
, dirs8From
, dirs9
, dirs9From
, isInt
, isZ2
, rot90cw
, rot90ccw
, Z2
, Z3
, Z4
) where

import Lens.Micro.Platform (both, over)

type Z2 = (Int, Int)
type Z3 = (Int, Int, Int)
type Z4 = (Int, Int, Int, Int)

dirs4 :: [Z2]
dirs4 = [(-1, 0), (0, -1), (0, 1), (1, 0)]

dirs8 :: [Z2]
dirs8 = filter (/=(0,0)) dirs9

dirs9 :: [Z2]
dirs9 = [(p,q) | p <- [-1, 0, 1], q <- [-1, 0, 1]]

dirs4From :: Z2 -> [Z2]
dirs4From (i,j) = map (addZ2 (i,j)) dirs4

dirs8From :: Z2 -> [Z2]
dirs8From (i,j) = map (addZ2 (i,j)) dirs8

dirs9From :: Z2 -> [Z2]
dirs9From (i,j) = map (addZ2 (i,j)) dirs9

addZ2 :: Z2 -> Z2 -> Z2
addZ2 (a,b) (x,y) = (a+x, b+y)

smulZ2 :: Int -> Z2 -> Z2
smulZ2 n (a, b) = (n * a, n * b)

manZ2 :: Z2 -> Int
manZ2 (a,b) = abs a + abs b

addZ4 :: Z4 -> Z4 -> Z4
addZ4 (a,b,c,d) (e,f,g,h) = (a+e, b+f, c+g, d+h)

rot90cw :: Z2 -> Z2
rot90cw (x,y) = (y,-x)

rot90ccw :: Z2 -> Z2
rot90ccw (x,y) = (-y,x)

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

isZ2 :: RealFrac a => (a,a) -> Bool
isZ2 = uncurry (&&) . over both isInt
