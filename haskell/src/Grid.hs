{-# LANGUAGE TemplateHaskell #-}
module Grid (
  addZ2
, smulZ2
, addZ4
, boundingBox
, dirs4
, dirs4From
, dirs8
, dirs8From
, dirs9
, dirs9From
, fromDims
, fromLines
, fromLinesToList
, isInt
, isZ2
, lookupSeq
, lookupSeqAt
, neighbors4At
, neighbors4AtList
, neighbors8At
, neighbors9At
, rot90cw
, rot90ccw
, setCoords
, toGridListWith
, toLines
, Grid
, Z2
, Z3
, Z4
) where

import Control.Monad.State
import Control.Monad.State.Lazy
import Data.Maybe
import qualified Data.Map as M
import qualified Data.PSQueue as P
import qualified Data.Set as S
import Data.PSQueue (Binding(..))
import Lens.Micro.Platform ((%=), (.=), both, makeLenses, over, use)

type Z2 = (Int, Int)
type Z3 = (Int, Int, Int)
type Z4 = (Int, Int, Int, Int)

type Grid a = M.Map Z2 a

fromLines :: (Char -> a) -> [String] -> Grid a
fromLines f ls = M.fromList . fromLinesToList f $ ls

fromLinesToList :: (Char -> a) -> [String] -> [(Z2, a)]
fromLinesToList  f xs = do (i, ls) <- zip [0..] xs
                           (j, x) <- zip [0..] ls
                           return ((i, j), f x)

-- TODO replace fromLinesToList with toGridListWith
toGridListWith :: (a -> b) -> [[a]] -> [(Z2, b)]
toGridListWith  f xs = do (i, ls) <- zip [0..] xs
                          (j, x) <- zip [0..] ls
                          return ((i, j), f x)

fromDims :: a -> Int -> Int -> Grid a
fromDims x h w = M.fromList [((i, j), x) | i<-[0..(h-1)], j<-[0..(w-1)]]

toLines :: Char -> (a -> Char) -> Grid a -> [String]
toLines c f grid = do i <- [imin..imax]
                      return $ map (charLookup i) [jmin..jmax]
  where (imin, imax, jmin, jmax) = fromMaybe (0, 0, 0, 0) $ boundingBox grid
        charLookup i' j' = fromMaybe c $ f <$> M.lookup (i', j') grid

boundingBox :: Grid a -> Maybe (Int, Int, Int, Int)
boundingBox grid = M.foldrWithKey f Nothing grid
  where f (i, j) _ (Just (imin, imax, jmin, jmax)) =
          Just (min i imin, max i imax, min j jmin, max j jmax)
        f (i, j) _ Nothing = Just (i, i, j, j)

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

neighbors4At :: Grid a -> Z2 -> [a]
neighbors4At grid z = catMaybes $ map (flip M.lookup grid) (dirs4From z)

neighbors8At :: Grid a -> Z2 -> [a]
neighbors8At grid z = catMaybes $ map (flip M.lookup grid) (dirs8From z)

neighbors9At :: Grid a -> Z2 -> [a]
neighbors9At grid z = catMaybes $ map (flip M.lookup grid) (dirs9From z)

neighbors4AtList :: Grid a -> Z2 -> [(Z2,a)]
neighbors4AtList grid z = catMaybes $ map (\z -> (z,) <$> M.lookup z grid) (dirs4From z)

setCoords :: [Z2] -> a -> Grid a -> Grid a
setCoords ks x grid = foldr (M.update (\_ -> Just x)) grid ks

lookupSeq :: [Z2] -> Grid a -> [a]
lookupSeq xs g = fromMaybe [] . sequence . map (\c -> M.lookup c g) $ xs

lookupSeqAt :: Z2 -> [Z2] -> Grid a -> [a]
lookupSeqAt (x,y) zs = lookupSeq [(x+z,y+z') | (z,z')<-zs]

addZ2 :: Z2 -> Z2 -> Z2
addZ2 (a,b) (x,y) = (a+x, b+y)

smulZ2 :: Int -> Z2 -> Z2
smulZ2 n (a, b) = (n * a, n * b)

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
