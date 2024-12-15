module Grid (
  addZ2
, addZ4
, boundingBox
, dirs4
, fromDims
, fromLines
, fromLinesToList
, isInt
, isZ2
, lookupSeq
, lookupSeqAt
, neighbors4At
, neighbors8At
, neighbors9At
, rot90cw
, setCoords
, toGridListWith
, toLines
, Grid
, Z2
, Z3
, Z4
) where

import Data.Maybe
import qualified Data.Map as M
import Lens.Micro.Platform (both, over)

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

neighbors8At :: Grid a -> Z2 -> [a]
neighbors8At grid (i, j) = catMaybes $ map (flip M.lookup grid) coords
  where coords = [(i+p, j+q) | p <- [-1, 0, 1], q <- [-1, 0, 1], (p,q) /= (0,0)]

neighbors9At :: Grid a -> Z2 -> [a]
neighbors9At grid (i, j) = catMaybes $ map (flip M.lookup grid) coords
  where coords = [(i+p, j+q) | p <- [-1, 0, 1], q <- [-1, 0, 1]]

neighbors4At :: Grid a -> Z2 -> [a]
neighbors4At grid (i, j) = catMaybes $ map (flip M.lookup grid) coords
  where coords = [(i+p, j+q) | (p,q) <- [(-1, 0), (0, -1), (0, 1), (1, 0)]]

setCoords :: [Z2] -> a -> Grid a -> Grid a
setCoords ks x grid = foldr (M.update (\_ -> Just x)) grid ks

lookupSeq :: [Z2] -> Grid a -> [a]
lookupSeq xs g = fromMaybe [] . sequence . map (\c -> M.lookup c g) $ xs

lookupSeqAt :: Z2 -> [Z2] -> Grid a -> [a]
lookupSeqAt (x,y) zs = lookupSeq [(x+z,y+z') | (z,z')<-zs]

addZ2 :: Z2 -> Z2 -> Z2
addZ2 (a,b) (x,y) = (a+x, b+y)

addZ4 :: Z4 -> Z4 -> Z4
addZ4 (a,b,c,d) (e,f,g,h) = (a+e, b+f, c+g, d+h)

rot90cw :: Z2 -> Z2
rot90cw (x,y) = (y,-x)

dirs4 :: Z2 -> [Z2]
dirs4 (i,j) = [(i+p, j+q) | (p,q) <- [(-1, 0), (0, -1), (0, 1), (1, 0)]]

isInt :: RealFrac a => a -> Bool
isInt x = x == fromInteger (round x)

isZ2 :: RealFrac a => (a,a) -> Bool
isZ2 = uncurry (&&) . over both isInt
