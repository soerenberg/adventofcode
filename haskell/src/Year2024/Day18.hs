module Year2024.Day18 where
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import AdventOfCode

dijkstra :: (a -> Bool) -> Grid a -> Z2 -> M.Map Z2 Int
dijkstra p g s = dijkstra' (M.fromList ((s,0):initL)) (S.singleton s) p g
  where initL = map (_2 .~ 1) . filter (p . snd) . neighbors4AtList g $ s

dijkstra' :: M.Map Z2 Int -> S.Set Z2 -> (a -> Bool) -> Grid a -> M.Map Z2 Int
dijkstra' acc vis p g =
  let us = M.toList . M.filterWithKey (\k _ -> not . (`S.member` vis) $ k) $ acc
      (minZ,minD) = minimumBy (\(_,a) (_,b) -> compare a b) $ us
      validZs = map fst . filter (p . snd) . neighbors4AtList g $ minZ
      newAcc = foldr (\z m -> M.insertWith min z (minD+1) m) acc validZs in
  if L.null us
  then acc
  else dijkstra' newAcc (S.insert minZ vis) p g

partA :: Int -> Int -> [Z2] -> Maybe Int
partA l n xs = let
  zs = [0..l]
  g = M.fromList [((i,j),True) | i<-[0..l], j<-[0..l]]
  gg = foldr (\(i,j) b -> M.insert (j,i) False b) g . take n $ xs in
  M.lookup (l,l) . dijkstra id gg $ (0,0)

partB :: Int -> Int -> [Z2] -> Maybe Int
partB _ 0 _ = Nothing
partB l n xs = maybe (partB l (n-1) xs) (Just . const (n+1)) (partA l n xs)

solve :: IO (Int, Int)
solve = do input <- pack <$> readFile "data/Year2024/day18.txt"
           let xs = fromRight [] $ parse (many line2Digits) "" input
           return (partA 70 1024 xs, partB 70 (length xs) xs)
