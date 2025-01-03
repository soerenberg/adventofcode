module Year2024.Day18 where
import qualified Data.List as L
import qualified Data.Map as M
import AdventOfCode

partA :: Int -> Int -> [Z2] -> Maybe Int
partA l n xs = let
  zs = [0..l]
  g = M.fromList [((i,j),True) | i<-[0..l], j<-[0..l]]
  gg = foldr (\(i,j) -> M.insert (j,i) False) g . take n $ xs in
  liftM fst . M.lookup (l,l) . dijkstra (0,0) $ (toAdj gg)

toAdj :: M.Map Z2 Bool -> (Z2 -> [(Z2, Int)])
toAdj g = map (_2 .~ 1) . filter snd . neighbors4AtList g

partB :: Int -> Int -> [Z2] -> Maybe String
partB l n xs = do
  i <- getIndex l n xs
  return . L.intercalate "," . (^.. each) . over both show . (!! i) $ xs
  where getIndex _ 0 _ = Nothing
        getIndex l' n' xs' = case partA l' n' xs' of
                            Nothing -> getIndex l' (n'-1) xs'
                            (Just _) -> Just n'

solve :: IO (Maybe Int, Maybe String)
solve = do input <- pack <$> readFile "data/Year2024/day18.txt"
           let xs = fromRight [] $ parse (many line2Digits) "" input
           return (partA 70 1024 xs, partB 70 (length xs) xs)
