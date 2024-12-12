module Year2024.Day12 where
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import AdventOfCode

exploreAllRegions :: Grid Char -> State (S.Set Z2) [(Int, [Z2])]
exploreAllRegions g = M.toList g `forM` (uncurry . flip $ exploreRegion g)

exploreRegion :: Grid Char -> Char -> Z2 -> State (S.Set Z2) (Int, [Z2])
exploreRegion g p z =
  do isVisited <- S.member z <$> get
     let matches =  fromMaybe False $ (==p) <$> M.lookup z g
     if isVisited || (not matches) then return (0,[])
     else do modify $ S.insert z
             let fnc = numExposedSides g p z
             rs <- mapM (exploreRegion g p) (dirs4 z)
             return $ foldr (\(k,ys) (k',ys') -> (k+k',ys++ys')) (fnc,[z]) rs

numExposedSides :: Grid Char -> Char -> Z2 -> Int
numExposedSides g c z = sum $ do
  v <- dirs4 z
  let match = fromMaybe False $ (==c) <$> M.lookup v g
  if match then return 0 else return 1

partA :: [(Int, [Z2])] -> Int
partA = sum . map (\(k,xs) -> k * (length xs))

solve :: IO (Int, Int)
solve = do input <- readFile "data/Year2024/day12.txt"
           let g = fromLines id . lines $ input
           let rs = evalState (exploreAllRegions g) S.empty
           return (partA rs, 0)
