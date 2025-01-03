module Year2024.Day16 where
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import AdventOfCode

data F = Wall | Free | End deriving (Eq, Show)

charToF :: Char -> F
charToF '#' = Wall
charToF 'E' = End
charToF _ = Free

adjacency :: Grid F -> (Z2,Z2) -> [((Z2,Z2), Int)]
adjacency g (v,d) = let xs = [((v, rot90cw d), 1000), ((v, rot90ccw d), 1000)]
                        w = v `addZ2` d in
    case M.lookup w g of
      Nothing -> xs
      (Just Wall ) -> xs
      (Just _) -> ((w, d), 1):xs

solve :: IO (Maybe Int, Int)
solve = do xs <- lines <$> readFile "data/Year2024/day16.txt"
           let s' = maybe (0,0) fst . L.find snd . toGridListWith (=='S') $ xs
           let t = maybe (0,0) fst . L.find snd . toGridListWith (=='E') $ xs
           let g = fromLines charToF xs

           let s = (s', (0,1))
           let endPts = [(t, d) | d <- dirs4]
           let ds = dijkstra s (adjacency g)
           let partA = liftM minimum . sequence . foldr (\z -> (:) (fst <$> M.lookup z ds)) [] $ endPts
           let allPaths = endPts >>= recoverPaths ds s >>= map fst
           let partB = S.size . S.fromList $ allPaths

           return (partA,partB)
