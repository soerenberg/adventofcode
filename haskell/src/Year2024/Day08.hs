module Year2024.Day08 where
import qualified Data.Map as M
import qualified Data.Set as S
import AdventOfCode

groupBy :: Grid Char -> [[Z2]]
groupBy = M.elems. M.fromListWith (++)
          . map (\(z, c)->(c, [z]))
          . (filter ((/='.') . snd))
          . M.toList

inBounds :: (Int, Int) -> Z2 -> Bool
inBounds (bx,by) (x,y) = x >= 0 && y >= 0 && x < bx && y < by

makeAntinodes :: (Z2 -> Z2 -> [Z2]) -> [Z2] -> [Z2]
makeAntinodes f xs = do x <- xs
                        y <- xs
                        guard $ x/= y
                        f x y

leapAll :: (Int, Int) -> Z2 -> Z2 -> [Z2]
leapAll b v w = [v] ++ leapfrog b v (minusZ2 v w)

leapfrog :: (Int, Int) -> Z2 -> Z2 -> [Z2]
leapfrog b (sx,sy) v@(vx,vy) = let w = (sx+vx, sy+vy) in
    if inBounds b w then w:(leapfrog b w v) else []

leapSingle :: (Int, Int) -> Z2 -> Z2 -> [Z2]
leapSingle b v w = take 1 $ leapfrog b v (minusZ2 v w)

minusZ2 :: Z2 -> Z2 -> Z2
minusZ2 (a,b) (c,d) = (a-c,b-d)

countAntinodes :: (Z2 -> Z2 -> [Z2]) -> [[Z2]] -> Int
countAntinodes f groups = S.size . S.fromList $ groups >>= makeAntinodes f

solve :: IO (Int, Int)
solve = do ls <- lines <$> readFile "data/Year2024/day08.txt"
           let bounds = (length ls, length . (!!0) $ ls)
           let groups = groupBy . fromLines id $ ls
           return (countAntinodes (leapSingle bounds) groups,
                   countAntinodes (leapAll bounds) groups)
