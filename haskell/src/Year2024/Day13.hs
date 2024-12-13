module Year2024.Day13 where
import AdventOfCode

data Machine = Machine { _a :: Z2, _b :: Z2, _prize :: Z2 } deriving (Eq, Show)
makeLenses ''Machine

solveMachine :: Fractional b => Machine -> (b, b)
solveMachine (Machine (ax,ay) (bx,by) (px,py)) =
  let det = fromIntegral $ ax*by - bx*ay
      detA1 = fromIntegral $ px*by - py*bx
      detA2 = fromIntegral $ py*ax - px*ay in (detA1/det, detA2/det)

calcTokens :: [Machine] -> Int
calcTokens = round . eval . filter isZ2 . map solveMachine

eval :: Num a => [(a, a)] -> a
eval [] = 0
eval ((x,y):xs) = 3*x + y + eval xs

machine :: Parser Machine
machine = Machine <$> line2Digits <*> line2Digits <*> line2Digits <* whitespace

solve :: IO (Int, Int)
solve = do input <- pack <$> readFile "data/Year2024/day13.txt"
           let ms = fromRight [] $ parse (many machine) "" input
           let n = 10000000000000
           let ms' = map (\m -> m & prize %~ (\(x,y) -> (x+n,y+n)) ) ms
           return (calcTokens ms, calcTokens ms')
