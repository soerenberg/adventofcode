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
machine = do ba <- (,) <$> (nonDigits >> digits) <*> (nonDigits >> digits) <* eol
             bb <- (,) <$> (nonDigits >> digits) <*> (nonDigits >> digits) <* eol
             p  <- (,) <$> (nonDigits >> digits) <*> (nonDigits >> digits) <* eol
             _ <- whitespace
             return $ Machine ba bb p

solve :: IO (Int, Int)
solve = do input <- pack <$> readFile "data/Year2024/day13.txt"
           let ms = fromRight [] $ parse (many machine) "" input
           return (calcTokens ms, 0)
