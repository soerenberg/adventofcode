module Year2024.Day14 where
import AdventOfCode

run :: Z2 -> Int -> (Z2, Z2) -> Z2
run (bx,by) n ((px,py),(vx,vy)) = ((px + n*vx) `mod` bx, (py + n*vy) `mod` by)

eval :: Z2 -> [Z2] -> Z4
eval b = foldr (\z t -> (addZ4 t) . (quadTup b) $ z) (0,0,0,0)

quadTup :: Z2 ->  Z2 -> (Int,Int,Int,Int)
quadTup (bx, by) (x,y) = let tx = bx `div` 2
                             ty = by `div` 2 in
  case (x==tx || y == ty, x <= tx, y <= ty) of
    (True,_,_) -> (0,0,0,0)
    (_,True,True) -> (1,0,0,0)
    (_,True,False) -> (0,1,0,0)
    (_,False,True) -> (0,0,1,0)
    otherwise -> (0,0,0,1)

mulZ4 :: Z4 -> Int
mulZ4 (a,b,c,d) = a*b*c*d

line :: Parser (Z2,Z2)
line = do
  p <- (,) <$> (nonSignedDigits >> signedInt) <*> (nonSignedDigits >> signedInt)
  v <- (,) <$> (nonSignedDigits >> signedInt) <*> (nonSignedDigits >> signedInt)
  _ <- eol
  return (p,v)

solve :: IO (Int, Int)
solve = do input <- pack <$> readFile "data/Year2024/day14.txt"
           let ms = fromRight [] $ parse (many line) "" input
           let bounds = (101,103)
           return (mulZ4 . eval bounds . map (run bounds 100) $ ms,
                   0)
