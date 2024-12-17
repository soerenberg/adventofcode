module Year2024.Day17 where
import Data.Array (Array, bounds, listArray, (!))
import AdventOfCode

eval :: Int -> Int -> Int -> Int -> (Array Int (Int,Int)) -> [Int]
eval a b c i xs =
  if i > (snd . bounds $ xs) then []
  else eval' a b c i xs (xs ! i)
  where
    eval' a b c i xs (k,op) = let cmb = combo a b c op in
      case (k,op) of
             (0,op) -> eval (a `div` 2^cmb) b c (i+1) xs
             (1,op) -> eval a (b `xor` op) c (i+1) xs
             (2,op) -> eval a (cmb `mod` 8) c (i+1) xs
             (3,op) -> if a == 0 then eval a b c (i+1) xs
                         else eval a b c op xs
             (4,_) -> eval a (b `xor` c) c (i+1) xs
             (5,op) -> (cmb `mod` 8):(eval a b c (i+1) xs)
             (6,op) -> eval a (a `div` 2^cmb) c (i+1) xs
             (7,op) -> eval a b (a `div` 2^cmb) (i+1) xs
    combo _ _ _ 0 = 0
    combo _ _ _ 1 = 1
    combo _ _ _ 2 = 2
    combo _ _ _ 3 = 3
    combo a _ _ 4 = a
    combo _ b _ 5 = b
    combo _ _ c 6 = c

line :: Parser (Int,Int,Int,[Int])
line = do a <- string "Register A: " >> digits <* eol
          b <- string "Register B: " >> digits <* eol
          c <- string "Register C: " >> digits <* eol
          _ <- whitespaceEOL
          xs <- string "Program: " >> digits `sepBy` char ','
          return (a,b,c, xs)

pair :: [a] -> [(a,a)]
pair [] = []
pair [x] = []
pair (x:y:xs) = (x,y):(pair xs)

solve :: IO ([Int], Int)
solve = do input <- pack <$> readFile "data/Year2024/day17.txt"
           return ([],0)
           let ms =  parse line "" input
           case ms of
             (Left e) -> do print e
                            return ([],0)
             (Right (a,b,c,xs)) -> do --
               let is = pair xs
               let as = listArray (0, length is - 1) is
               return (eval a b c 0 as, 0)
