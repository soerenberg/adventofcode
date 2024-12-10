module Year2024.Day09 where
import qualified Data.Map as M
import qualified Data.Set as S

import AdventOfCode

data D = N Int | S deriving (Eq, Show)


foo :: [Int] -> [D]
foo = foo' True 0
  where foo' _ _ [] = []
        foo' True n (0:xs) = foo' False n xs
        foo' True n (x:xs) = (N n) : (foo' True n ((x-1):xs))
        foo' False n (0:xs) = foo' True (n+1) xs
        foo' False n (x:xs) = S : (foo' False n ((x-1):xs))

fix :: [D] -> [D]
fix xs = fix' (length ys) xs ys
  where ys = filter (/=S) . reverse $ xs

fix' :: Int -> [D] -> [D] -> [D]
fix' 0 _ _ = []
-- fix' xs [] = []
fix' n ((N m):xs) ys = (N m):(fix' (n-1) xs ys)
-- fix' (S:xs) [] = []
fix' n (S:xs) ys = (head ys):(fix' (n-1) xs (tail ys))


checkSum :: [Int] -> Int
checkSum xs = sum $ zipWith (*) [0..] xs

deco :: D -> Int
deco (N n) = n


decos :: [D] -> [Int]
decos = map deco

line :: Parser [Int]
line = digits `sepBy` whitespace <* eol


data B = B Int Int | Sp Int deriving (Eq, Show)

bar :: [Int] -> [B]
bar xs = bar' True $ zip [0..] xs
  where bar' _ [] = []
        bar' True ((i,x):xs) = (B x (i `div` 2)):(bar' False xs)
        bar' False ((i,x):xs) = (Sp x):(bar' True xs)

-- fix2 :: [B] -> [B]
-- fix2 xs = fix22 xs (reverse xs)

-- fix22 :: [B] -> [B]-> [B]
-- fix22 ((B l n):xs _ = (B l n):(fix 22 xs)
-- fix22 xs ((S _):ys) = fix22 xs ys
-- fix22 ((S lx):xs) ((B ly vy):ys) =    fix22 xs ys

insrtAll :: [B] -> [B]
insrtAll xs = insrtAll' xs xs

insrtAll' :: [B] -> [B] -> [B]
insrtAll' xs ys = foldr (\a b -> insrtt a b) xs ys

-- insrtAll xs = 

insrtt :: B -> [B] -> [B]
insrtt (Sp _) xs = xs
insrtt b@(B l i) (bb@(B l' i'):xs) = if b == bb 
                                     then (B l i):xs
                                     else (B l' i'):(insrtt (B l i) xs)
-- insrtt (B l i) ((B a b):xs) = (B a b):(insrtt (B l i) xs)
insrtt b@(B l i) ((Sp ls):xs) =
  if l == ls
  then (B l i):xs
  else if l < ls
       then (B l i):(Sp (ls-l)):xs
       else (Sp ls):(insrtt b xs)


-- insrt :: Int -> Int -> [B] -> [B]
-- insrt l i ((B l i):xs) = (B l i):xs
-- insrt l i ((B a b):xs) = (B a b):(insrt l i xs)
-- insrt l i ((S ls):xs) =
--   if l == ls
--   then (B l i):xs
--   else if l < ls
--        then (B l i):(S (ls-ls)):xs
--        else (S ls):(insrt l i xs)


-- fix33 :: Int -> M.Map Int Int -> [B] -> [B]
-- fix33 i mp xs = case tryFindSpace (M.findWithDefault 0 i mp) xs of
--                  () -> 

-- tryFindSpace :: Int -> Int -> [B] -> Maybe [B]
-- tryFindSpace _ _ [] = Nothing
-- tryFindSpace i l ((B _ _):xs)) = 
-- tryFindSpace i l ((S ls):xs) =
--   if l == ls
--   then Just $ (B l i):xs
--   else if l < ls
--        then Just $ (B l i):(S (ls-l)):xs
--        else do bs <- tryFindSpace i l xs
--                return $ (S ls):bs

-- moveInto :: B -> [B] -> Maybe [B]
-- moveInto b xs = 

expnd :: [B] -> [Int]
expnd = expnd' S.empty

expnd' :: S.Set Int -> [B] -> [Int]
expnd' _ [] = []
expnd' s ((Sp l):xs) = (replicate l 0) ++ expnd' s xs
expnd' s ((B l i):xs) = if i `S.member` s then (replicate l 0) ++ expnd' s xs
                        else (replicate l i) ++ (expnd' (S.insert i s) xs)

-- checkSum2 :: [B] -> Int
-- checkSum2 = checkSum2' 0 S.empty

-- checkSum2' :: Int -> S.Set Int -> [B] -> Int
-- checkSum2' n s (Sp _:xs) = checkSum2' n s xs
-- checkSum2' n s ((B l i):xs) = if i `S.member` s
--                               then checkSum2' n s xs
--                               else n*i


solve :: IO (Int, Int)
solve = do --input <- map (N . read) <$> readFile "data/Year2024/day09_test.txt"
           input <- map digitToInt . init <$> readFile "data/Year2024/day09.txt"
           -- input <- map digitToInt . init <$> readFile "data/Year2024/day09_test.txt"
           -- let input = map digitToInt "12345"
           -- print $ input
           -- print $ foo input
           print $ checkSum . decos . fix . foo $ input
           print $ checkSum . expnd . insrtAll . bar $ input

           -- print $ "%%%%%"

           -- print $ bar $ input
           -- print $ insrtAll . bar $ input
           -- print $ expnd . insrtAll . bar $ input
           -- print "**********"

           -- print $ insrtAll' (bar input) [B 2 9, B 3 7, B 2 4, B 1 2]
           -- print "^^^"
           -- print $ insrtAll' (bar input) (reverse [B 4 8,B 2 9])
           -- print $ insrtt (B 3 7) $ insrtAll' (bar input) (reverse [B 4 8,B 2 9])
           -- print $                  insrtAll' (bar input) (reverse [B 3 7,B 4 8,B 2 9])
           -- -- print $                  insrtAll ([B 3 7,B 4 8,B 2 9])
           -- print $ insrtAll' (bar input) (reverse [B 2 0, B 3 1,B 1 2,B 3 3,B 2 4,B 4 5,B 4 6,B 3 7,B 4 8,B 2 9])
           -- print $ insrtAll' (bar input) (reverse [B 2 0,Sp 3,B 3 1,Sp 3,B 1 2,Sp 3,B 3 3,Sp 1,B 2 4,Sp 1,B 4 5,Sp 1,B 4 6,Sp 1,B 3 7,Sp 1,B 4 8,Sp 0,B 2 9])
           -- print "fr"
           -- print $ insrtt (B 1 2) $ insrtt (B 2 4) $ insrtt (B 3 7) $ insrtt (B 2 9) (bar input)

           -- print $ "xxx"
           -- print $ insrtt (B 2 9) (bar input)
           -- print $ insrtt (B 3 7) $ insrtt (B 2 9) (bar input)
           -- print $ insrtt (B 2 4) $ insrtt (B 3 7) $ insrtt (B 2 9) (bar input)
           -- print $ insrtt (B 1 2) $ insrtt (B 2 4) $ insrtt (B 3 7) $ insrtt (B 2 9) (bar input)
           -- print $ "xxx"


           -- print "--"
           -- print $ insrtt (B 3 9) [B 1 1, Sp 1 , B 2 2, Sp 6]
           -- print $ insrtt (B 3 9 ) $ insrtt (B 3 9) [B 1 1, Sp 1 , B 2 2, Sp 6]
           -- print "-----"
           -- print $ [B 1 1, Sp 1 , B 1 2, Sp 6, B 3 9]
           -- print $ insrtAll [B 1 1, Sp 1 , B 1 2, Sp 6, B 3 9]
           -- print $ insrtAll [B 1 1, Sp 1 , B 1 2, Sp 6, B 3 9]
           -- let ps = fromRight [] $ parse (many line) "" input
           return (0,0)
