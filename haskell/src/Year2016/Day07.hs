module Year2016.Day07 (solve) where

import qualified Data.Set as S

import AdventOfCode

isAbba :: (Char, Char, Char, Char) -> Bool
isAbba (a,b,c,d) = a == d && b == c && a /= b

hasAbba :: String -> Bool
hasAbba (a:b:c:d:xs) = isAbba (a,b,c,d) || hasAbba (b:c:d:xs)
hasAbba _ = False

hasTLS :: [String] -> Bool
hasTLS = hasTLS' True False
  where hasTLS' True b (x:xs) = hasTLS' False (b || hasAbba x) xs
        hasTLS' False b (x:xs) = (not . hasAbba $ x) && hasTLS' True b xs
        hasTLS' _ b [] = b

findABAs :: Bool -> String -> [(Char, Char)]
findABAs q (a:b:c:xs) = let r=(findABAs q (b:c:xs))
                            t = if q then (a,b) else (b,a)
                        in if a==c && a/=b then t:r else r
findABAs _ _ = []


hasSSL :: [String] -> Bool
hasSSL = hasSSL' True S.empty S.empty
  where hasSSL' b l r xs =
         case (null $ l `S.intersection` r, xs, b) of
           (False, _, _) -> True
           (True, [], _) -> False
           (True, y:ys, True) -> hasSSL' False (S.union l . g b $ y) r ys
           (True, y:ys, False) -> hasSSL' True l (S.union r . g b $ y) ys
        g b' = S.fromList . findABAs b'

line :: Parser [String]
line = do
  xs <- letters `sepBy1` (char '[' <|> char ']')
  _ <- eolf
  pure xs

solve :: String -> Either ParseError (Int, Int)
solve t = do
    ls <- parse (many line) "" (pack t)
    let n = length . filter hasTLS $ ls
    let m = length . filter hasSSL $ ls
    pure (n, m)
