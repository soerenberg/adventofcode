module Year2016.Day05 (solve) where

import qualified Data.Map as M

import AdventOfCode


pruneIfValid :: String -> Maybe String
pruneIfValid s = do
  let (l,r) = splitAt 5 s
  guard $ l == "00000"
  pure r

doors :: String -> [String]
doors s = mapMaybe pruneIfValid [h | i <- [0..], let h = strMD5 (s ++ (show i))]

fn :: [String] -> Maybe String
fn xs = fn' xs M.empty
  where fn' ((a:b:_):ys) m
          | M.size m >= 8 = pure $ M.elems m
          | digitToInt a < 8 = fn' ys $ M.insertWith (flip const) (digitToInt a) b m
          | otherwise = fn' ys m
        fn' _ _ = Nothing

solve :: String -> Either String (String, String)
solve t = do
    let xs = doors t
    a <- maybeToRight "fail in a" $ sequence . map listToMaybe . take 8 $ xs
    b <- maybeToRight "fail in b" $ fn xs
    pure (a, b)
