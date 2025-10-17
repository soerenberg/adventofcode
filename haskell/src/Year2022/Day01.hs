{-# LANGUAGE OverloadedStrings #-}
module Year2022.Day01 where

import Data.List (sortBy)
import qualified Data.Text as T
import Data.Text.Read (decimal)


readInt :: T.Text -> Either String Int
readInt t = fst <$> decimal t

sumLines :: T.Text -> Either String Int
sumLines xs = sum <$> mapM readInt (T.lines xs)

readCalories :: T.Text -> Either String [Int]
readCalories t = sequence $ map sumLines splits
  where splits = T.splitOn "\n\n" t

top3Calories :: Either String [Int] -> Either String [Int]
top3Calories x = (take 3) . sortBy (flip compare) <$> x

solve :: String -> Either String (Int, Int)
solve t = do
    top3 <- top3Calories . readCalories . T.pack $ t
    let a = head top3
    let b = sum top3
    return (a,b)
