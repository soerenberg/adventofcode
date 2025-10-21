module Count
( aggByKey
, frequencies
, groupBy
, groupByList
, mostCommon
, mostCommon'
)
where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Ord (comparing)

aggByKey :: Ord a => [(a, b)] -> [(a, [b])]
aggByKey = M.toList . M.fromListWith (++) . map (\(k,v) -> (k,[v]))

groupBy :: Ord a => [(a, b)] -> (b -> b -> b) -> [(a, b)]
groupBy xs f = M.toList . M.fromListWith f $ xs

groupByList :: Ord a => [(a, b)] -> ([b] -> c) -> [(a, c)]
groupByList xs f = map (\(k,vs) -> (k, f vs)) . aggByKey $ xs

frequencies :: Ord a => [a] -> [(a, Int)]
frequencies xs = groupBy (map (\x -> (x,1)) xs) (+)

mostCommon :: Ord a => [a] -> (a, Int)
mostCommon = L.maximumBy (comparing snd) . frequencies

mostCommon' :: Ord a => [a] -> a
mostCommon' = fst . mostCommon
