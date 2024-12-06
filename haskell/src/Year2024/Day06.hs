{-# LANGUAGE TemplateHaskell #-}
module Year2024.Day06 where
import qualified Data.Map as M
import qualified Data.Set as S

import AdventOfCode


data S = S { _pos :: !Z2
           , _dir :: !Z2
           , _vis :: S.Set Z2
           , _visDirs :: S.Set (Z2, Z2) } deriving (Eq, Show)
makeLenses ''S

walk :: Grid Bool -> State S (Maybe Int)
walk g = do posDir <- (,) <$> use pos <*> use dir
            isLoop <- S.member posDir <$> use visDirs
            if isLoop then return Nothing
            else
              do visDirs %= S.insert posDir
                 let nextPos = uncurry addZ2 posDir
                 case M.lookup nextPos g of
                   Nothing -> Just . S.size <$> use vis
                   (Just False) -> do vis %= S.insert nextPos
                                      pos .= nextPos
                                      walk g
                   (Just True) -> dir %= rot90cw >> walk g

countLoops :: S -> Grid Bool -> [Z2] -> Int
countLoops s g ps = foldr f 0 ps
  where f p n = maybe (1+n) (const n) $ evalState (walk $ M.insert p True g) s

solve :: IO (Maybe Int, Int)
solve = do g <-  fromLines id . lines <$> readFile "data/Year2024/day06.txt"
           let p = M.foldrWithKey (\k c t -> if c=='^' then k else t) (0,0) g
           let gb = M.map (=='#') g

           let s = S p (-1,0) (S.singleton p) S.empty
           let (r,final) = runState (walk gb) s
           return (r, countLoops s gb (S.toList . _vis $ final))
