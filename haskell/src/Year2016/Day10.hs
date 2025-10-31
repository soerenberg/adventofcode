module Year2016.Day10 (solve) where

import qualified Data.Map as M
import qualified Data.Set as S

import AdventOfCode

data E = Bot Int | Output Int deriving (Eq, Show, Ord)
data T = Gives E E E | ValueTo Int E deriving (Eq, Show, Ord)

line :: Parser T
line = (valueTo <|> gives) <* eolf
  where
    valueTo = do _ <- string "value "
                 n <- digits <* string " goes to "
                 a <- e
                 pure $ ValueTo n a
    gives = do a <- bot
               _ <- string " gives low to "
               b <- e
               _ <- string " and high to "
               c <- e
               pure $ Gives a b c
    e = (Output <$> (string "output " >> digits)) <|> bot
    bot = Bot <$> (string "bot " >> digits)

makeInit :: [T] -> (M.Map Int (S.Set Int), M.Map Int (E, E))
makeInit zs = makeInit' M.empty M.empty zs
  where
   makeInit' m s [] = (m, s)
   makeInit' m s ((ValueTo v (Bot b)):xs) = makeInit' (M.insertWith S.union b (S.singleton v) m) s xs
   makeInit' m s ((ValueTo _ (Output _)):xs) = makeInit' m s xs
   makeInit' m s ((Gives (Output _) _ _):xs) = makeInit' m s xs
   makeInit' m s ((Gives (Bot b) l h):xs) = makeInit' m (M.insert b (l,h) s) xs

safeMapQuery :: (a -> Bool) -> (M.Map k a) -> Maybe (k, a)
safeMapQuery cond m = listToMaybe . M.assocs . M.filter cond $ m

iter :: (S.Set Int -> Bool) -> M.Map Int (S.Set Int) -> M.Map Int (E, E) -> Maybe Int -> M.Map Int Int -> Maybe (Int, M.Map Int Int)
iter p bots instr predBot out =
  case safeMapQuery ((==2) . S.size) bots of
    Nothing -> (, out) <$> predBot
    (Just (b, s)) -> do let pb = predBot `mplus` (if p s then Just b else Nothing)
                        i <- M.lookup b instr
                        l <- S.lookupMin s
                        h <- S.lookupMax s
                        let state' = M.insert b S.empty bots
                        case i of
                          (Bot b', Output o) -> iter p (foo b' l state') instr pb (M.insert o h out)
                          (Output o, Bot b') -> iter p (foo b' h state') instr pb (M.insert o l out)
                          (Bot b', Bot b'') ->  iter p (foo b' l . foo b'' h $ state') instr pb out
                          (Output o, Output o') -> iter p state' instr pb (M.insert o l . M.insert o' h $ out)
  where foo k v mp = M.insertWith S.union k (S.singleton v) mp

solve :: String -> Either String (Int, Int)
solve t = do
    xs <- mapLeft show $ parse (many line) "" (pack t)
    let (initState, instr) = makeInit xs
    (a, m) <- maybeToRight "fail: no solution" $ iter (==(S.fromList [17,61])) initState instr Nothing M.empty
    pure (a,0)
