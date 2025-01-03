module Graph (
  adjacencyFromMap
, dijkstra
, recoverPaths
) where

import Control.Monad.State
import Control.Monad.State.Lazy
import qualified Data.Map as M
import qualified Data.PSQueue as P
import qualified Data.Set as S
import Data.PSQueue (Binding(..))
import Lens.Micro.Platform ((%=), (.=), makeLenses, use)

data DijkstraState a = DS {
    _pqueue  :: P.PSQ a Int
  , _visited :: S.Set a
  , _dists   :: M.Map a (Int, [a]) } deriving (Eq, Show)
makeLenses ''DijkstraState

dijkstra :: Ord a => a -> (a -> [(a, Int)]) -> M.Map a (Int, [a])
dijkstra s af = evalState (dijkstraLoop af) (initDijkstraState s)

initDijkstraState :: a -> DijkstraState a
initDijkstraState s = DS { _pqueue = P.singleton s 0
                         , _visited = S.singleton s
                         , _dists = M.singleton s (0, []) }

dijkstraLoop :: Ord a => (a -> [(a, Int)])
                      -> State (DijkstraState a) (M.Map a (Int, [a]))
dijkstraLoop af =
  do next <- P.minView <$> use pqueue
     case next of
       Nothing -> use dists
       (Just (u :-> p, qPopped)) ->
         do pqueue .= qPopped
            vs <- use visited
            visited %= S.insert u
            mapM_ (dijkstraStepAt (u,p)) . dropVisited vs . af $ u
            dijkstraLoop af
  where dropVisited vs' = filter (flip S.notMember vs' . fst)

dijkstraStepAt :: Ord a => (a, Int) -> (a, Int) -> State (DijkstraState a) ()
dijkstraStepAt (u,du) (v, cuv) =
  do costs <- M.lookup v <$> use dists
     pqueue %= P.insert v (du + cuv)
     case costs of
       Nothing -> dists %= M.insert v ((du+cuv), [u])
       Just (n,_) ->
         if n == du + cuv
         then dists %= M.insertWith (\(_,xs) (_,ys) -> (n,xs ++ ys)) v (n, [u])
         else if n > du + cuv
              then dists %= M.insert v ((du+cuv), [u])
              else return ()

recoverPaths :: Ord a => M.Map a (Int, [a]) -> a -> a -> [[a]]
recoverPaths m s t
  | s == t = [[s]]
  | otherwise = case M.lookup t m of
                       Nothing -> []
                       (Just (d, xs)) -> do v <- xs
                                            ps <- recoverPaths m s v
                                            return $ ps ++ [t]

adjacencyFromMap :: Ord a => M.Map a [(a,Int)] -> (a -> [(a, Int)])
adjacencyFromMap = flip $ M.findWithDefault []
