module Year2024.Day16 where
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import AdventOfCode

data F = Wall | Free | End deriving (Eq, Show)

data S = S { _pos :: !Z2
           , _dir :: !Z2
           , _score :: !Int
           , _seats :: !(S.Set Z2) } deriving (Eq, Show)
makeLenses ''S

data G = G { _visited :: !(M.Map (Z2,Z2) Int)
           , _best :: !(Maybe Int) } deriving (Eq, Show)
makeLenses ''G

type BacktrackState s l a = StateT l (LogicT (State s)) a

runBacktrackState :: s -> l -> BacktrackState s l a -> ([(a, l)], s)
runBacktrackState globalState localState computation =
  runState (observeAllT (runStateT computation localState)) globalState

run :: Grid F -> BacktrackState G S Int
run g = do p <- use pos
           abortIfNotOpt
           if maybe False (==End) . M.lookup p $ g
           then succeed
           else loop
  where succeed = do sc <- use score
                     lift $ best %= (\m -> Just . maybe sc (min sc) $ m)
                     return sc
        abortIfNotOpt = do sc <- use score
                           bst <- lift $ use best
                           if maybe False (<sc) bst then mzero else return ()
        loop = do
          p <- use pos
          v@(vx,vy) <- use dir
          sc <- use score
          vs <- lift $ use visited
          if maybe False (<sc) $ M.lookup (p,v) vs then mzero else return ()
          lift $ visited %= M.insert (p,v) sc
          (w,d) <- msum . map return $ [(v,1), ((vy,-vx), 1001),
                                        ((-vx,-vy), 2001), ((-vy,vx), 1001)]
          let q = addZ2 p w
          if M.findWithDefault Wall q g == Wall
          then mzero
          else do pos .= q
                  dir .= w
                  score += d
                  seats %= S.insert q
                  run g

charToF :: Char -> F
charToF '#' = Wall
charToF 'E' = End
charToF _ = Free

solve :: IO (Int, Int)
solve = do xs <- lines <$> readFile "data/Year2024/day16.txt"
           let z = maybe (0,0) fst . L.find snd . toGridListWith (=='S') $ xs
           let g = fromLines charToF xs

           let initState = S z (0,1) 0 (S.singleton z)
           let initGState = G M.empty Nothing
           let (s, _) = runBacktrackState initGState initState (run g)

           let partA = minimum . map fst $ s
           let bestPaths = filter ((==partA) . fst) $ s
           let partB = S.size . S.unions . map (_seats . snd) $ bestPaths

           return (partA,partB)
