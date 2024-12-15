module Year2024.Day15 where
import qualified Data.List as L
import qualified Data.Map as M
import AdventOfCode

data F = Free | Wall | BoxL | BoxR deriving (Eq, Show)

data S a = S { _robot :: Z2
             , _warehouse :: Grid a
             , _moves :: [Z2]} deriving (Eq, Show)
makeLenses ''S

run :: (Z2 -> Z2 -> Grid F -> [(Z2,F)]) -> State (S F) ()
run f = do isDone <- null <$> use moves
           if isDone
           then return ()
           else do v <- pop
                   p <- use robot
                   g <- use warehouse
                   let w = addZ2 p v
                   let cs = f p v g
                   let ds = update v cs
                   if posIsNoWall w g && cellsUnblocked g ds
                   then do mapM_ (\(z,_) -> warehouse %= M.insert z Free) cs
                           mapM_ (\(z,fb) -> warehouse %= M.insert z fb) ds
                           robot .= w
                   else return ()
                   run f
  where pop = (head <$> use moves) <* (moves %= tail)
        update v' = map (\(q,fb) -> (q `addZ2` v', fb))
        posIsNoWall pos = (/=Wall) . M.findWithDefault Wall pos
        cellsUnblocked g = not . any (==Wall)
                           . map (\(z,_) -> M.findWithDefault Wall z g)

childsA :: Z2 -> Z2 -> Grid F -> [(Z2, F)]
childsA p v g = let w = p `addZ2` v in
                case M.findWithDefault Wall w g of
                  BoxL -> (w,BoxL):(childsA w v g)
                  _ -> []

gps :: S F -> Int
gps = toGPS . M.keys . M.filter (==BoxL) . _warehouse
  where toGPS [] = 0
        toGPS ((x,y):xs) = 100*x + y + toGPS xs

input :: Parser (Z2, Grid F, Z2, Grid F, [Z2])
input = do raw <- manyTill anyChar (try $ eol >> eol)
           is <- statements
           case L.find snd . toGridListWith (=='@') . lines $ raw of
             Nothing -> fail "No start position found."
             (Just ((i,j),_)) -> do
               return ((i,j), gridA raw, (i,2*j), gridB raw, is)
  where charToF '#' = Wall
        charToF 'O' = BoxL
        charToF _ = Free
        gridA = fromLines charToF . lines
        gridB = M.fromList . toGridListWith id . map lineToF . lines
        lineToF [] = []
        lineToF ('.':xs) = Free:Free:(lineToF xs)
        lineToF ('O':xs) = BoxL:BoxR:(lineToF xs)
        lineToF ('@':xs) = Free:Free:(lineToF xs)
        lineToF (_:xs) = Wall:Wall:(lineToF xs)
        statements = many (u <|> l <|> r <|> d)
        u = char '^' >> return (-1,0) <* whitespaceEOL
        l = char '<' >> return (0,-1) <* whitespaceEOL
        r = char '>' >> return (0,1) <* whitespaceEOL
        d = char 'v' >> return (1,0) <* whitespaceEOL

solve :: IO (Int, Int)
solve = do txt <- pack <$> readFile "data/Year2024/day15.txt"
           let parsed = parse input "" txt
           case parsed of
             (Right (p,g,pb,gb,stmts)) -> do
               return (gps . execState (run childsA) $ (S p g stmts),
                       0)
             (Left e) -> do print e
                            return (0,0)
