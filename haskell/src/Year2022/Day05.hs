{-# LANGUAGE OverloadedStrings #-}
module Year2022.Day05 (solve) where

import AdventOfCode
import Control.Monad.State.Lazy (State, evalState, get, gets, modify)
import qualified Data.Map as M
import Text.Parsec (digit, many1, optional)


type Line = [[Char]]
type Stack  = [Char]
type Stacks = M.Map Int Stack
type Move = (Int, Int, Int)

parseFile :: Parser (Stacks, [Move])
parseFile = do stacks <- buildStacks <$> many1 stackLine
               _ <- many $ char ' ' <|> eol <|> digit
               moves <- many move
               return (stacks, moves)

move :: Parser Move
move = do num  <- string "move " >> digits
          from <- string " from " >> digits
          to   <- string " to " >> digits <* optional eol
          return (num, from, to)

stackLine :: Parser Line
stackLine = many (cell <|> emptyCell) <* eol
  where cell = char '[' >> letters <* char ']' <* (optional $ char ' ')
        emptyCell = (try $ string "   " >> (optional $ char ' ')) >> return []

buildStacks :: [Line] -> Stacks
buildStacks xs = M.fromList $ zip [1..] (foldr1 (zipWith (++)) xs)

type Index     = Int
data MachineType = T9000 | T9001 deriving Eq

tops :: MachineType -> [Move] -> State Stacks String
tops _ [] = gets $ (map (head . snd)) . M.toAscList
tops b ((n, f, t):xs) = do s <- pop f n
                           push b t s
                           tops b xs

pop :: Index -> Int -> State Stacks Stack
pop i n = do st <- get
             let (popped, rest) = splitAt n $ M.findWithDefault [] i st
             modify $ M.insert i rest
             return popped

push :: MachineType -> Index -> Stack -> State Stacks ()
push t i s = modify $ M.insertWith merge i s
  where merge xs ys = if t == T9001 then xs ++ ys else reverse xs ++ ys

solve :: String -> Either ParseError (String, String)
solve input = do
  (stacks, moves) <- parse parseFile "" (pack input)
  let topCratesA =  evalState (tops T9000 moves) stacks
  let topCratesB = evalState (tops T9001 moves) stacks
  return (topCratesA, topCratesB)
