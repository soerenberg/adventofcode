module Year2022.Main (main) where

import AdventOfCode
import qualified Year2022.Day01 as Day01
import qualified Year2022.Day02 as Day02
import qualified Year2022.Day03 as Day03


showSolutions :: (Show a, Show b) => (a, b) -> IO ()
showSolutions (partA, partB) = do putStrLn $ "part A: " ++ show partA
                                  putStrLn $ "part B: " ++ show partB

getSolve :: String -> IO ()
getSolve "1" = Day01.solve >>= showSolutions
getSolve "2" = Day02.solve >>= showSolutions
getSolve "3" = Day03.solve >>= showSolutions
getSolve s   = putStrLn $ "invalid day " ++ s

main :: IO ()
main = do
  options <- execParser opts
  getSolve $ day options
