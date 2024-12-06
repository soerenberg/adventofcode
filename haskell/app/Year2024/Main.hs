module Year2024.Main (main) where

import AdventOfCode
import qualified Year2024.Day01 as Day01
import qualified Year2024.Day02 as Day02
import qualified Year2024.Day03 as Day03
import qualified Year2024.Day04 as Day04
import qualified Year2024.Day05 as Day05
import qualified Year2024.Day06 as Day06


showSolutions :: (Show a, Show b) => (a, b) -> IO ()
showSolutions (partA, partB) = do putStrLn $ "part A: " ++ show partA
                                  putStrLn $ "part B: " ++ show partB

getSolve :: String -> IO ()
getSolve "1" = Day01.solve >>= showSolutions
getSolve "2" = Day02.solve >>= showSolutions
getSolve "3" = Day03.solve >>= showSolutions
getSolve "4" = Day04.solve >>= showSolutions
getSolve "5" = Day05.solve >>= showSolutions
getSolve "6" = Day06.solve >>= showSolutions
getSolve s   = putStrLn $ "invalid day " ++ s

main :: IO ()
main = do
  options <- execParser opts
  getSolve $ day options
