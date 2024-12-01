module Year2024.Main (main) where

import AdventOfCode
import qualified Year2024.Day01 as Day01


showSolutions :: (Show a, Show b) => (a, b) -> IO ()
showSolutions (partA, partB) = do putStrLn $ "part A: " ++ show partA
                                  putStrLn $ "part B: " ++ show partB

getSolve :: String -> IO ()
getSolve "1" = Day01.solve >>= showSolutions
getSolve s   = putStrLn $ "invalid day " ++ s

main :: IO ()
main = do
  options <- execParser opts
  getSolve $ day options
