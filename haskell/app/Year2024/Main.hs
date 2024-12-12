module Year2024.Main (main) where

import AdventOfCode
import qualified Year2024.Day01 as Day01
import qualified Year2024.Day02 as Day02
import qualified Year2024.Day03 as Day03
import qualified Year2024.Day04 as Day04
import qualified Year2024.Day05 as Day05
import qualified Year2024.Day06 as Day06
import qualified Year2024.Day07 as Day07
import qualified Year2024.Day08 as Day08
import qualified Year2024.Day09 as Day09
import qualified Year2024.Day10 as Day10
import qualified Year2024.Day11 as Day11
import qualified Year2024.Day12 as Day12
-- import qualified Year2024.Day13 as Day13
-- import qualified Year2024.Day14 as Day14
-- import qualified Year2024.Day15 as Day15
-- import qualified Year2024.Day16 as Day16
-- import qualified Year2024.Day17 as Day17
-- import qualified Year2024.Day18 as Day18
-- import qualified Year2024.Day19 as Day19
-- import qualified Year2024.Day20 as Day20
-- import qualified Year2024.Day21 as Day21
-- import qualified Year2024.Day22 as Day22
-- import qualified Year2024.Day23 as Day23
-- import qualified Year2024.Day24 as Day24


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
getSolve "7" = Day07.solve >>= showSolutions
getSolve "8" = Day08.solve >>= showSolutions
getSolve "9" = Day09.solve >>= showSolutions
getSolve "10" = Day10.solve >>= showSolutions
getSolve "11" = Day11.solve >>= showSolutions
getSolve "12" = Day12.solve >>= showSolutions
-- getSolve "13" = Day13.solve >>= showSolutions
-- getSolve "14" = Day14.solve >>= showSolutions
-- getSolve "15" = Day15.solve >>= showSolutions
-- getSolve "16" = Day16.solve >>= showSolutions
-- getSolve "17" = Day17.solve >>= showSolutions
-- getSolve "18" = Day18.solve >>= showSolutions
-- getSolve "19" = Day19.solve >>= showSolutions
-- getSolve "20" = Day20.solve >>= showSolutions
-- getSolve "21" = Day21.solve >>= showSolutions
-- getSolve "22" = Day22.solve >>= showSolutions
-- getSolve "23" = Day23.solve >>= showSolutions
-- getSolve "24" = Day24.solve >>= showSolutions
getSolve s   = putStrLn $ "invalid day " ++ s

main :: IO ()
main = do
  options <- execParser opts
  getSolve $ day options
