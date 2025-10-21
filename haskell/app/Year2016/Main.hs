module Year2016.Main (main) where

import AdventOfCode
import qualified Year2016.Day01 as Day01
import qualified Year2016.Day02 as Day02
import qualified Year2016.Day03 as Day03
import qualified Year2016.Day04 as Day04
import qualified Year2016.Day05 as Day05
import qualified Year2016.Day06 as Day06
import qualified Year2016.Day07 as Day07
import qualified Year2016.Day08 as Day08
import qualified Year2016.Day09 as Day09
import qualified Year2016.Day10 as Day10
import qualified Year2016.Day11 as Day11
import qualified Year2016.Day12 as Day12
import qualified Year2016.Day13 as Day13
import qualified Year2016.Day14 as Day14
import qualified Year2016.Day15 as Day15
import qualified Year2016.Day16 as Day16
import qualified Year2016.Day17 as Day17
import qualified Year2016.Day18 as Day18
import qualified Year2016.Day19 as Day19
import qualified Year2016.Day20 as Day20
import qualified Year2016.Day21 as Day21
import qualified Year2016.Day22 as Day22
import qualified Year2016.Day23 as Day23
import qualified Year2016.Day24 as Day24
import qualified Year2016.Day25 as Day25


$(makePrintResultForDay 2016 [1..25])

main :: IO ()
main = do
  options <- execParser opts
  printResultForDay $ day options
