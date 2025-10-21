module Year${YEAR}.Main (main) where

import AdventOfCode
import qualified Year${YEAR}.Day01 as Day01
import qualified Year${YEAR}.Day02 as Day02
import qualified Year${YEAR}.Day03 as Day03
import qualified Year${YEAR}.Day04 as Day04
import qualified Year${YEAR}.Day05 as Day05
import qualified Year${YEAR}.Day06 as Day06
import qualified Year${YEAR}.Day07 as Day07
import qualified Year${YEAR}.Day08 as Day08
import qualified Year${YEAR}.Day09 as Day09
import qualified Year${YEAR}.Day10 as Day10
import qualified Year${YEAR}.Day11 as Day11
import qualified Year${YEAR}.Day12 as Day12
import qualified Year${YEAR}.Day13 as Day13
import qualified Year${YEAR}.Day14 as Day14
import qualified Year${YEAR}.Day15 as Day15
import qualified Year${YEAR}.Day16 as Day16
import qualified Year${YEAR}.Day17 as Day17
import qualified Year${YEAR}.Day18 as Day18
import qualified Year${YEAR}.Day19 as Day19
import qualified Year${YEAR}.Day20 as Day20
import qualified Year${YEAR}.Day21 as Day21
import qualified Year${YEAR}.Day22 as Day22
import qualified Year${YEAR}.Day23 as Day23
import qualified Year${YEAR}.Day24 as Day24
import qualified Year${YEAR}.Day25 as Day25


$(makePrintResultForDay ${YEAR} [1..25])

main :: IO ()
main = do
  options <- execParser opts
  printResultForDay $ day options
