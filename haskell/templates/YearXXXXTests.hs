module Tests.Year${YEAR} (tests) where

import qualified Tests.Year${YEAR}.Day01
import qualified Tests.Year${YEAR}.Day02
import qualified Tests.Year${YEAR}.Day03
import qualified Tests.Year${YEAR}.Day04
import qualified Tests.Year${YEAR}.Day05
import qualified Tests.Year${YEAR}.Day06
import qualified Tests.Year${YEAR}.Day07
import qualified Tests.Year${YEAR}.Day08
import qualified Tests.Year${YEAR}.Day09
import qualified Tests.Year${YEAR}.Day10
import qualified Tests.Year${YEAR}.Day11
import qualified Tests.Year${YEAR}.Day12
import qualified Tests.Year${YEAR}.Day13
import qualified Tests.Year${YEAR}.Day14
import qualified Tests.Year${YEAR}.Day15
import qualified Tests.Year${YEAR}.Day16
import qualified Tests.Year${YEAR}.Day17
import qualified Tests.Year${YEAR}.Day18
import qualified Tests.Year${YEAR}.Day19
import qualified Tests.Year${YEAR}.Day20
import qualified Tests.Year${YEAR}.Day21
import qualified Tests.Year${YEAR}.Day22
import qualified Tests.Year${YEAR}.Day23
import qualified Tests.Year${YEAR}.Day24
import qualified Tests.Year${YEAR}.Day25

import Test.Tasty (TestTree, testGroup)


tests :: [TestTree]
tests =
  [ testGroup "day01" Tests.Year${YEAR}.Day01.tests
  , testGroup "day02" Tests.Year${YEAR}.Day02.tests
  , testGroup "day03" Tests.Year${YEAR}.Day03.tests
  , testGroup "day04" Tests.Year${YEAR}.Day04.tests
  , testGroup "day05" Tests.Year${YEAR}.Day05.tests
  , testGroup "day06" Tests.Year${YEAR}.Day06.tests
  , testGroup "day07" Tests.Year${YEAR}.Day07.tests
  , testGroup "day08" Tests.Year${YEAR}.Day08.tests
  , testGroup "day09" Tests.Year${YEAR}.Day09.tests
  , testGroup "day10" Tests.Year${YEAR}.Day10.tests
  , testGroup "day11" Tests.Year${YEAR}.Day11.tests
  , testGroup "day12" Tests.Year${YEAR}.Day12.tests
  , testGroup "day13" Tests.Year${YEAR}.Day13.tests
  , testGroup "day14" Tests.Year${YEAR}.Day14.tests
  , testGroup "day15" Tests.Year${YEAR}.Day15.tests
  , testGroup "day16" Tests.Year${YEAR}.Day16.tests
  , testGroup "day17" Tests.Year${YEAR}.Day17.tests
  , testGroup "day18" Tests.Year${YEAR}.Day18.tests
  , testGroup "day19" Tests.Year${YEAR}.Day19.tests
  , testGroup "day20" Tests.Year${YEAR}.Day20.tests
  , testGroup "day21" Tests.Year${YEAR}.Day21.tests
  , testGroup "day22" Tests.Year${YEAR}.Day22.tests
  , testGroup "day23" Tests.Year${YEAR}.Day23.tests
  , testGroup "day24" Tests.Year${YEAR}.Day24.tests
  , testGroup "day25" Tests.Year${YEAR}.Day25.tests
  ]
