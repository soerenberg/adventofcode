module Tests.Year2016 (tests) where

import qualified Tests.Year2016.Day01
import qualified Tests.Year2016.Day02
import qualified Tests.Year2016.Day03
import qualified Tests.Year2016.Day04
import qualified Tests.Year2016.Day05
import qualified Tests.Year2016.Day06
import qualified Tests.Year2016.Day07
import qualified Tests.Year2016.Day08
import qualified Tests.Year2016.Day09
import qualified Tests.Year2016.Day10
import qualified Tests.Year2016.Day11
import qualified Tests.Year2016.Day12
import qualified Tests.Year2016.Day13
import qualified Tests.Year2016.Day14
import qualified Tests.Year2016.Day15
import qualified Tests.Year2016.Day16
import qualified Tests.Year2016.Day17
import qualified Tests.Year2016.Day18
import qualified Tests.Year2016.Day19
import qualified Tests.Year2016.Day20
import qualified Tests.Year2016.Day21
import qualified Tests.Year2016.Day22
import qualified Tests.Year2016.Day23
import qualified Tests.Year2016.Day24
import qualified Tests.Year2016.Day25

import Test.Tasty (TestTree, testGroup)


tests :: [TestTree]
tests =
  [ testGroup "day01" Tests.Year2016.Day01.tests
  , testGroup "day02" Tests.Year2016.Day02.tests
  , testGroup "day03" Tests.Year2016.Day03.tests
  , testGroup "day04" Tests.Year2016.Day04.tests
  , testGroup "day05" Tests.Year2016.Day05.tests
  , testGroup "day06" Tests.Year2016.Day06.tests
  , testGroup "day07" Tests.Year2016.Day07.tests
  , testGroup "day08" Tests.Year2016.Day08.tests
  , testGroup "day09" Tests.Year2016.Day09.tests
  , testGroup "day10" Tests.Year2016.Day10.tests
  , testGroup "day11" Tests.Year2016.Day11.tests
  , testGroup "day12" Tests.Year2016.Day12.tests
  , testGroup "day13" Tests.Year2016.Day13.tests
  , testGroup "day14" Tests.Year2016.Day14.tests
  , testGroup "day15" Tests.Year2016.Day15.tests
  , testGroup "day16" Tests.Year2016.Day16.tests
  , testGroup "day17" Tests.Year2016.Day17.tests
  , testGroup "day18" Tests.Year2016.Day18.tests
  , testGroup "day19" Tests.Year2016.Day19.tests
  , testGroup "day20" Tests.Year2016.Day20.tests
  , testGroup "day21" Tests.Year2016.Day21.tests
  , testGroup "day22" Tests.Year2016.Day22.tests
  , testGroup "day23" Tests.Year2016.Day23.tests
  , testGroup "day24" Tests.Year2016.Day24.tests
  , testGroup "day25" Tests.Year2016.Day25.tests
  ]
