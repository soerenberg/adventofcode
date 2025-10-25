{-# LANGUAGE MultilineStrings #-}
module Tests.Year2016.Day08 (tests) where

import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2016.Day08 (solve)


data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2016 8

tests :: [TestTree]
tests = [solutionChecks]

expectedB = """\n###..#..#.###..#..#..##..####..##..####..###.#....
#..#.#..#.#..#.#..#.#..#.#....#..#.#......#..#....
#..#.#..#.#..#.#..#.#....###..#..#.###....#..#....
###..#..#.###..#..#.#....#....#..#.#......#..#....
#.#..#..#.#.#..#..#.#..#.#....#..#.#......#..#....
#..#..##..#..#..##...##..####..##..####..###.####.\n"""

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right 121)
    , testCase "B" $ (snd <$> solution) @?= (Right expectedB)  -- Remark: RURUCEOEIL
    ]
  where solution = solve data_input
