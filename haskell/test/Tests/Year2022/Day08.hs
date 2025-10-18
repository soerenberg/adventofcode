{-# LANGUAGE MultilineStrings #-}
module Tests.Year2022.Day08 (tests) where

import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2022.Day08 (solve)


exampleInput :: String
exampleInput = """30373
25512
65332
33549
35390"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2022 8

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA" $ (fst <$> solution) @?= (Right 21)
    , testCase "exampleB" $ (snd <$> solution) @?= (Right 8)
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right 1647)
    , testCase "B" $ (snd <$> solution) @?= (Right 392080)
    ]
  where solution = solve data_input
