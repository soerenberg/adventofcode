{-# LANGUAGE MultilineStrings #-}
module Tests.Year2016.Day05 (tests) where

import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2016.Day05 (solve)


exampleInput :: String
exampleInput = """
"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2016 5

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA" $ (fst <$> solve "abc") @?= (Right "18f47a30")
    , testCase "exampleB" $ (snd <$> solution) @?= (Right "na")
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right "1a3099aa")
    , testCase "B" $ (snd <$> solution) @?= (Right "na")
    ]
  where solution = solve data_input
