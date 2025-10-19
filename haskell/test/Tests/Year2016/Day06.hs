{-# LANGUAGE MultilineStrings #-}
module Tests.Year2016.Day06 (tests) where

import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2016.Day06 (solve)


exampleInput :: String
exampleInput = """
"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2016 6

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA" $ (fst <$> solution) @?= (Right 0)
    , testCase "exampleB" $ (snd <$> solution) @?= (Right 0)
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right 0)
    , testCase "B" $ (snd <$> solution) @?= (Right 0)
    ]
  where solution = solve data_input
