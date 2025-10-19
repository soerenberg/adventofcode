{-# LANGUAGE MultilineStrings #-}
module Tests.Year2016.Day01 (tests) where

import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2016.Day01 (solve)


exampleInput :: String
exampleInput = """
"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2016 1

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA 0" $ (fst <$> solve "R2, L3") @?= (Right 5)
    , testCase "exampleA 1" $ (fst <$> solve "R2, R2, R2") @?= (Right 2)
    , testCase "exampleA 2" $ (fst <$> solve "R5, L5, R5, R3") @?= (Right 12)
    , testCase "exampleB" $ (snd <$> solution) @?= (Right 0)
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right 226)
    , testCase "B" $ (snd <$> solution) @?= (Right 0)
    ]
  where solution = solve data_input
