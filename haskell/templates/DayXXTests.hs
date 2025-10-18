{-# LANGUAGE MultilineStrings #-}
module Tests.Year${YEAR}.Day${PADDAY} (tests) where

import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year${YEAR}.Day${PADDAY} (solve)


exampleInput :: String
exampleInput = """
"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath ${YEAR} ${DAY}

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
