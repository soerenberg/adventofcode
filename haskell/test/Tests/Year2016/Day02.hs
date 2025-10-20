{-# LANGUAGE MultilineStrings #-}
module Tests.Year2016.Day02 (tests) where

import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2016.Day02 (solve)


exampleInput :: String
exampleInput = """ULL
RRDDD
LURDL
UUUUD\n"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2016 2

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA" $ (fst <$> solution) @?= (Right "1985")
    , testCase "exampleB" $ (snd <$> solution) @?= (Right "5DB3")
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right "35749")
    , testCase "B" $ (snd <$> solution) @?= (Right "9365C")
    ]
  where solution = solve data_input
