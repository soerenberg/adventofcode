{-# LANGUAGE MultilineStrings #-}
module Tests.Year2022.Day02 (tests) where

import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2022.Day02 (solve)


exampleInput :: String
exampleInput = """A Y
B X
C Z"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2022 2

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA" $ (fst <$> solution) @?= (Right 15)
    , testCase "exampleB" $ (snd <$> solution) @?= (Right 12)
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right 12156)
    , testCase "B" $ (snd <$> solution) @?= (Right 10835)
    ]
  where solution = solve data_input
