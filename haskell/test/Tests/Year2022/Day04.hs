{-# LANGUAGE MultilineStrings #-}
module Tests.Year2022.Day04 (tests) where

import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2022.Day04 (solve)


exampleInput :: String
exampleInput = """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8\n"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2022 4

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA" $ (fst <$> solution) @?= (Right 2)
    , testCase "exampleB" $ (snd <$> solution) @?= (Right 4)
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right 536)
    , testCase "B" $ (snd <$> solution) @?= (Right 845)
    ]
  where solution = solve data_input
