{-# LANGUAGE MultilineStrings #-}
module Tests.Year2022.Day01 (tests) where

import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2022.Day01 (solve)


exampleInput :: String
exampleInput = """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2022 1

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA" $ (fst <$> solution) @?= (Right 24000)
    , testCase "exampleB" $ (snd <$> solution) @?= (Right 45000)
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right 69626)
    , testCase "B" $ (snd <$> solution) @?= (Right 206780)
    ]
  where solution = solve data_input
