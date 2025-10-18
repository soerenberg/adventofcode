{-# LANGUAGE MultilineStrings #-}
module Tests.Year2022.Day05 (tests) where

import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2022.Day05 (solve)


exampleInput :: String
exampleInput = """    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2022 5

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA" $ (fst <$> solution) @?= (Right "CMZ")
    , testCase "exampleB" $ (snd <$> solution) @?= (Right "MCD")
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right "RNZLFZSJH")
    , testCase "B" $ (snd <$> solution) @?= (Right "CNSFCGJSM")
    ]
  where solution = solve data_input
