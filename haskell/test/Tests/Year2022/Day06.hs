{-# LANGUAGE MultilineStrings #-}
module Tests.Year2022.Day06 (tests) where

import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2022.Day06 (solve)


exampleInput :: String
exampleInput = """
"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2022 6

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA0" $ (fst <$> solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb") @?= (Right 7)
    , testCase "exampleA1" $ (fst <$> solve "bvwbjplbgvbhsrlpgdmjqwftvncz") @?= (Right 5)
    , testCase "exampleA2" $ (fst <$> solve "nppdvjthqldpwncqszvftbrmjlhg") @?= (Right 6)
    , testCase "exampleA3" $ (fst <$> solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") @?= (Right 10)
    , testCase "exampleA4" $ (fst <$> solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") @?= (Right 11)
    , testCase "exampleB0" $ (snd <$> solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb") @?= (Right 19)
    , testCase "exampleB1" $ (snd <$> solve "bvwbjplbgvbhsrlpgdmjqwftvncz") @?= (Right 23)
    , testCase "exampleB2" $ (snd <$> solve "nppdvjthqldpwncqszvftbrmjlhg") @?= (Right 23)
    , testCase "exampleB3" $ (snd <$> solve "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") @?= (Right 29)
    , testCase "exampleB4" $ (snd <$> solve "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") @?= (Right 26)
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right 1582)
    , testCase "B" $ (snd <$> solution) @?= (Right 3588)
    ]
  where solution = solve data_input
