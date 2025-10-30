{-# LANGUAGE MultilineStrings #-}
module Tests.Year2016.Day09 (tests) where

import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2016.Day09 (solve)


data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2016 9

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA0" $ (fst <$> solve "ADVENT") @?= (Right 6)
    , testCase "exampleA1" $ (fst <$> solve "A(1x5)BC") @?= (Right 7)
    , testCase "exampleA2" $ (fst <$> solve "(3x3)XYZ") @?= (Right 9)
    , testCase "exampleA3" $ (fst <$> solve "A(2x2)BCD(2x2)EFG") @?= (Right 11)
    , testCase "exampleA4" $ (fst <$> solve "(6x1)(1x3)A") @?= (Right 6)
    , testCase "exampleA5" $ (fst <$> solve "X(8x2)(3x3)ABCY") @?= (Right 18)
    , testCase "exampleB1" $ (snd <$> solve "(3x3)XYZ") @?= (Right 9)
    , testCase "exampleB1" $ (snd <$> solve "X(8x2)(3x3)ABCY") @?= (Right 20)
    , testCase "exampleB1" $ (snd <$> solve "(27x12)(20x12)(13x14)(7x10)(1x12)A") @?= (Right 241920)
    , testCase "exampleB1" $ (snd <$> solve "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") @?= (Right 445)
    ]

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right 70186)
    , testCase "B" $ (snd <$> solution) @?= (Right 10915059201)
    ]
  where solution = solve data_input
