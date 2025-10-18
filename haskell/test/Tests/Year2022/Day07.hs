{-# LANGUAGE MultilineStrings #-}
module Tests.Year2022.Day07 (tests) where

import qualified Data.Map as M
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2022.Day07 (solve)


exampleInput :: String
exampleInput = """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k\n"""

data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2022 7

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA" $ (fst <$> solution) @?= (Right 95437)
    , testCase "exampleB" $ (snd <$> solution) @?= (Right 24933642)
    ]
  where solution = solve exampleInput

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right 1444896)
    , testCase "B" $ (snd <$> solution) @?= (Right 404395)
    ]
  where solution = solve data_input
