module Main
  ( main
  , tests
  ) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Tasty.HUnit (testCase, (@?=))

import qualified Tests.Graph
import qualified Tests.Grid
import qualified Tests.Parser
import qualified Tests.Year2022


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "adventofcode tests"
  [ testGroup "Graph" Tests.Graph.tests
  , testGroup "Grid" Tests.Grid.tests
  , testGroup "Parser" Tests.Parser.tests
  , testGroup "AoC"
      [ testGroup "year2022" Tests.Year2022.tests
      ]
  ]
