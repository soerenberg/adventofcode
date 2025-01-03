module Main
  ( main
  , tests
  ) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Tests.Graph
import qualified Tests.Grid
import qualified Tests.Parser


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "adventofcode tests"
  [ testGroup "Graph" Tests.Graph.tests
  , testGroup "Grid" Tests.Grid.tests
  , testGroup "Parser" Tests.Parser.tests
  ]
