{-# LANGUAGE OverloadedStrings #-}
module Tests.Grid (tests) where

import Data.Char (toUpper)
import Data.Either      (isLeft)
import qualified Data.Map as M
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Grid

tests :: [TestTree]
tests =
  [
  ]
