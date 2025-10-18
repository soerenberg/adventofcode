module Tests.Year2022 (tests) where

import qualified Tests.Year2022.Day01

import Test.Tasty (TestTree, testGroup)


tests :: [TestTree]
tests =
  [ testGroup "day01" Tests.Year2022.Day01.tests
  ]
