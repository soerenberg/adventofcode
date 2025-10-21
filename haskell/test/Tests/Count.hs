module Tests.Count (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Count

xs :: [(Char, Int)]
xs = []

tests :: [TestTree]
tests =
  [ testGroup "aggByKey"
    [ testCase "empty" $ aggByKey xs @?= []
    , testCase "simple" $ aggByKey [('a', 2), ('b', 3), ('a', 4)] @?=
                                   [('a',[4,2]),('b',[3])]
    ]
  , testGroup "groupBy"
    [ testCase "empty" $ groupBy xs (+) @?= []
    , testCase "simple" $ groupBy [('a', 2), ('b', 3), ('a', 4)] (+) @?=
                                  [('a',6),('b',3)]
    ]
  , testGroup "groupByList"
    [ testCase "empty" $ groupByList xs id @?= []
    , testCase "id" $ groupByList [('a', 2), ('b', 3), ('a', 4)] id @?=
                                  [('a',[4,2]),('b',[3])]
    , testCase "sum" $ groupByList [('a', 2), ('b', 3), ('a', 4)] sum @?=
                                   [('a',6),('b',3)]
    ]
  , testGroup "frequencies"
    [ testCase "empty" $ frequencies xs @?= []
    , testCase "singleton" $ frequencies "a" @?= [('a',1)]
    , testCase "simple" $ frequencies "abacba" @?= [('a',3),('b',2),('c',1)]
    , testCase "tie" $ frequencies "abab" @?= [('a',2),('b',2)]
    ]
  , testGroup "mostCommon"
    [ testCase "singleton" $ mostCommon "a" @?= ('a',1)
    , testCase "simple" $ mostCommon "abacba" @?= ('a',3)
    , testCase "tie" $ mostCommon "abab" @?= ('b',2)
    ]
  , testGroup "mostCommon'"
    [ testCase "singleton" $ mostCommon' "a" @?= 'a'
    , testCase "simple" $ mostCommon' "abacba" @?= 'a'
    , testCase "tie" $ mostCommon' "abab" @?= 'b'
    ]
  , testGroup "leastCommon"
    [ testCase "singleton" $ leastCommon "a" @?= ('a',1)
    , testCase "duplicate" $ leastCommon "aa" @?= ('a',2)
    , testCase "simple" $ leastCommon "abacba" @?= ('c',1)
    , testCase "tie" $ leastCommon "abab" @?= ('a',2)
    ]
  , testGroup "leastCommon'"
    [ testCase "singleton" $ leastCommon' "a" @?= 'a'
    , testCase "simple" $ leastCommon' "abacba" @?= 'c'
    , testCase "tie" $ leastCommon' "cababcc" @?= 'a'
    ]
  ]
