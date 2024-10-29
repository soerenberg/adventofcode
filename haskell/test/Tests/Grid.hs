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
  [ testGroup "fromLines"
    [ testCase "empty" $ fromLines id [] @?= M.empty
    , testCase "2x3" $ fromLines id ["abc", "def"] @?= gridA
    , testCase "l3,2" $ fromLines toUpper ["abc", "de"] @?= M.fromList
        [((0,0),'A'), ((0,1),'B'), ((0,2),'C'), ((1,0),'D'), ((1,1),'E')]
    ],
    testGroup "fromLinesToList"
    [ testCase "empty" $ fromLinesToList id [] @?= []
    , testCase "2x3" $ fromLinesToList id ["abc", "def"] @?= listA
    , testCase "l3,2" $ fromLinesToList toUpper ["abc", "de"] @?=
        [((0,0),'A'), ((0,1),'B'), ((0,2),'C'), ((1,0),'D'), ((1,1),'E')]
    ],
    testGroup "fromDims"
    [ testCase "empty" $ fromDims 'a' 0 0 @?= M.empty
    , testCase "1x1" $ fromDims 'a' 1 1 @?= M.fromList [((0,0),'a')]
    , testCase "2x3" $ fromDims 'a' 2 3 @?= M.fromList [((i,j),'a')|i<-[0..1], j<-[0..2]]
    ],
    testGroup "boundingBox"
    [ testCase "2x3" $ boundingBox gridA @?= Just (0, 1, 0, 2)
    , testCase "1x1" $ boundingBox gridB @?= Just (2, 2, 3, 3)
    , testCase "empty" $ boundingBox gridEmpty @?= Nothing
    ],
    testGroup "neighbors8At"
    [ testCase "empty" $ neighbors8At gridEmpty (0, 0) @?= []
    , testCase "2x3" $ neighbors8At gridA (0, 0) @?= "bde"
    , testCase "3x3" $ neighbors8At gridC (1, 1) @?= "ABCDFGHI"
    ],
    testGroup "neighbors4At"
    [ testCase "empty" $ neighbors4At gridEmpty (0, 0) @?= []
    , testCase "2x3" $ neighbors4At gridA (0, 0) @?= "bd"
    , testCase "3x3" $ neighbors4At gridC (1, 1) @?= "BDFH"
    ],
    testGroup "setCoords"
    [ testCase "empty" $ setCoords [] 'x' gridEmpty @?= gridEmpty
    , testCase "2x3" $ setCoords [(0,1), (1,2)] 'x' gridA @?=
        M.fromList [((0,0),'a'), ((0,1),'x'), ((0,2),'c'),
                    ((1,0),'d'), ((1,1),'e'), ((1,2),'x')]
    ]
  ]

listA :: [(Z2, Char)]
listA = [((0,0),'a'), ((0,1),'b'), ((0,2),'c'),
         ((1,0),'d'), ((1,1),'e'), ((1,2),'f')]

gridA :: Grid Char
gridA = M.fromList listA

gridB :: Grid Char
gridB = M.fromList [((2, 3), 'A')]

gridEmpty :: Grid Char
gridEmpty = M.fromList []

listC :: [(Z2, Char)]
listC = [((0,0),'A'), ((0,1),'B'), ((0,2),'C'),
         ((1,0),'D'), ((1,1),'E'), ((1,2),'F'),
         ((2,0),'G'), ((2,1),'H'), ((2,2),'I')]

gridC :: Grid Char
gridC = M.fromList listC
