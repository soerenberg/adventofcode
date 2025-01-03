module Tests.Graph (tests) where

import qualified Data.Map as M
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Graph

adjCities :: M.Map String [(String, Int)]
adjCities = M.fromList [
  ("Frankfurt", [("Mannheim", 85), ("Wurzburg", 217), ("Kassel", 173)]),
  ("Mannheim",  [("Frankfurt", 85), ("Karlsruhe", 80)]),
  ("Karlsruhe", [("Mannheim", 80), ("Augsburg", 250)]),
  ("Augsburg", [("Karlsruhe", 250), ("Munich", 84)]),
  ("Munich", [("Augsburg", 84), ("Nuremberg", 167), ("Kassel", 502)]),
  ("Wurzburg", [("Frankfurt", 217), ("Erfurt", 186), ("Nuremberg", 103)]),
  ("Erfurt", [("Wurzburg", 186)]),
  ("Nuremberg", [("Wurzburg", 103), ("Stuttgart", 183), ("Munich", 167)]),
  ("Stuttgart", [("Nuremberg", 183)]),
  ("Kassel", [("Frankfurt", 173), ("Munich", 502)]) ]

citiesDijkstraResult :: M.Map String (Int, [String])
citiesDijkstraResult = M.fromList [ ("Augsburg",(415,["Karlsruhe"]))
                                  , ("Erfurt",(403,["Wurzburg"]))
                                  , ("Frankfurt",(0,[]))
                                  , ("Karlsruhe",(165,["Mannheim"]))
                                  , ("Kassel",(173,["Frankfurt"]))
                                  , ("Mannheim",(85,["Frankfurt"]))
                                  , ("Munich",(487,["Nuremberg"]))
                                  , ("Nuremberg",(320,["Wurzburg"]))
                                  , ("Stuttgart",(503,["Nuremberg"]))
                                  , ("Wurzburg",(217,["Frankfurt"]))
                                  ]

adjSimple :: M.Map String [(String, Int)]
adjSimple = M.fromList [
  ("a", [("b", 2), ("c", 3)]),
  ("b", [("e", 5)]),
  ("c", [("d", 2)]),
  ("d", [("e", 2)])]

simpleDijkstraResult :: M.Map String (Int, [String])
simpleDijkstraResult = M.fromList [ ("a",(0,[]))
                                  , ("b",(2,["a"]))
                                  , ("c",(3,["a"]))
                                  , ("d",(5,["c"]))
                                  , ("e",(7,["d","b"]))
                                  ]

tests :: [TestTree]
tests =
  [ testGroup "dijkstra"
    [ testCase "cities" $ dijkstra "Frankfurt" (adjacencyFromMap adjCities) @?=
                          citiesDijkstraResult
    , testCase "simple" $ dijkstra "a" (adjacencyFromMap adjSimple) @?=
                          simpleDijkstraResult
    ],
    testGroup "recoverPaths"
    [ testCase "cities" $
        recoverPaths citiesDijkstraResult "Frankfurt" "Munich" @?=
        [["Frankfurt","Wurzburg","Nuremberg","Munich"]]
    , testCase "simple" $
        recoverPaths simpleDijkstraResult "a" "e" @?=
        [["a", "c", "d", "e"], ["a", "b", "e"]]
    ]
  ]
