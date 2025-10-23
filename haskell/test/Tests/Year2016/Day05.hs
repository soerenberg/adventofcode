{-# LANGUAGE MultilineStrings #-}
module Tests.Year2016.Day05 (tests) where

import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import AdventOfCode
import Year2016.Day05 (solve)


data_input :: String
data_input = unsafePerformIO . readFile $ inputFilePath 2016 5

tests :: [TestTree]
tests = [exampleChecks, solutionChecks]

exampleChecks :: TestTree
exampleChecks = testGroup "exampleChecks"
    [ testCase "exampleA" $ (fst <$> solution) @?= (Right "18f47a30")
    , testCase "exampleB" $ (snd <$> solution) @?= (Right "05ace8e3")
    ]
  where solution = solve "abc"

solutionChecks :: TestTree
solutionChecks = testGroup "solutionChecks"
    [ testCase "A" $ (fst <$> solution) @?= (Right "1a3099aa")
    , testCase "B" $ (snd <$> solution) @?= (Right "694190cd")
    ]
  where solution = solve data_input


-- First 22 valid hashes starting with 5 0s
-- results :: [(Int, String)]
-- results = [
--   (4515059, "00000191970e97b86ecd2220e76d86b2"), -- 1
--   (6924074, "00000a1568b97dfc4736c4248df549b3"),
--   (8038154, "00000312234ca27718d52476a44c257c"), -- 3
--   (13432968,"00000064ec7123bedfc9ff00cc4f55f2"), -- 0
--   (13540621,"0000091c9c2cd243304328869af7bab2"),
--   (14095580,"0000096753dd21d352853f1d97e19d01"),
--   (14821988,"00000a220003ca08164ab5fbe0b7c08f"),
--   (16734551,"00000aaa1e7e216d6fb95a53fde7a594"),
--   (17029030,"00000a66c43cd6fa9980223accdf2cde"),
--   (17670493,"00000ac0e22f994640b38d250d1ee9c6"),
--   (17743256,"000002457920bc00c2bd4d769a3da01c"), -- 2
--   (18333805,"000002e49710aff8ed8c7b098a125cb1"),
--   (19112977,"000005074f875107f82b4ffb39a1fbf0"), -- 5
--   (20616595,"0000049d19713e17d7d93e9b1f02c856"), -- 4
--   (21658552,"000006c0b6e2bfeabd18eb400b3aecf7"), -- 6
--   (21926249,"00000667310fdb96834554e59b39ca90"),
--   (26326685,"000007d44ea65d0437b810035fec92f2"), -- 7
--   (27178738,"00000a22b84cb73c2ffe23c8bdc3bc41"),
--   (27369774,"000004b898cd6d83ede4afde86bd1fa7"),
--   (28051048,"000008969379ca9d415cce2c1a2d2755"),
--   (28165494,"00000ae9416a2976d2181ac9db6648a2"),
--   (30023486,"000007b24a940feac61491703fc10cec")
--   ]
