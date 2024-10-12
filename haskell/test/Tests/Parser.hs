{-# LANGUAGE OverloadedStrings #-}
module Tests.Parser (tests) where

import Data.Either      (isLeft)
import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Parser

tests :: [TestTree]
tests =
  [ testGroup "Parser"
    [ testCase "eol" $ parse eol "" "\n" @?= (Right '\n')
    , testCase "whitespace ''" $ parse whitespace "" "" @?= (Right "")
    , testCase "whitespace" $ parse whitespace "" " \t " @?= (Right " \t ")
    , testCase "whitespace1" $ parse whitespace1 "" " \t a" @?= (Right " \t ")
    , testCase "whitespace1 f" $ assertBool "parser did not fail" $
        isLeft $ parse whitespace1 "" "a "
    , testCase "letters" $ parse letters "" "abc3" @?= (Right "abc")
    , testCase "letters f" $ assertBool "parser did not fail" $
        isLeft $ parse letters "" "1a "
    , testCase "nonDigits ''" $ parse nonDigits "" "" @?= (Right "")
    , testCase "nonDigits" $ parse nonDigits "" "abc-+" @?= (Right "abc-+")
    , testCase "nonDigits1" $ parse nonDigits1 "" "abc-+" @?= (Right "abc-+")
    , testCase "nonSignedDigits ''" $
        parse nonSignedDigits "" "" @?= (Right "")
    , testCase "nonSignedDigits" $
        parse nonSignedDigits "" "abc-45" @?= (Right "abc")
    , testCase "nonSignedDigits1" $
        parse nonSignedDigits "" "abc+45" @?= (Right "abc")
    , testCase "unsignedInt" $ parse unsignedInt "" "12" @?= (Right 12)
    , testCase "signedInt" $ parse signedInt "" "12" @?= (Right 12)
    , testCase "signedInt +" $ parse signedInt "" "+12" @?= (Right 12)
    , testCase "signedInt -" $ parse signedInt "" "-12" @?= (Right $ -12)
    , testCase "signedFloat" $ parse signedFloat "" "12.34" @?= (Right 12.34)
    , testCase "signedFloat int" $ parse signedFloat "" "12" @?= (Right 12.0)
    , testCase "signedFloat +" $ parse signedFloat "" "+1.2" @?= (Right 1.2)
    , testCase "signedFloat -" $ parse signedFloat "" "-1.2" @?= (Right $ -1.2)
    ]
  ]
