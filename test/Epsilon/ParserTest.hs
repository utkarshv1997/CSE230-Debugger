module Epsilon.ParserTest where

import Epsilon.Types
import Epsilon.Parser
import Epsilon.Evaluator

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Parsec
import Text.Parsec.String

-- >>> :set -package tasty
--

-- >>> :set -package tasty-hunit
--

-- >>> :set -package tasty-quickcheck
--

assertParser :: (Eq a, Show a) => String -> Parser a -> a -> Assertion
assertParser input parser expected = assertEqual "" (Right expected) result
  where
    result = runParser parser () "TEST-CASE" input

assertBParser :: (Eq a, Show a) => String -> Parser a -> a -> Bool
assertBParser input parser expected = (Right expected) == result
  where
    result = runParser parser () "TEST-CASE" input

testIntValP :: TestTree
testIntValP = testGroup "intValP" 
  [ testCase "single digit integer" $ assertParser "1" intValP (IntVal 1)
  , testCase "multiple digit integer" $ assertParser "1236" intValP (IntVal 1236)
  , testProperty "parse positive integer literals" $ \n -> (n :: Int) >= 0 ==> assertBParser (show n) intValP (IntVal n)
  ]

testBoolValP :: TestTree
testBoolValP = testGroup "boolValP"
  [ testCase "true" $ assertParser "true" boolValP (BoolVal True)
  , testCase "false" $ assertParser "false" boolValP (BoolVal False)
  ]

testCharValP :: TestTree
testCharValP = testGroup "charValP"
  [ testCase "simple character literal" $ assertParser "'c'" charValP (CharVal 'c')
  , testCase "escaped character" $ assertParser "'\n'" charValP (CharVal '\n')
  , testProperty "parse character literals" $ \c -> assertBParser (['\'', c, '\'']) charValP (CharVal c)
  ]

testStringValP :: TestTree
testStringValP = testGroup "stringValP"
  [ testCase "simple string literal" $ assertParser "\"hello world\"" stringValP (StringVal "hello world")
  , testCase "simple string literal with escape sequences" $ assertParser "\"hello \\\"\n world\"" stringValP (StringVal "hello \"\n world")
  ]
