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
  
testVarP :: TestTree
testVarP = testGroup "testVarP"
  [ testCase "simple variable with 1 character" $ assertParser "a" varP (var x)
  , testCase "simple string with multiple characters" $ assertParser "myvar" varP (var "myvar")
  ]

testunOpP :: TestTree
testunOpP = testGroup "testunOpP"
  [ testCase "unary operator not" $ assertParser "not" unOpP (Not)
  ]

testbinOpP :: TestTree
testbinOpP = testGroup "testbinOpP"
  [ testCase "binary operator Add" $ assertParser "+" binOpP (Add)
    testCase "binary operator Sub" $ assertParser "-" binOpP (Sub)
    testCase "binary operator Mul" $ assertParser "*" binOpP (Mul)
    testCase "binary operator Div" $ assertParser "/" binOpP (Div)
    testCase "binary operator Gt"  $ assertParser ">" binOpP (Gt)
    testCase "binary operator Gte" $ assertParser ">=" binOpP (Gte)
    testCase "binary operator Lt"  $ assertParser "<" binOpP (Lt)
    testCase "binary operator Lte" $ assertParser "<=" binOpP (Lte)
    testCase "binary operator And" $ assertParser "&&" binOpP (And)
    testCase "binary operator Or"  $ assertParser "||" binOpP (Or)
    testCase "binary operator Idx" $ assertParser "." binOpP (Idx)
  ]

testopExp :: TestTree
testopExp = testGroup "testopExp"
  [
    testCase "simple binary expression add numbers" $ assertParser "5 + 3" BinOpExpr (IntVal 5) Add (IntVal 3)
    testCase "simple binary expression sub numbers" $ assertParser "5 - 3" BinOpExpr (IntVal 5) Sub (IntVal 3)
    testCase "simple binary expression add variables" $ assertParser "x + y" BinOpExpr (var x) Add (var y)
    testCase "simple binary expression add number and variable" $ assertParser "x + 3" BinOpExpr (var x) Sub (IntVal 3)
  ]
