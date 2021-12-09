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
  [ testCase "simple variable with 1 character" $ assertParser "a" varP ("a")
  , testCase "simple string with multiple characters" $ assertParser "myvar" varP ("myvar")
  ]

testunOpP :: TestTree
testunOpP = testGroup "testunOpP"
  [ testCase "unary operator not" $ assertParser "!" unOpP (Not)
  ]

testbinOpP :: TestTree
testbinOpP = testGroup "testbinOpP"
  [ testCase "binary operator Add" $ assertParser "+" binOpP (Add)
  ,  testCase "binary operator Sub" $ assertParser "-" binOpP (Sub)
  ,  testCase "binary operator Mul" $ assertParser "*" binOpP (Mul)
  ,  testCase "binary operator Div" $ assertParser "/" binOpP (Div)
  ,  testCase "binary operator Gt"  $ assertParser ">" binOpP (Gt)
  ,  testCase "binary operator Gte" $ assertParser ">=" binOpP (Gte)
  ,  testCase "binary operator Lt"  $ assertParser "<" binOpP (Lt)
  ,  testCase "binary operator Lte" $ assertParser "<=" binOpP (Lte)
  ,  testCase "binary operator And" $ assertParser "&&" binOpP (And)
  ,  testCase "binary operator Or"  $ assertParser "||" binOpP (Or)
  ,  testCase "binary operator Idx" $ assertParser "." binOpP (Idx)
  ]

testopExpP :: TestTree
testopExpP = testGroup "testopExpP"
  [
    testCase "simple binary expression add numbers" $ assertParser "5 + 3" opExp (BinOpExpr Add (Val (IntVal 5)) (Val (IntVal 3)))
  ,  testCase "simple binary expression sub numbers" $ assertParser "5 - 3" opExp (BinOpExpr Sub (Val (IntVal 5)) (Val (IntVal 3)))
  ,  testCase "simple binary expression add variables" $ assertParser "x + y" opExp (BinOpExpr Add (Var ("x")) (Var ("y")))
  ,  testCase "simple binary expression add number and variable" $ assertParser "x + 3" opExp (BinOpExpr Add (Var ("x")) (Val (IntVal 3)))
  ]

basicAssignmentStatement = "var x = 3"
basicAssignmentSequence = "var x = 3;\n\
\var y = x\n"
iteStatementExample = "var x = 3;\n\
\if x > 2\n\
\  then return 1\n\
\  else return false endif"

testBasicStatementP :: TestTree
testBasicStatementP = testGroup "testBasicStatementP"
  [
    testCase "single assignment" $ assertParser basicAssignmentStatement statementP (AssignDef "x" (Val (IntVal 3)) 1)
  ,  testCase "basic assignment sequence" $ assertParser basicAssignmentSequence statementP (Sequence [
      (AssignDef "x" (Val (IntVal 3)) 1),
      (AssignDef "y" (Var ("x")) 2)
      ])
  ,  testCase "if then else sequence" $ assertParser iteStatementExample statementP (Sequence [
      (AssignDef "x" (Val (IntVal 3)) 1),
      (IfElse (BinOpExpr Gt (Var "x") (Val (IntVal 2)))
              (Return (Val (IntVal 1)) 3)
              (Return (Val (BoolVal False)) 4)
       2)
    ])
  ]