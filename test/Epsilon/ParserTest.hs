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

testListValP :: TestTree
testListValP = testGroup "testListValP"
  [ testCase "integer list" $ assertParser "[1,2,3,4]" listValP (ListVal [(InVal 1), (IntVal 2), (IntVal 3)])
  , testCase "string list" $ assertParser "[\"hello\", \"world\"]" listValP (ListVal [(StringVal "hello"), (StringVal "world")])
  , testCase "empty list" $ assertParser "[]" listValP ()
  ]


-- verify as types.hs has different format?
testmapValP :: TestTree
mapValP = testGroup "testmapValP"
  [ testCase "integer values map" $ assertParser "{\"orange\":1, \"apple\":2, \"banana\":3}" mapValP (MapVal[("orange", (IntVal 1)), ("apple", (IntVal 2)), ("banana", (IntVal 3)))]
  , testCase "string values map" $ assertParser "{\"a\":'a', \"b\":'b', \"c\":'c'}" mapValP (MapVal [("a", (charVal 'a')), ("b", (charVal 'b')), ("c", (charVal 'c'))])
  , testCase "empty map" $ assertParser "{}" mapValP ()
  ]

testVarP :: TestTree
testVarP = testGroup "testVarP"\

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
    testCase "binary expression add numbers" $ assertParser "5 + 3" opExp (BinOpExpr Add (Val (IntVal 5)) (Val (IntVal 3)))
  , testCase "binary expression sub numbers" $ assertParser "5 - 3" opExp (BinOpExpr Sub (Val (IntVal 5)) (Val (IntVal 3)))
  , testCase "binary expression add variables" $ assertParser "x + y" opExp (BinOpExpr Add (Var ("x")) (Var ("y")))
  , testCase "binary expression add number and variable" $ assertParser "x + 3" opExp (BinOpExpr Add (Var ("x")) (Val (IntVal 3)))
  , testCase "binary expression greater than" $ assertParser "x > 10" opExp (BinOpExpr Gt (Var "x") (Val (IntVal 10)))
  , testCase "binary expression and operator" $assertParser "true && false" opExp (BinOpExpr And (Val (BoolVal True)) (Val (BoolVal False)))
  , testCase "unary operator and binary operator" $assertParser "!true || false" opExp(BinOpExpr (UnOpExpr (Val (BoolVal True))) (Val (BoolVal False)))
  , testCase "map/list indexing" $assertParser "myList.\"orange\"" opExp (BinOpExpr (Var "myList")(Val (StringVal "orange")))
  ]

testExprP :: TestTree
testExprP = testGroup "testExpP"
  [
    testCase "algebraic exp no brackets" $ assertParser "1+20/3-1" exprP (BinOpExpr Add (Val (IntVal 1)) (BinOpExpr Div (Val (IntVal 20)) (BinOpExpr Sub (Val (IntVal 3)) (Val(IntVal 1)))))
  , testCase "algebraic exp brackets" $ assertParser "(((1+20)/3)-1)" exprP (BinOpExpr Sub (BinOpExpr Div (BinOpExpr Add (Val (IntVal 1)) (Val (IntVal 20))) (Val (IntVal 3))) (Val (IntVal 1)) )
  , testCase "lambda defn 1" $ assertParser "fn square(a) {return a * a}" exprP (Lambda ["a"] (Return (opExp Mul (Var ("a")) (Var ("a")) 1)))
  , testCase "lambda defn 2" $ assertParser "fn sumofsquares(a,b) {return (apply square(a) + apply square(b))}" exprP (Lambda ["a", "b"] (Return (BinOpExpr (Add (Call (Var "square") (Var ("a"))) (Call (Var "square") (Var ("b")))) ) ) )
  , testCase "lambda call 1 variable" $ assertParser "apply square (5)" exprP (Call (Var "square") (Val (IntVal 5)))
  -- what about line numbers when function defn is an argument?
  , testCase "lambda call 2 defn" $ assertParser "apply fn square(a) {return a * a} (\"x\")" exprP (Call (Lambda ["a"] (Return (opExp Mul (Var ("a")) (Var ("a")) 1))) (Var ("x")))
  ]

basicAssignmentStatement = "var x = 3"
basicAssignmentSequence = "var x = 3;\n\
\var y = x;\n\
\x = x + 5"
iteStatementExample = "var x = 3;\n\
\if x > 2\n\
\  then return 1\n\
\  else return false endif"
iteNopStatementExample = "var x = 3;\n\
\if x > 2\n\
\  then return 1\n\
\  else skip endif"
nestedIteStatementExample = "var x = 3;\n\
\if x < 2\n\
\  then return 1\n\
\else if x < 5\n\
\       then return 2\n\   
\       return true\n\
\       endif\n\
\endif"
whileStatementExample = "var x = 4;\n\
\while x > 0\n\
\do x = x - 1\n\
\endwhile"
nestedWhileStatementExample = "var x = 4;\n\
\while x > 0\n\
\do x = x - 1\n\
\endwhile"
functionDefnAndCallExample = "var s = fn square(a) {\n\
\  return a * a}\n\
\var y = apply s(5)"
functionCallWithDefnExample = "var s = apply fn square(a) {return a * a} (5)"

testBasicStatementP :: TestTree
testBasicStatementP = testGroup "testBasicStatementP"
  [
     testCase "single assignment" $ assertParser basicAssignmentStatement statementP (AssignDef "x" (Val (IntVal 3)) 1)
  ,  testCase "basic assignment sequence" $ assertParser basicAssignmentSequence statementP (Sequence [
      (AssignDef "x" (Val (IntVal 3)) 1),
      (AssignDef "y" (Var ("x")) 2),
      (Assign "x" (BinOpExpr Add (Var ("x")) (Val (IntVal 5))) 3)
      ])
  ,  testCase "if then else sequence" $ assertParser iteStatementExample statementP (Sequence [
      (AssignDef "x" (Val (IntVal 3)) 1),
      (IfElse (BinOpExpr Gt (Var "x") (Val (IntVal 2)))
              (Return (Val (IntVal 1)) 3)
              (Return (Val (BoolVal False))
       2)
    ])
  ,  testCase "if then else sequence" $ assertParser iteNopStatementExample statementP (Sequence [
      (AssignDef "x" (Val (IntVal 3)) 1),
      (IfElse (BinOpExpr Gt (Var "x") (Val (IntVal 2)))
              (Return (Val (IntVal 1)) 3)
              (Nop 4)
       2)
    ])
  , testCase "nested if then else sequence" $ assertParser nestedIteStatementExample statementP (Sequence [
      (AssignDef "x" (Val (IntVal 3)) 1),
      (IfElse (BinOpExpr Lt (Var "x") (Val (IntVal 2)))
              (Return (Val (IntVal 1)) 3)
              (IfElse (BinOpExpr Lt (Var "x") (Val (IntVal 5)))    
                      (Return (Val (IntVal 2)) 5) 
                      (Return (Val (BoolVal False)) 6)
              4)
       2)
    ])
  ,  testCase "while sequence" $ assertParser whileStatementExample statementP (Sequence [
      (AssignDef "x" (Val (IntVal 4)) 1),
      (While (BinOpExpr Gt (Var "x") (Val (IntVal 0)))
             (Assign "x" (BinOpExpr Add (Var ("x")) (Val (IntVal 1))) 3)
       2)
    ])
  ,  testCase "Function Defn and Call sequence" $ assertParser functionDefnAndCallExample statementP (Sequence [
      (AssignDef "s" (Lambda ["a"] (Return (opExp Mul (Var ("a")) (Var ("a"))) 2)) 1),
      (AssignDef "y" (Call (Var "s") (Val (IntVal 5))) 3)  --2 or 3? not sure about line number in this case
    ])
  ,  testCase "Function Call with Defn sequence" $ assertParser functionCallWithDefnExample statementP (Sequence [
      (AssignDef "s" (Call 
                        (Lambda ["a"] (Return (opExp Mul (Var ("a")) (Var ("a"))) 1)) 
                        (Val (IntVal 5))
                        1)
    ])
  ]


--parse a file?
--for example:
-- var f = fn fib(n){       --1
--   var first = 0 
--   var second = 1
--   while ((n - 2) >= 0)
--   do 
--     third = first + second
--     first = second
--     second = third
--     n = n - 1
--   endwhile
--   return third
-- }
-- var ans = f(5)     --13

-- (Sequence [
--     (AssignDef "f" (Lambda ["n"] (Sequence[
--         (AssignDef "first" (Val (IntVal 0)) 2),
--         (AssignDef "second" (Val (IntVal 1)) 3),
--         (While 
--           (BinOpExpr Gte (BinOpExpr Sub (Var "n") (Val (IntVal 2))) (Val (IntVal 0)))
--           (Sequence[
--             (Assign "third" (opExp Add (Var ("first")) (Var ("second"))) 6),
--             (Assign "first" (Var ("second"))) 7),
--             (Assign "second" (Var ("first"))) 8),
--             (AssignDef "n" (opExp Sub (Var ("n")) (Val (IntVal 1)))) 9),
--           ])
--           4
--         ),
--         (Return (opExp Mul (Var ("a")) (Var ("a"))) 11) 
--         ]))
--     1),
--     (AssignDef "ans" (Call (Var "f") (Val (IntVal 5))) 13)
--   ]
-- )
