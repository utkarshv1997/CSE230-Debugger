module Epsilon.EvaluatorTest where

import Epsilon.Types
import Epsilon.Evaluator
import Data.Maybe
import Data.Map

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

assertVar :: (EState, Maybe EError) -> Variable -> Value -> Assertion
assertVar (estate, _) x v = assertEqual ("variable " ++ (show x) ++ " has unexpected value " ++ (show v') ++ " instead of " ++ (show v)) v v'
  where
    mainFrame = findWithDefault (error "shouldnt happen") 1 (memory estate)
    v' = findWithDefault (VoidVal) x (variables mainFrame)

assertEvalEpsilon :: Statement -> [(EState, Maybe EError) -> Assertion] -> Assertion
assertEvalEpsilon statement assertions = do
  sequence $ (\assertionFn -> assertionFn result) <$> assertions  
  return ()
  where
    (estate, Right (_, act)) = case startEpsilon statement of
      (estate, Right (dstate, act)) ->  (estate, Right (dstate, act))
      _ -> error "shouldn't happen"
    result = runEpsilon act estate

testEvalAssignDef :: TestTree
testEvalAssignDef = testCase "evaluating definition assignment" $
  assertEvalEpsilon (Sequence [(AssignDef "x" (Val $ IntVal 4) 1)])
  [ \(_, error) -> assertBool "does not error out" $ isNothing error
  , \res -> assertVar res "x" (IntVal 4)
  ]

nested_loops_statement = 
  Sequence [AssignDef "isEven" 
                  (Lambda ["n"] 
                      (IfElse (BinOpExpr And 
                               (BinOpExpr Gte (Var "n") (Val (IntVal 0))) 
                               (BinOpExpr Lte (Var "n") (Val (IntVal 0)))) 
                        (Return (Val (BoolVal True)) 3) 
                        (Return (UnOpExpr Not 
                                      (Call (Var "isEven") 
                                            [BinOpExpr Sub 
                                                      (Var "n") 
                                                      (Val (IntVal 1))])) 4) 2)) 1,
            AssignDef "isZeroEven" 
                  (Call (Var "isEven") 
                        [Val (IntVal 0)]) 7,
            AssignDef "isFiveEven" 
                  (Call (Var "isEven") 
                        [Val (IntVal 5)]) 8,
            AssignDef "isEvenSix" 
                  (Call (Lambda ["n"] 
                            (Sequence [AssignDef "half" 
                                            (BinOpExpr Div 
                                                      (Var "n") 
                                                      (Val (IntVal 2))) 10,
                                      AssignDef "double" 
                                            (BinOpExpr Mul 
                                                      (Val (IntVal 2)) 
                                                      (Var "half")) 11,
                                      Return (BinOpExpr And 
                                                      (BinOpExpr Gte 
                                                                (Var "double") 
                                                                (Var "n")) 
                                                      (BinOpExpr Lte 
                                                                (Var "double") 
                                                                (Var "n"))) 12])) 
                        [Val (IntVal 6)]) 9]

testEvalNestedLoops :: TestTree
testEvalNestedLoops = testGroup "Nested Loops tests"
  [testCase "evaluating nested loops 6" $
  assertEvalEpsilon (nested_loops_statement)
  [ \(_, error) -> assertBool "does not error out" $ isNothing error
  , \res -> assertVar res "isEvenSix" (BoolVal True)
  ],
  testCase "evaluating nested loops 0" $
  assertEvalEpsilon (nested_loops_statement)
  [ \(_, error) -> assertBool "does not error out" $ isNothing error
  , \res -> assertVar res "isZeroEven" (BoolVal True)
  ],
  testCase "evaluating nested loops 5" $
  assertEvalEpsilon (nested_loops_statement)
  [ \(_, error) -> assertBool "does not error out" $ isNothing error
  , \res -> assertVar res "isFiveEven" (BoolVal False)
  ]
  ]

closures_test_statement = 
  Sequence [AssignDef "addNum" 
              (Lambda ["n"] 
                      (Return (Lambda ["m"] 
                              (Return (BinOpExpr Add 
                                                (Var "m") 
                                                (Var "n")) 3)) 2)) 1,
            AssignDef "x" (Call 
                            (Call (Var "addNum") 
                                  [Val (IntVal 2)]) 
                            [Val (IntVal 3)]) 6,
            AssignDef "y" (Call 
                            (Call (Var "addNum") 
                                  [Val (IntVal 5)]) 
                            [Val (IntVal 10)]) 7]

testEvalClosures :: TestTree
testEvalClosures = testGroup "Closures tests"
  [testCase "evaluating closures x" $
  assertEvalEpsilon (closures_test_statement)
  [ \(_, error) -> assertBool "does not error out" $ isNothing error
  , \res -> assertVar res "x" (IntVal 5)
  ],
  testCase "evaluating closures y" $
  assertEvalEpsilon (closures_test_statement)
  [ \(_, error) -> assertBool "does not error out" $ isNothing error
  , \res -> assertVar res "y" (IntVal 15)
  ]
  ]
