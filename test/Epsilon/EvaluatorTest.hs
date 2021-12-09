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
