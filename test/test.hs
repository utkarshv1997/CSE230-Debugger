import Test.Tasty

import Epsilon.Parser
import Epsilon.ParserTest
import Epsilon.EvaluatorTest

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testIntValP
  , testBoolValP
  , testCharValP
  , testStringValP
  , testListValP
  , testVarP
  , testunOpP
  , testbinOpP
  , testopExpP
  , testExprP
  , testBasicStatementP
  , testEvalAssignDef
  , testEvalNestedLoops
  , testEvalClosures
  ]
