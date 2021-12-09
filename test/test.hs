import Test.Tasty

import Epsilon.Parser
import Epsilon.ParserTest

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
  ]
