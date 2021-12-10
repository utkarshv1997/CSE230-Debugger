module Epsilon.Types where

import Data.Map

data Frame = MkFrame
  {
    name :: Variable
  , variables :: Map Variable Value
  , environment :: FramePtr
  }
  deriving (Eq, Show)

type FramePtr = Int

data EState = MkEState
  { stack :: [FramePtr]
  , memory :: Map FramePtr Frame
  }
  deriving (Eq, Show)

-------------------------------------------------
--             PRIMITIVE VALUES
-------------------------------------------------

data Type
  = TInt
  | TBool
  | TChar
  | TString
  | TList
  | TMap
  | TClosure

data Value
  = VoidVal
  | IntVal Int
  | BoolVal Bool
  | CharVal Char
  | StringVal String
  | ListVal [Value]
  | MapVal (Map String Value)
  | Closure FramePtr [Variable] Statement -- TODO: Closure may need a field to capture their lexical environment
  deriving (Eq, Show)

-------------------------------------------------
--             VARIABLES
-------------------------------------------------

type Variable = String

-------------------------------------------------
--             UNARY OPERATORS
-------------------------------------------------

data UnOp
  = Not
  deriving (Eq, Show)

-------------------------------------------------
--             BINARY OPERATORS
-------------------------------------------------

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Lte
  | Gte
  | Lt
  | Gt
  | Idx
  | Or
  | And
  deriving (Eq, Show)

-------------------------------------------------
--             EXPRESSIONS
-------------------------------------------------

data Expression
  = Var Variable
  | Val Value
  | BinOpExpr BinOp Expression Expression
  | UnOpExpr UnOp Expression
  | Lambda [Variable] Statement 
  | Call Expression [Expression]
  deriving (Eq, Show)

-------------------------------------------------
--             STATEMENTS
-------------------------------------------------

type Metadata = Int -- Only store line number in statement metadata

data Statement
  = Expr Expression Metadata
  | Nop Metadata
  | AssignDef Variable Expression Metadata
  | Assign Variable Expression Metadata
  | Return Expression Metadata
  | Sequence [Statement]
  | IfElse Expression Statement Statement Metadata
  | While Expression Statement Metadata
  | Breakpoint Statement Metadata
  deriving (Eq, Show)

mapStatement :: (Statement -> Statement) -> Statement -> Statement
mapStatement f (Sequence stmts) = let applyStmts = (Prelude.map (mapStatement f) stmts) in
  f(Sequence applyStmts)
mapStatement f (IfElse e stmt1 stmt2 m) = let applyStmt1 = ((mapStatement f) stmt1)
                                              applyStmt2 = ((mapStatement f) stmt2)
                                              applye     = ((mapStatementIntoExpr f) e) in
                                                f (IfElse applye applyStmt1 applyStmt2 m)
mapStatement f (While e stmt m) = let applyStmt = ((mapStatement f) stmt)
                                      applye    = ((mapStatementIntoExpr f) e) in 
                                        f (While applye applyStmt m)
mapStatement f (Breakpoint stmt m) = let applyStmt = ((mapStatement f) stmt) in 
                                          f (Breakpoint applyStmt m)
mapStatement f (Expr e m) = let applye = ((mapStatementIntoExpr f) e) in
                                          f (Expr applye m)
mapStatement f (Return e m) = let applye = ((mapStatementIntoExpr f) e) in
                                          f (Return applye m)
mapStatement f s@(Nop _) = f s
mapStatement f (AssignDef v e m) = let applye = ((mapStatementIntoExpr f) e) in
                                              f (AssignDef v applye m)
mapStatement f (Assign v e m) = let applye = ((mapStatementIntoExpr f) e) in
                                              f (Assign v applye m)

mapStatementIntoExpr :: (Statement -> Statement) -> Expression -> Expression
mapStatementIntoExpr f (Lambda vars stmt) = let applyStmt = ((mapStatement f) stmt) in
                                                (Lambda vars applyStmt)
mapStatementIntoExpr f (BinOpExpr b e1 e2) = let applye1 = ((mapStatementIntoExpr f) e1)
                                                 applye2 = ((mapStatementIntoExpr f) e2) in
                                                (BinOpExpr b applye1 applye2)
mapStatementIntoExpr f (UnOpExpr b e) = let applye = ((mapStatementIntoExpr f) e) in
                                                (UnOpExpr b applye)
mapStatementIntoExpr f (Call e es) = let applye = ((mapStatementIntoExpr f) e)
                                         applyes = (Prelude.map (mapStatementIntoExpr f) es) in
                                                (Call applye applyes)
mapStatementIntoExpr _ expr = expr

testFunction :: Statement -> Statement
testFunction s@(Sequence _) = s
testFunction s = Breakpoint s (-500)

testStatement = Sequence [(IfElse (Var "x") (Nop 1) (Nop 2) 3), (While (Val (BoolVal False)) (Nop 4) 5), (Breakpoint (Nop 6) 7)]

fibTestStmt = Sequence [AssignDef "f" (Lambda ["n"] 
                        (Sequence [AssignDef "first" (Val (IntVal 0)) 2,
                                   AssignDef "second" (Val (IntVal 1)) 3,
                                   While (BinOpExpr Gte (BinOpExpr Sub (Var "n") (Val (IntVal 2))) (Val (IntVal 0))) 
                                         (Sequence [Assign "third" (BinOpExpr Add (Var "first") (Var "second")) 6,
                                                    Assign "first" (Var "second") 7,
                                                    Assign "second" (Var "third") 8,
                                                    Assign "n" (BinOpExpr Sub (Var "n") (Val (IntVal 1))) 9]) 4,
                                                    Return (Var "third") 11])) 1,
                        AssignDef "ans" (Call (Var "f") [Val (IntVal 5)]) 13]


-- >>> mapStatementIntoExpr testFunction (Lambda ["n"] (Nop 1))
-- Lambda ["n"] (Breakpoint (Nop 1) (-500))
--
-- >>> mapStatement testFunction (AssignDef "f" (Lambda ["n"] (Nop 1)) 1)
-- Breakpoint (AssignDef "f" (Lambda ["n"] (Breakpoint (Nop 1) (-500))) 1) (-500)
--
-- >>> mapStatement testFunction fibTestStmt
-- Sequence [Breakpoint (AssignDef "f" (Lambda ["n"] (Sequence [Breakpoint (AssignDef "first" (Val (IntVal 0)) 2) (-500),Breakpoint (AssignDef "second" (Val (IntVal 1)) 3) (-500),Breakpoint (While (BinOpExpr Gte (BinOpExpr Sub (Var "n") (Val (IntVal 2))) (Val (IntVal 0))) (Sequence [Breakpoint (Assign "third" (BinOpExpr Add (Var "first") (Var "second")) 6) (-500),Breakpoint (Assign "first" (Var "second") 7) (-500),Breakpoint (Assign "second" (Var "third") 8) (-500),Breakpoint (Assign "n" (BinOpExpr Sub (Var "n") (Val (IntVal 1))) 9) (-500)]) 4) (-500),Breakpoint (Return (Var "third") 11) (-500)])) 1) (-500),Breakpoint (AssignDef "ans" (Call (Var "f") [Val (IntVal 5)]) 13) (-500)]
--
