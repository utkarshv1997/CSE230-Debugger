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
                                              applyStmt2 = ((mapStatement f) stmt2) in
                                                f (IfElse e applyStmt1 applyStmt2 m)
mapStatement f (While e stmt m) = let applyStmt = ((mapStatement f) stmt) in 
                                        f (While e applyStmt m)
mapStatement f (Breakpoint stmt m) = let applyStmt = ((mapStatement f) stmt) in 
                                          f (Breakpoint applyStmt m)
mapStatement f s = f s

testFunction :: Statement -> Statement
testFunction s = Breakpoint s (-500)

testStatement = Sequence [(IfElse (Var "x") (Nop 1) (Nop 2) 3), (While (Val (BoolVal False)) (Nop 4) 5), (Breakpoint (Nop 6) 7)]

-- >>> mapStatement testFunction testStatement
-- Breakpoint (Sequence [Breakpoint (IfElse (Var "x") (Breakpoint (Nop 1) (-500)) (Breakpoint (Nop 2) (-500)) 3) (-500),Breakpoint (While (Val (BoolVal False)) (Breakpoint (Nop 4) (-500)) 5) (-500),Breakpoint (Breakpoint (Breakpoint (Nop 6) (-500)) 7) (-500)]) (-500)
--
