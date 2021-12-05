mmodule Epsilon.Types where

import Data.Map

data Frame = MkFrame
  {
    name :: Variable
  , variables :: Map Variable Value
  }
  deriving (Eq, Show)

data EState = MkEState
  { stack :: [Frame]
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
  = IntVal Int
  | BoolVal Bool
  | CharVal Char
  | StringVal String
  | ListVal [Value]
  | MapVal (Map String Value)
  | Closure EState [Variable] Statement -- TODO: Closure may need a field to capture their lexical environment
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