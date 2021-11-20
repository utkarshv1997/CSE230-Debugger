module Epsilon.Types where

import Data.Map

-------------------------------------------------
--             PRIMITIVE VALUES
-------------------------------------------------

data Value
  = IntVal Int
  | BoolVal Bool
  | CharVal Char
  | StringVal String
  | ListVal [Value]
  | MapVal (Map String Value)
  | Closure [String] Statement
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
  | Call Expression [Expression]
  deriving (Eq, Show)

-------------------------------------------------
--             STATEMENTS
-------------------------------------------------

data Statement
  = Expr Expression
  | Nop
  | Sequence [Statement]
  | IfElse Expression Statement Statement
  | While Expression Statement
  deriving (Eq, Show)
