{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Epsilon.Evaluator where

import           Epsilon.Types
import qualified Data.Map as M
import           Control.Monad.Except
import           Control.Monad.State

data EState = ES
  { store :: M.Map Variable Value 
  }
  deriving (Eq, Show)

data EError = ReferenceError Variable
            | TypeError
  deriving (Eq, Show)

type MonadEpsilon m = (MonadState EState m, MonadError EError m)

readVar :: (MonadEpsilon m) => Variable -> m Value
readVar x = do
  ES s <- get
  case M.lookup x s of
    Just v -> return v
    Nothing -> throwError $ ReferenceError x

writeVar :: (MonadState EState m) => Variable -> Value -> m ()
writeVar x v = do
  ES s <- get
  let s' = M.insert x v s
  put $ ES s'

typeCheckInt :: (MonadError EError m) =>  Value -> m Int
typeCheckInt (IntVal i) = return i
typeCheckInt _ = throwError TypeError

typeCheckBool :: (MonadError EError m) =>  Value -> m Bool
typeCheckBool (BoolVal b) = return b
typeCheckBool _ = throwError TypeError

typeCheckChar :: (MonadError EError m) =>  Value -> m Char
typeCheckChar (CharVal c) = return c
typeCheckChar _ = throwError TypeError

typeCheckString :: (MonadError EError m) =>  Value -> m String
typeCheckString (StringVal s) = return s
typeCheckString _ = throwError TypeError

typeCheckList :: (MonadError EError m) =>  Value -> m [Value]
typeCheckList (ListVal l) = return l
typeCheckList _ = throwError TypeError

typeCheckMap :: (MonadError EError m) => Value -> m (M.Map String Value)
typeCheckMap (MapVal m) = return m
typeCheckMap _ = throwError TypeError

evalUnOp :: (MonadEpsilon m) => UnOp -> Value -> m Value
evalUnOp Not v = do
  b <- typeCheckBool v
  return $ BoolVal (not b)

evalBinOp :: (MonadEpsilon m) => BinOp -> Value -> Value -> m Value
evalBinOp Add (IntVal i1) (IntVal i2) = return $ IntVal (i1 + i2)
evalBinOp Sub (IntVal i1) (IntVal i2) = return $ IntVal (i1 - i2)
evalBinOp Mul (IntVal i1) (IntVal i2) = return $ IntVal (i1 * i2)
evalBinOp Div (IntVal i1) (IntVal i2) = return $ IntVal (i1 `div` i2)
evalBinOp Lte (IntVal i1) (IntVal i2) = return $ BoolVal (i1 <= i2)
evalBinOp Gte (IntVal i1) (IntVal i2) = return $ BoolVal (i1 >= i2)
evalBinOp Lt (IntVal i1) (IntVal i2) = return $ BoolVal (i1 < i2)
evalBinOp Gt (IntVal i1) (IntVal i2) = return $ BoolVal (i1 > i2)
evalBinOp Idx (ListVal l) (IntVal i) = return $ l !! i
evalBinOp Or (BoolVal b1) (BoolVal b2) = return $ BoolVal (b1 || b2)
evalBinOp And (BoolVal b1) (BoolVal b2) = return $ BoolVal (b1 && b2)
evalBinOp _ _ _ = throwError TypeError


evalE :: (MonadEpsilon m) => Expression -> m Value
evalE (Var x) = readVar x
evalE (Val v) = return v
evalE (BinOpExpr bop e1 e2) = do
  v1 <- evalE e1
  v2 <- evalE e2
  evalBinOp bop v1 v2
evalE (UnOpExpr unop e) = do
  v <- evalE e
  evalUnOp unop v
