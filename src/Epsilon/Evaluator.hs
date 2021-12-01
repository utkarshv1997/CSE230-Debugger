{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Epsilon.Evaluator where

import           Epsilon.Types
import qualified Data.Map as M
import           Control.Monad.Coroutine
import           Control.Monad.Coroutine.SuspensionFunctors
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Identity

data EError = ReferenceError Variable
            | TypeError
            | UnexpectedError
  deriving (Eq, Show)

data WhatHappened = Step
                  | Break
                  | Done
  deriving (Eq, Show)

data DState = MkDState
  {
    what  :: WhatHappened,
    loc :: Metadata,
    state :: EState
  }
  deriving (Eq, Show)

mkDState :: (MonadState EState m) => Statement -> WhatHappened -> m DState
mkDState s w = do
  state <- get
  let m = metadata s
  return $ MkDState w m state

type MonadEpsilon m = (MonadState EState m, MonadError EError m)
type MonadEpsilonC m = Coroutine (Yield DState) m

readVar :: (MonadEpsilon m) => Variable -> m Value
readVar x = do
  state <- get
  readStack x $ stack state

readStack :: (MonadEpsilon m) => Variable -> [Frame] -> m Value
readStack x [] = throwError $ ReferenceError x
readStack x (f:fs) = case M.lookup x (variables f) of
  Just v -> return v
  Nothing -> readStack x fs

writeVar :: (MonadEpsilon m) => Variable -> Value -> m ()
writeVar x v = do
  state <- get
  stack <- writeStack x v $ stack state
  put $ state{ stack = stack }

writeStack :: (MonadEpsilon m) => Variable -> Value -> [Frame] -> m [Frame]
writeStack x _ [] = throwError $ ReferenceError x
writeStack x v (f:fs) = case M.lookup x (variables f) of
  Just _ -> return (f':fs)
    where
      vars = M.insert x v (variables f)
      f' = f{variables = vars}
  Nothing -> do
    fs' <- writeStack x v fs
    return (f:fs')

{-
writeStack :: (MonadEpsilon m) => Variable -> Value -> EState -> m EState
writeStack x _ [] = throwError $ ReferenceError x
writeStack x v state@(MkEState [ptr:ptrs] mem) = case M.lookup x (variables (getFrame ptr state)) of
  Just _ -> return (MkEState [ptr:ptrs] mem')
    where
      let f = (getFrame ptr state) in
        vars = M.insert x v (variables f)
        f' = f{variables = vars}
        mem' = M.insert ptr f' mem
  Nothing -> do
    fs' <- writeStack (MkEState [ptrs] mem)
    return (MkEState [ptr:ptrs] mem)
-}
  
defineVar :: (MonadEpsilon m) => Variable -> Value -> m ()
defineVar x v = do
  state <- get
  stack <- defineStack x v $ stack state
  put $ state{ stack = stack }

defineStack :: (MonadEpsilon m) => Variable -> Value -> [Frame] -> m [Frame]
defineStack _ _ [] = throwError $ UnexpectedError
defineStack x v (f:fs) = return (f':fs)
  where
    vars = M.insert x v (variables f)
    f' = f{variables = vars}

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

metadata :: Statement -> Metadata
metadata (Expr _ m) = m
metadata (Nop m) = m
metadata (AssignDef _ _ m) = m
metadata (Assign _ _ m) = m
metadata (Return _ m) = m
metadata (Sequence _) = error "never"
metadata (IfElse _ _ _ m) = m
metadata (While _ _ m) = m
metadata (Breakpoint _ m) = m

evalS :: (MonadEpsilon m) => Statement -> MonadEpsilonC m ()
evalS (Expr e _) = do
  lift $ evalE e
  return ()
evalS (Nop _) = return ()
evalS (AssignDef x e _) = do
  v <- lift $ evalE e
  lift $ defineVar x v
evalS (Assign x e _) = do
  v <- lift $ evalE e
  lift $ writeVar x v
evalS (Sequence []) = return ()
evalS (Sequence (s:ss)) = do
  dstate <- lift $ mkDState s Step
  yield dstate
  evalS s
  evalS $ Sequence ss
evalS (IfElse e s1 s2 _) = do
  v <- lift $ evalE e
  b <- lift $ typeCheckBool v
  if b then evalS s1 else evalS s2
evalS w@(While e s _) = do
  v <- lift $ evalE e
  b <- lift $ typeCheckBool v
  if b then evalS (Sequence [s, w]) else return ()
evalS (Breakpoint s _) = do
  dstate <- lift $ mkDState s Break
  yield dstate
  evalS s


type Epsilon a = MonadEpsilonC (ExceptT EError (StateT EState Identity)) a

initialState :: EState
initialState = MkEState {
  stack = [
    MkFrame {
      name = "main",
      variables = M.empty
    }
  ] 
}

startEpsilon :: Statement -> (EState, Either (Maybe EError) (DState, Epsilon ()))
startEpsilon program = stepEpsilon (evalS program) initialState

stepEpsilon :: Epsilon () -> EState -> (EState, Either (Maybe EError) (DState, Epsilon ()))
stepEpsilon act s = res
  where
    (exceptRes, estate) = runState (runExceptT $ resume act) s
    res = case exceptRes of
      Left err -> (estate, Left (Just err))
      Right coRes -> 
        case coRes of
          Left (Yield dstate act') -> (estate, Right (dstate, act'))
          Right () -> (estate, Left Nothing) 
    

continueEpsilon :: Epsilon () -> EState -> (EState, Either (Maybe EError) (DState, Epsilon ()))
continueEpsilon act s = contRes
  where
    stepRes@(estate, res) = stepEpsilon act s
    contRes = case res of
      Left _ -> stepRes
      Right ((MkDState Break _ _), _) -> stepRes
      Right (_, act') -> continueEpsilon act' estate

runEpsilon :: Epsilon () -> EState -> (EState, Maybe EError)
runEpsilon act s = runRes
  where
    (estate, res) = continueEpsilon act s
    runRes = case res of
      Left err -> (estate, err)
      Right (_, act') -> runEpsilon act' estate 

prog1 :: Statement
prog1 = Sequence 
  [ AssignDef "n" (Val $ IntVal 5) 1
  , AssignDef "fact" (Val $ IntVal 1) 2
  , AssignDef "i" (Val $ IntVal 1) 3
  , While (BinOpExpr Lte (Var "i") (Var "n")) ( Sequence
    [ Assign "fact" (BinOpExpr Mul (Var "fact") (Var "i")) 5
    , Assign "i" (BinOpExpr Add (Var "i") (Val $ IntVal 1)) 6
    ]
    ) 4
  ]
