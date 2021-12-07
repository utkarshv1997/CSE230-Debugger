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
            | FrameReferenceError
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

firstFramePtr :: (MonadError EError m) => [FramePtr] -> m FramePtr
firstFramePtr [] = throwError FrameReferenceError
firstFramePtr (ptr:_) = return ptr

nextFramePtr :: (MonadState EState m) => m FramePtr
nextFramePtr = do
  (MkEState _ memory) <- get
  return $ ((maximum (M.keys memory))  + 1)


readVar :: (MonadEpsilon m) => Variable -> m Value
readVar x = do
  (MkEState stack memory) <- get
  ptr <- firstFramePtr stack
  readStack x ptr memory

readStack :: (MonadEpsilon m) => Variable -> FramePtr -> M.Map FramePtr Frame -> m Value
readStack x ptr mem = do
  frame <- getFrame ptr mem
  case M.lookup x (variables frame) of
    Just v -> return v
    Nothing -> readStack x (environment frame) mem


writeVar :: (MonadEpsilon m) => Variable -> Value -> m ()
writeVar x v = do
  state@(MkEState stack memory) <- get
  ptr <- firstFramePtr stack
  memory' <- writeStack x v ptr memory
  put $ state{ memory = memory' }

getFrame :: (MonadEpsilon m) => FramePtr -> M.Map FramePtr Frame -> m Frame
getFrame ptr mem = case M.lookup ptr mem of
  Just frame -> return frame
  Nothing -> throwError UnexpectedError

writeStack :: (MonadEpsilon m) => Variable -> Value -> FramePtr -> M.Map FramePtr Frame -> m (M.Map FramePtr Frame) 
writeStack x v ptr mem = do
  frame@(MkFrame _ _ env) <- getFrame ptr mem
  case M.lookup x (variables frame) of
    Just _ -> do
      let vars = M.insert x v (variables frame)
      let frame' = frame{variables = vars}
      let mem' = M.insert ptr frame' mem
      return mem'
    Nothing -> do
      writeStack x v env mem
  
defineVar :: (MonadEpsilon m) => Variable -> Value -> m ()
defineVar x v = do
  state@(MkEState stack memory) <- get
  ptr <- firstFramePtr stack
  memory' <- defineStack x v ptr memory
  put $ state{ memory = memory' }

defineStack :: (MonadEpsilon m) => Variable -> Value -> FramePtr -> M.Map FramePtr Frame -> m (M.Map FramePtr Frame)
defineStack x v ptr mem = do
  frame <- getFrame ptr mem
  let vars = M.insert x v (variables frame)
  let frame' = frame{variables = vars}
  let mem' = M.insert ptr frame' mem
  return mem'

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

typeCheckClosure :: (MonadError EError m) => Value -> m (FramePtr, [Variable], Statement)
typeCheckClosure (Closure ptr vars body) = return (ptr, vars, body)
typeCheckClosure _ = throwError TypeError

getClosureName :: Expression -> Variable
getClosureName (Var v) = v
getClosureName _ = "<anonymous>"

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


evalE :: (MonadEpsilon m) => Expression -> MonadEpsilonC m Value
evalE (Var x) = lift $ readVar x
evalE (Val v) = return v
evalE (BinOpExpr bop e1 e2) = do
  v1 <- evalE e1
  v2 <- evalE e2
  lift $ evalBinOp bop v1 v2
evalE (UnOpExpr unop e) = do
  v <- evalE e
  lift $ evalUnOp unop v
evalE (Lambda args body) = do
  (MkEState stack _) <- lift $ get
  ptr <- lift $ firstFramePtr stack
  return $ (Closure ptr args body)
evalE (Call e args) = do
  let n = getClosureName e
  v <- evalE e
  c <- lift $ typeCheckClosure v
  args' <- sequence (evalE <$> args)
  evalClosure n c args'
  
evalClosure :: (MonadEpsilon m) => Variable -> (FramePtr, [Variable], Statement) -> [Value] -> MonadEpsilonC m Value
evalClosure name (ptr, params, body) args = do
  nextPtr <- lift $ nextFramePtr
  let variables = M.fromList $ zip params args
  let newFrame = MkFrame {
    name = name,
    variables = variables, 
    environment = ptr
  }
  (MkEState stack memory) <- lift $ get
  let memory' = M.insert nextPtr newFrame memory
  let stack' = (nextPtr:stack)
  lift $ put (MkEState stack' memory')
  v <- evalS body
  state <- lift $ get
  lift $ put $ state{stack = stack}
  return v
  
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

evalS :: (MonadEpsilon m) => Statement -> MonadEpsilonC m Value
evalS (Expr e _) = evalE e
evalS (Nop _) = return VoidVal
evalS (AssignDef x e _) = do
  v <- evalE e
  lift $ defineVar x v
  return VoidVal
evalS (Assign x e _) = do
  v <- evalE e
  lift $ writeVar x v
  return VoidVal
evalS (Sequence []) = return VoidVal
evalS (Sequence (s:ss)) = do
  dstate <- lift $ mkDState s Step
  yield dstate
  v <- evalS s
  if ss == [] then return v else evalS $ Sequence ss
evalS (IfElse e s1 s2 _) = do
  v <- evalE e
  b <- lift $ typeCheckBool v
  if b then evalS s1 else evalS s2
evalS w@(While e s _) = do
  v <- evalE e
  b <- lift $ typeCheckBool v
  if b then evalS (Sequence [s, w]) else return VoidVal
evalS (Breakpoint s _) = do
  dstate <- lift $ mkDState s Break
  yield dstate
  evalS s


type Epsilon a = MonadEpsilonC (ExceptT EError (StateT EState Identity)) a

initialState :: EState
initialState = MkEState {
  stack = [ 1 ],
  memory = (M.singleton 1 
    (MkFrame {
      name = "main",
      variables = M.empty,
      environment = -1
    }))
}

startEpsilon :: Statement -> (EState, Either (Maybe EError) (DState, Epsilon Value))
startEpsilon program = stepEpsilon (evalS program) initialState

stepEpsilon :: Epsilon Value -> EState -> (EState, Either (Maybe EError) (DState, Epsilon Value))
stepEpsilon act s = res
  where
    (exceptRes, estate) = runState (runExceptT $ resume act) s
    res = case exceptRes of
      Left err -> (estate, Left (Just err))
      Right coRes -> 
        case coRes of
          Left (Yield dstate act') -> (estate, Right (dstate, act'))
          Right _ -> (estate, Left Nothing) 
    

continueEpsilon :: Epsilon Value -> EState -> (EState, Either (Maybe EError) (DState, Epsilon Value))
continueEpsilon act s = contRes
  where
    stepRes@(estate, res) = stepEpsilon act s
    contRes = case res of
      Left _ -> stepRes
      Right ((MkDState Break _ _), _) -> stepRes
      Right (_, act') -> continueEpsilon act' estate

runEpsilon :: Epsilon Value -> EState -> (EState, Maybe EError)
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

prog2 :: Statement
prog2 = Sequence
  [ AssignDef "factorial" (Lambda ["n"] $ Sequence 
    [ AssignDef "fact" (Val $ IntVal 1) 2
    , AssignDef "i" (Val $ IntVal 1) 3
    , While (BinOpExpr Lte (Var "i") (Var "n")) ( Sequence
      [ Assign "fact" (BinOpExpr Mul (Var "fact") (Var "i")) 5
      , Assign "i" (BinOpExpr Add (Var "i") (Val $ IntVal 1)) 6
      ]) 4
    , Expr (Var "fact") 5
    ]) 1
  , AssignDef "f5" (Call (Var "factorial") [(Val $ IntVal 5)]) 6
  , AssignDef "f6" (Call (Var "factorial") [(Val $ IntVal 6)]) 7
  ]
