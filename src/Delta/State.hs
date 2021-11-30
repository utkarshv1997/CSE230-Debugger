module Delta.State where

import qualified Data.Map as M
import qualified Epsilon.Types as T

-----------------------
--   NODE POINTERS
-----------------------

type StatementPtr = Int

type ExpressionPtr = Int

data GenericPtr
    = GenericStmt StatementPtr
    | GenericExpr ExpressionPtr
    deriving(Eq, Show, Ord)

-------------------
--     NODES
-------------------

-- As in Types, but using pointers to reference nodes

data ExpressionNode
    = Var T.Variable
    | Val T.Value
    | BinOpExpr T.BinOp ExpressionPtr ExpressionPtr
    | UnOpExpr T.UnOp ExpressionPtr
    | Call ExpressionPtr [ExpressionPtr]
    deriving (Eq, Show)

data StatementNode
    = Expr ExpressionPtr
    | Nop
    | Sequence [StatementPtr]
    | IfElse ExpressionPtr StatementPtr StatementPtr
    | While ExpressionPtr StatementPtr
    deriving (Eq, Show)

data GenericNode
    = GenericNodeStmt StatementNode
    | GenericNodeExpr ExpressionNode
    deriving (Eq, Show)

----------------------
--     TREE MAPS
----------------------

-- Head node has a fixed pointer. Magic numbers. obviously.

expressionHead :: ExpressionPtr
expressionHead = 1 -- I don't think this ever matters, admittedly

statementHead :: StatementPtr
statementHead = 1

data ExpressionMap 
    = ExpMap (M.Map ExpressionPtr ExpressionNode)
    deriving (Eq, Show)

data StatementMap 
    = StmtMap (M.Map StatementPtr StatementNode)
    deriving (Eq, Show)

data VariableMap
    = VarMap (M.Map T.Variable T.Value)
    deriving (Eq, Show)

-- defaults here should never happen, and if they do we should be throwing an error
-- this should work for now, though, hopefully, and avoids handling maybes

getExprNode :: ExpressionPtr -> ExpressionMap -> ExpressionNode
getExprNode ptr (ExpMap map) = M.findWithDefault (Val (T.IntVal 0)) ptr map

getStmtNode :: StatementPtr -> StatementMap -> StatementNode
getStmtNode ptr (StmtMap map) = M.findWithDefault Nop ptr map

getGenericNode :: GenericPtr -> StatementMap -> ExpressionMap -> GenericNode
getGenericNode (GenericStmt ptr) map _ = GenericNodeStmt (getStmtNode ptr map)
getGenericNode (GenericExpr ptr) _ map = GenericNodeExpr (getExprNode ptr map)

-- using a maybe here because this might miss
getVariable :: T.Variable -> VariableMap -> Maybe T.Value
getVariable var (VarMap map) = M.lookup var map

{- 
data ExpressionMap 
    = ExpMap (M.Map ExpressionPtr ExpressionNode)
    deriving (Eq, Show)


data StatementMap 
    = StmtMap (M.Map StatementPtr StatementNode)
    deriving (Eq, Show)

data VariableMap
    = VarMap (M.Map T.Variable T.Value)
    deriving (Eq, Show)
-}

-- for some reason I can't do anything when these aren't commented out
-- that makes things harder and I can't really test anything, but oh well
-- these seem to be working for me now? I'm not sure what's going on with them

----------------------------
--    EVALUATION STACK
----------------------------

-- pointer to the node, list of known values in reverse order
data EvalRequest
    = ToEval GenericPtr [T.Value]
    deriving (Eq, Show)

-- we'll need to be able to have closures which point to prior closures for referencing
-- for now, I'll gleefully ignore that, though

data ProgramState
    = ProgState [EvalRequest] [ExpressionMap] [StatementMap] [VariableMap]
    | Output T.Value
    | Error
    deriving (Eq, Show)

data WhatHappened
    = Statement -- statement returned, so end any single steps
    | Expression -- expression returned, not that relevant
    | PushNode -- put something new on the eval stack
    | Break -- made a NewCall from a break node - note break nodes don't exist as of this comment
    | FuncReturn -- function returned, things popped off the map stacks
    | FuncCall -- called a closure, putting new things on map stacks
    | Termination -- program ended
    deriving (Eq, Show)

data EvalResult
    = Return T.Value [ExpressionMap] [StatementMap] [VariableMap]
    | NewCall EvalRequest [ExpressionMap] [StatementMap] [VariableMap]
    | ThrowError
    deriving (Eq, Show)

data NodeResult -- Simplification of an EvalResult so the node-level functions can pass less around
    = NOutput T.Value
    | NewPtr GenericPtr
    | ClosureMaps ExpressionMap StatementMap VariableMap
    | NodeError
    deriving (Eq, Show)

-- top-level function, takes a program state and runs it one step forward
takeOneStep :: ProgramState -> (ProgramState, WhatHappened)
takeOneStep (ProgState [] _ _ _) = (Error, Termination)
takeOneStep (ProgState _ [] _ _) = (Error, Termination)
takeOneStep (ProgState _ _ [] _) = (Error, Termination)
takeOneStep (ProgState _ _ _ []) = (Error, Termination)
takeOneStep Error                = (Error, Termination) -- errors stay errors
takeOneStep (Output val)         = (Output val, Termination) -- done

takeOneStep (ProgState evalList@(eval:evals) expList@(expMap:expMaps) stmtList@(stmtMap:stmtMaps) vars) = 
    let (result, whatHappened) = (processEvalRequest eval expMap stmtMap vars) 
    in ((filterIn result evalList expList stmtList vars), whatHappened)

-- combines an eval result with an old program state to get a new one
filterIn :: EvalResult -> [EvalRequest] -> [ExpressionMap] -> [StatementMap] -> [VariableMap] -> ProgramState

filterIn ThrowError _ _ _ _ = Error

filterIn (NewCall newEval newExprs newStmts newVars) oldEvals oldExprs oldStmts oldVars = 
    let endEval = (newEval : oldEvals); endExprs = newExprs ++ oldExprs; endStmts = newStmts ++ oldStmts; endVars = newVars ++ oldVars
    in ProgState endEval endExprs endStmts endVars

filterIn (Return returnVal newExprs newStmts newVars) (_:(ToEval evalPtr evalValues):evals) (_:expMaps) (_:stmtMaps) (_:varMaps) = 
    let endEval = ((ToEval evalPtr (returnVal : evalValues)):evals); endExprs = newExprs ++ expMaps; endStmts = newStmts ++ stmtMaps; endVars = newVars ++ varMaps
    in ProgState endEval endExprs endStmts endVars

filterIn (Return returnVal _ _ _) (_:[]) (_:[]) (_:[]) (_:[]) = Output returnVal
-- will need to run one more step to reach Termination
-- could probably fix that, but I'm not sure anyone cares enough

filterIn _ _ _ _ _ = Error

-- unpacks the node from an eval request's pointer, then calls child functions to evaluate it
processEvalRequest :: EvalRequest -> ExpressionMap -> StatementMap -> [VariableMap] -> (EvalResult, WhatHappened)
processEvalRequest (ToEval evalPtr evalVals) exprMap stmtMap varMaps = 
    let nodeToEval = (getGenericNode evalPtr stmtMap exprMap) in -- needs tweaks if evalPtr=statementHead for functions
        (evalNodeAndConvert nodeToEval evalVals exprMap stmtMap varMaps)

-- calls child functions to evaluate a node, then converts the result into an EvalResult (using WhatHappened)
evalNodeAndConvert :: GenericNode -> [T.Value] -> ExpressionMap -> StatementMap -> [VariableMap] -> (EvalResult, WhatHappened)
evalNodeAndConvert node values exprMap stmtMap vars@(varMap:varMaps) =
    let (result, whatHappened) = (evalNodeInContext node values vars) in
        case whatHappened of 
            Statement  -> (let (NOutput val) = result in ((Return val [exprMap] [stmtMap] [varMap]), whatHappened))
            Expression -> (let (NOutput val) = result in ((Return val [exprMap] [stmtMap] [varMap]), whatHappened))
            PushNode   -> (let (NewPtr ptr) = result in ((NewCall (ToEval ptr []) [exprMap] [stmtMap] [varMap]), whatHappened))
            Break      -> (let (NewPtr ptr) = result in ((NewCall (ToEval ptr []) [exprMap] [stmtMap] [varMap]), whatHappened))
            FuncReturn -> (let (NOutput val) = result in ((Return val [] [] []), whatHappened)) -- pop all maps
            FuncCall   -> (let (ClosureMaps newExpr newStmt newVars) = result; ptr = (GenericStmt statementHead) in
                ((NewCall (ToEval ptr []) [newExpr, exprMap] [newStmt, stmtMap] [newVars, varMap]), whatHappened))
            Termination -> (case result of 
                (NOutput val) -> ((Return val [] [] []), whatHappened)
                (NewPtr _)    -> (ThrowError, whatHappened)
                (ClosureMaps _ _ _) -> (ThrowError, whatHappened)
                NodeError     -> (ThrowError, whatHappened))

-- evaluates a node and determines the next step, returning a NodeResult and a WhatHappened
-- many cases, depending on what the node is
evalNodeInContext :: GenericNode -> [T.Value] -> [VariableMap] -> (NodeResult, WhatHappened)
evalNodeInContext node values vars = case node of
    GenericNodeStmt (Expr ptr)            -> case values of 
        []  -> ((NewPtr (GenericExpr ptr)), PushNode) -- if we haven't run the expression, run it
        [x] -> ((NOutput x), Statement) -- if we have run the expression, return the output
        _   -> (NodeError, Termination) -- should never happen
    GenericNodeStmt Nop     -> ((NOutput (T.IntVal (-2000))), Statement) -- Nops have to return something, so return an error code
    GenericNodeStmt (Sequence statements) -> let isFinished = ((length values) == (length statements)) in
        if isFinished then ((NOutput (head values)), Statement) -- if we ran them all, return the last value
        else ((NewPtr (GenericStmt (statements !! (length values)))), PushNode) -- if not, run the next
    GenericNodeStmt (IfElse expPtr stmtPtr1 stmtPtr2) -> case values of
        []                -> ((NewPtr (GenericExpr expPtr)), PushNode) -- if we haven't run the expression, run it
        [T.BoolVal True]  -> ((NewPtr (GenericStmt stmtPtr1)), PushNode) -- if true, run the first statement
        [T.BoolVal False] -> ((NewPtr (GenericStmt stmtPtr2)), PushNode) -- if false, run the second statement
        [_]               -> (NodeError, Termination) -- if expression is neither T nor F, throw an error
        [x, _]            -> ((NOutput x), Statement) -- if we have run a statement, return the output
    GenericNodeStmt (While expPtr stmtPtr) -> let checkingExpr = (even (length values)) in -- if even length, check condition
        if checkingExpr then ((NewPtr (GenericExpr expPtr)), PushNode) -- push the expr onto the stack
        else case values of
            [T.BoolVal False]       -> ((NOutput (T.IntVal (-3000))), Statement) -- condition was immediately false, act as a nop
            [T.BoolVal True]        -> ((NewPtr (GenericStmt stmtPtr)), PushNode) -- condition was true, run the statement - maybe redundant
            [T.BoolVal False, x, _] -> ((NOutput x), Statement) -- condition false after some run, return the output
            [T.BoolVal True, _]     -> ((NewPtr (GenericStmt stmtPtr)), PushNode) -- condition was true after some runs, run the statement
    GenericNodeExpr (Var varName) -> let val = (getVarFromStack varName vars) in 
        case val of
            Just x  -> ((NOutput x), Expression) -- found the variable
            Nothing -> (NodeError, Termination) -- did not find the variable, invalid reference
    GenericNodeExpr (Val x) -> ((NOutput x), Expression) -- just output that value
    GenericNodeExpr (BinOpExpr op ptr1 ptr2) -> case values of
        []       -> ((NewPtr (GenericExpr ptr1)), PushNode) -- if nothing's done, calculate the first
        [x]      -> ((NewPtr (GenericExpr ptr2)), PushNode) -- if one is calculated, calculate the second
        [x2, x1] -> let val = (evalBinOp op x1 x2) in -- compute the operation
            case val of
                Just x  -> ((NOutput x), Expression) -- if it gave output, return that
                Nothing -> (NodeError, Termination) -- if it didn't, error
    GenericNodeExpr node -> (NodeError, Termination) --TODO

getVarFromStack :: T.Variable -> [VariableMap] -> Maybe T.Value
getVarFromStack varName (varMap:varMaps) = let val = (getVariable varName varMap) in
    case val of
        Just x  -> Just x
        Nothing -> getVarFromStack varName varMaps
getVarFromStack varName []       = Nothing

evalBinOp :: T.BinOp -> T.Value -> T.Value -> Maybe T.Value
evalBinOp T.Add (T.IntVal x) (T.IntVal y) = Just (T.IntVal (x + y))
evalBinOp T.Add _ _ = Nothing
evalBinOp T.Sub (T.IntVal x) (T.IntVal y) = Just (T.IntVal (x - y))
evalBinOp T.Sub _ _ = Nothing
evalBinOp T.Mul (T.IntVal x) (T.IntVal y) = Just (T.IntVal (x * y))
evalBinOp T.Mul _ _ = Nothing
evalBinOp T.Div (T.IntVal x) (T.IntVal 0) = Nothing
evalBinOp T.Div (T.IntVal x) (T.IntVal y) = Just (T.IntVal (div x  y))
evalBinOp T.Div _ _ = Nothing
evalBinOp T.Lte (T.IntVal x) (T.IntVal y) = Just (T.BoolVal (x <= y))
evalBinOp T.Lte _ _ = Nothing
evalBinOp T.Gte (T.IntVal x) (T.IntVal y) = Just (T.BoolVal (x >= y))
evalBinOp T.Gte _ _ = Nothing
evalBinOp T.Lt  (T.IntVal x) (T.IntVal y) = Just (T.BoolVal (x < y))
evalBinOp T.Lt  _ _ = Nothing
evalBinOp T.Gt  (T.IntVal x) (T.IntVal y) = Just (T.BoolVal (x > y))
evalBinOp T.Gt  _ _ = Nothing
evalBinOp T.Idx (T.StringVal x) (T.IntVal y) = Just (T.CharVal (x !! y))
evalBinOp T.Idx (T.ListVal x)   (T.IntVal y) = Just (x !! y)
evalBinOp T.Idx (T.MapVal x) (T.StringVal y) = M.lookup y x
evalBinOp T.Idx _ _ = Nothing
evalBinOp T.Or  (T.BoolVal x) (T.BoolVal y) = Just (T.BoolVal (x || y))
evalBinOp T.Or  _ _ = Nothing
evalBinOp T.And (T.BoolVal x) (T.BoolVal y) = Just (T.BoolVal (x && y))
evalBinOp T.And _ _ = Nothing
-- evalBinOp _ _ _   = Nothing