module Epsilon.Parser where

import qualified Epsilon.Types as T
import Data.Map
import Text.Parsec
import Text.Parsec.String
 

-- Helper to parse some particular string and return an AST Node
constP :: String -> a -> Parser a
constP s a = do
  string s
  return a

-------------------------------------------------
--        PARSING PRIMITIVE VALUES
-------------------------------------------------

valueP :: Parser T.Value
valueP =  intValP
      <|> boolValP
      <|> charValP
      <|> stringValP
      <|> listValP
      <|> mapValP
--    <|> closureP -- creating closures is the evaluator's job

-- TODO: negative integer literals
intValP :: Parser T.Value
intValP = do
  intStr <- many1 digit
  return $ T.IntVal $ read intStr

boolValP :: Parser T.Value
boolValP = constP "true" (T.BoolVal True) 
        <|> constP "false" (T.BoolVal False)

charValP :: Parser T.Value
charValP =  T.CharVal 
        <$> between (char '\'') (char '\'') anyChar

stringValP :: Parser T.Value
stringValP =  T.StringVal 
          <$> stringLiteral

stringLiteral :: Parser String
stringLiteral = between (char '"') (char '"') (many quotedChar)

quotedChar :: Parser Char
quotedChar =  unescapedChar 
          <|> escapedChar

unescapedChar :: Parser Char
unescapedChar = noneOf ['"', '\\']

escapedChar :: Parser Char
escapedChar = do
  char '\\'
  c <- oneOf ['\\', '"', 'r', 'n']
  return $ case c of
    '\\' -> '\\'
    '"' -> '"'
    'r' -> '\r'
    'n' -> '\n'
    c -> c

-- Parser for the separator between consecutive list/ map items/entries
itemSep :: Parser ()
itemSep = do
  many space 
  char ','
  many space
  return ()

listValP :: Parser T.Value
listValP = T.ListVal 
        <$> (between (char '[') (char ']') listItems)

-- TODO: list items can be any expression
listItems :: Parser [T.Value]
listItems = valueP `sepBy`  
            itemSep

mapValP :: Parser T.Value
mapValP = do
  string "map"
  entries <- between (char '{') (char '}') mapEntries 
  return $ T.MapVal $ fromList entries

mapEntries :: Parser [(String, T.Value)]
mapEntries = mapEntry `sepBy`
             itemSep

-- Parser for the separator between the map entry's key and value
kvSep :: Parser ()
kvSep = do
  many space 
  char ':'
  many space
  return ()

mapEntry :: Parser (String, T.Value)
mapEntry = do
  key <- stringLiteral
  kvSep
  value <- valueP
  return (key, value)

--Closure EState [Variable] Statement 

-------------------------------------------------
--             VARIABLES
-------------------------------------------------

-- The following is a parser for variables, which are one-or-more letters. 
varP :: Parser T.Variable
varP = many1 letter --upper

-------------------------------------------------
--             UNARY OPERATORS
-------------------------------------------------
unOpP :: Parser T.UnOp
unOpP = constP "!" T.Not

-------------------------------------------------
--             BINARY OPERATORS
-------------------------------------------------

binOpP :: Parser T.BinOp
binOpP = try(constP "+" T.Add)
       <|> try(constP "-" T.Sub)
       <|> try(constP "*" T.Mul)
       <|> try(constP "/" T.Div)
       <|> try(constP ">=" T.Gte)
       <|> try(constP ">" T.Gt)
       <|> try(constP "<=" T.Lte)
       <|> try(constP "<" T.Lt)
       <|> try(constP "||" T.Or)
       <|> try(constP "&&" T.And)
       <|> (constP "." T.Idx)


-------------------------------------------------
--             EXPRESSIONS
-------------------------------------------------

-- data Expression
--   = Var Variable
--   | Val Value
--   | BinOpExpr BinOp Expression Expression
--   | UnOpExpr UnOp Expression
--   | Lambda [Variable] Statement 
--   | Call Expression [Expression]
--   deriving (Eq, Show)


varExp :: Parser T.Expression
varExp = do 
          var <- varP
          return (T.Var var)

valExp :: Parser T.Expression
valExp = do
          val <- valueP
          return (T.Val val)


-- TODO: BODMAS ordering instead of right associative evaluation
opExp :: Parser T.Expression
opExp = do
          x <-  try(opbExp) <|> try(callExp) <|> try(valExp) <|> try(varExp) <|> unOpExp
          spaces
          op <- binOpP
          spaces
          y <-  exprP
          return (T.BinOpExpr op x y)

opbExp :: Parser T.Expression
opbExp = do
          string "("
          exp <- try(opExp) <|> unOpExp
          string ")"
          return (exp)

unOpExp :: Parser T.Expression
unOpExp = do 
            op <- unOpP
            spaces
            v <- exprP
            return (T.UnOpExpr op v)

parItems :: Parser [T.Variable]
parItems = varP `sepBy` itemSep     --itemSep parses comma

lambdaExp :: Parser T.Expression
lambdaExp = do 
              string "fn"
              spaces 
              pars <- (between (char '(') (char ')') parItems)
              spaces
              string "{"
              spaces
              body <- statementP
              spaces
              string "}"
              spaces
              return (T.Lambda pars body)

-- >>> parseTest statementP "return (apply square(a) + apply square(b))"
-- Return (BinOpExpr Add (Call (Var "square") [Var "a"]) (Call (Var "square") [Var "b"])) 1
--
-- >>> parseTest exprP "(apply square(a) + apply square(b))"
-- BinOpExpr Add (Call (Var "square") [Var "a"]) (Call (Var "square") [Var "b"])
--
-- >>> parseTest callExp "apply square(a)"
-- Call (Var "square") [Var "a"]
--
-- >>> parseTest opExp "apply square(a) + 1"
-- BinOpExpr Add (Call (Var "square") [Var "a"]) (Val (IntVal 1))
--

argItems :: Parser [T.Expression]
argItems = exprP `sepBy` itemSep

callExp :: Parser T.Expression
callExp = do
            string "apply"
            spaces
            exp <- exprP                    --can be a variable to which lambda exp is assigned or directly a lambda expression
            spaces
            args <- (between (char '(') (char ')') argItems)
            return (T.Call exp args)

exprP :: Parser T.Expression
exprP =     try(lambdaExp)
        <|> try(callExp)
        <|> try(opExp) 
        <|> try(opbExp) 
        <|> try(unOpExp) 
        <|> try(valExp) 
        <|> (varExp)

-------------------------------------------------
--             STATEMENTS
-------------------------------------------------

-- type Metadata = Int -- Only store line number in statement metadata

-- data Statement
--   = Expr Expression 
--   | Nop  
--   | AssignDef Variable Expression 
--   | Assign Variable Expression 
--   | Return Expression 
--   | Sequence [Statement]
--   | IfElse Expression Statement Statement 
--   | While Expression Statement 
--   | Breakpoint Statement 
--   deriving (Eq, Show)


-------------------------------------------------------------------------------
-- | Parsing Statements 
-------------------------------------------------------------------------------

-- Next, use the expression parsers to build a statement parser

-- TODO: wrapper function for linenumber

exprStatement :: Parser T.Statement               
exprStatement = do 
                  spaces
                  pos <- getPosition
                  let line = sourceLine pos
                  exp <- exprP
                  return (T.Expr exp line)

assignDefStatement :: Parser T.Statement                --first time variable declaration
assignDefStatement = do 
                  spaces
                  pos <- getPosition
                  let line = sourceLine pos
                  string "var"
                  spaces
                  var <- varP
                  spaces
                  string "="
                  spaces
                  exp <- exprP
                  return (T.AssignDef var exp line)

assignStatement :: Parser T.Statement
assignStatement = do 
                  spaces
                  pos <- getPosition
                  let line = sourceLine pos
                  var <- varP
                  spaces
                  string "="
                  spaces
                  exp <- exprP
                  return (T.Assign var exp line)

ifStatement :: Parser T.Statement
ifStatement = do
               spaces
               pos <- getPosition
               let line = sourceLine pos
               string "if"
               spaces
               exp <- exprP
               spaces
               string "then"
               ifs <- statementP
               spaces
               string "else"
               els <- statementP
               spaces
               string "endif"
               return (T.IfElse exp ifs els line)

whileStatement :: Parser T.Statement
whileStatement = do
                     spaces
                     pos <- getPosition
                     let line = sourceLine pos
                     string "while"
                     spaces
                     exp <- exprP
                     spaces
                     string "do"
                     spaces
                     s <- statementP
                     spaces
                     string "endwhile"
                     return (T.While exp s line)  

nonSequenceStatement :: Parser T.Statement
nonSequenceStatement = try(assignDefStatement)
                   <|> try(assignStatement) 
                   <|> try(ifStatement) 
                   <|> try(whileStatement) 
                   <|> try(nopStatement) 
                   <|> try(breakpointStatement) 
                   <|> try(returnStatement) 
                   <|> (exprStatement)

sequenceSep :: Parser ()
sequenceSep = do
  char ';'
  spaces
  return ()

sequenceStatement :: Parser T.Statement
sequenceStatement = do 
                     spaces
                     s1 <- nonSequenceStatement
                     string ";"
                     spaces
                     stmts <- nonSequenceStatement `sepBy` sequenceSep
                     return (T.Sequence (s1:stmts))

nopStatement :: Parser T.Statement
nopStatement = do 
                  spaces
                  pos <- getPosition
                  let line = sourceLine pos
                  string "skip"
                  return (T.Nop line)

breakpointStatement :: Parser T.Statement
breakpointStatement = do
                        spaces
                        pos <- getPosition
                        let line = sourceLine pos
                        string "break"
                        s <- statementP 
                        return (T.Breakpoint s line)

returnStatement :: Parser T.Statement
returnStatement = do
                        spaces
                        pos <- getPosition
                        let line = sourceLine pos
                        string "return"
                        spaces
                        e <- exprP 
                        return (T.Return e line)

statementP :: Parser T.Statement
statementP = try(sequenceStatement)  
             <|> try(assignDefStatement)
             <|> try(assignStatement) 
             <|> try(ifStatement) 
             <|> try(whileStatement)      
             <|> try(nopStatement) 
             <|> try(breakpointStatement)
             <|> try(returnStatement)
             <|> (exprStatement)



testParser :: Parser (Int, Int)
testParser = do
  pos1 <- getPosition
  let line1 = sourceLine pos1
  spaces
  pos2 <- getPosition
  let line2 = sourceLine pos2
  return (line1, line2)

spaces1 = "    "
spaces2 = "  \n  "


-- >>> parseTest opExp "(!true) || false"
-- BinOpExpr Or (UnOpExpr Not (Val (BoolVal True))) (Val (BoolVal False))
--
-- >>> parseFromFile statementP "test/fib_test.imp"
-- Right (Sequence [AssignDef "f" (Lambda ["n"] (Sequence [AssignDef "first" (Val (IntVal 0)) 2,AssignDef "second" (Val (IntVal 1)) 3,While (BinOpExpr Gte (BinOpExpr Sub (Var "n") (Val (IntVal 2))) (Val (IntVal 0))) (Sequence [Assign "third" (BinOpExpr Add (Var "first") (Var "second")) 6,Assign "first" (Var "second") 7,Assign "second" (Var "third") 8,Assign "n" (BinOpExpr Sub (Var "n") (Val (IntVal 1))) 9]) 4,Return (Var "third") 11])) 1,AssignDef "ans" (Call (Var "f") [Val (IntVal 5)]) 13])
--
