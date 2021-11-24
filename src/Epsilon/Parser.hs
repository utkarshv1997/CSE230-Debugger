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
--    <|> closureP TODO: After statementP is available

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
