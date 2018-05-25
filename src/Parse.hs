module Parse where

import Data.Char
import Data.Fix
import Control.Applicative
import Control.Monad

import Expr
import NanoParsec

x = listOf expr

schemeList :: Parser a -> Parser a
schemeList p = parens p <|> braces p

-- parse a scheme expression
expr :: Parser Expr
expr = number <|> 
       bool <|> 
       ifParse <|> 
       letParse <|> 
       letRecParse <|>
       lambda <|> 
       set <|> 
       begin <|> 
       appExp <|> 
       symbol
       --return Void

file :: Parser [Expr]
file = many $ token expr

-- parse a number
number :: Parser Expr
number = Number <$> integer

-- parse a boolean
bool :: Parser Expr
bool = Bool <$> (true <|> false)
  where
    true = string "#t" >>= const (return True)

    false = string "#f" >>= const (return False)

-- parse a symbol
symbol :: Parser Expr
symbol = Symbol <$> symbolString

-- parse a symbol, but just return the string
symbolString :: Parser String
symbolString = some $ satisfy (\c -> not (isSpace c || c `elem` "()[]"))

-- parse an if expression
ifParse :: Parser Expr
ifParse = schemeList $ do
  reserved "if"
  bool <- token expr
  ifTrue <- token expr
  ifFalse <- token expr
  return $ If bool ifTrue ifFalse

-- parse a let binding
letBinding :: Parser (String, Expr)
letBinding = schemeList $ do
  s <- token symbolString
  val <- token expr
  return (s, val)

listOf :: Parser a -> Parser [a]
listOf = schemeList . many . token

-- parse a let expression
letParse :: Parser Expr
letParse = schemeList $ do
  reserved "let"
  bindings <- token $ listOf letBinding
  body <- token expr
  return $ Let bindings body

letRecParse :: Parser Expr
letRecParse = schemeList $ do
  reserved "letrec"

  bindings <- token $ listOf letBinding
  let zeroWidth = '\8203'
  let syms = map fst bindings
  let vals = map snd bindings
  let syms' = map (zeroWidth:) syms

  body<- token expr

  return $ Let (zip syms $ repeat Void) $ Let (zip syms' vals) $ Begin $ zipWith Set syms (map Symbol syms') ++ [body]

{--
Let (zip syms $ repeat Void) $ 
  Let (zip syms' vals) $ 
  Begin $ zipWith Set syms (map Symbol syms') ++ [body]
  where
    zeroWidth = '\8203'
    syms = map fst bindings
    vals = map snd bindings
    syms' = map (zeroWidth:) syms --}

--parse a lambda expr
lambda :: Parser Expr
lambda = schemeList $ do
  reserved "lambda"
  args <- token (listOf symbolString)
  body <- token expr
  return $ Lambda args body

set :: Parser Expr
set = schemeList $ do
  reserved "set!"
  sym <- token symbolString
  val <- token expr
  return $ Set sym val

begin :: Parser Expr
begin = schemeList $ do
  reserved "begin"
  exprs <- many $ token expr
  return $ Begin exprs

-- parse an application
appExp :: Parser Expr
appExp = AppExp <$> listOf expr
