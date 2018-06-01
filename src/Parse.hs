module Parse where

import Data.Char
import Control.Applicative
import Control.Monad

import Expr
import NanoParsec

schemeList :: Parser a -> Parser a
schemeList = between "([" ")]"

-- parse a scheme expression
expr :: Parser Expr
expr = number <|> 
       bool <|> 
       ifParse <|> 
       cond <|>
       letParse <|> 
       letRecParse <|>
       lambda <|> 
       set <|> 
       begin <|> 
       define <|>
       appExp <|> 
       quote <|>
       symbol

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
symbolString = some $ satisfy (\c -> not (isSpace c || c `elem` "()[]`"))

-- parse an if expression
ifParse :: Parser Expr
ifParse = schemeList $ do
  reserved "if"
  bool <- token expr
  ifTrue <- token expr
  ifFalse <- token expr
  return $ If bool ifTrue ifFalse

cond :: Parser Expr
cond = schemeList $ do
  reserved "cond"
  ifs <- many $ token condIfs
  return $ Cond ifs
  where
    condIfs = schemeList $ do
      bool <- token expr
      ifBool <- token expr
      return (bool, ifBool)

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

  return $ Let (zip syms $ repeat (Number 0)) $ Let (zip syms' vals) $ Begin $ zipWith Set syms (map Symbol syms') ++ [body]

--parse a lambda expr
lambda :: Parser Expr
lambda = schemeList $ do
  reserved "lambda"
  args <- token (listOf symbolString)
  body <- token expr
  return $ Lambda args body

-- parse a set statement
set :: Parser Expr
set = schemeList $ do
  reserved "set!"
  sym <- token symbolString
  val <- token expr
  return $ Set sym val

-- parse a begin statement
begin :: Parser Expr
begin = schemeList $ do
  reserved "begin"
  exprs <- many $ token expr
  return $ Begin exprs

-- parse a definition
define :: Parser Expr
define = schemeList $ do
  reserved "define"
  sym <- token symbolString
  val <- token expr
  return $ Define sym val

-- parse an application
appExp :: Parser Expr
appExp = AppExp <$> listOf expr

-- parse a quoted expression
quote :: Parser Expr
quote = do
  char '`'
  quote <- quote'
  return $ Quote quote
  where
    quote' = number <|> bool <|> symbol <|> (AppExp <$> listOf quote')
