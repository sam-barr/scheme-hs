module Parse where

import Data.Char
import Data.Fix
import Control.Applicative
import Control.Monad

import Expr
import NanoParsec

expr :: Parser Expr
expr = number <|> bool <|> symbol <|> ifParse

number :: Parser Expr
number = (Fix . Number) <$> integer

bool :: Parser Expr
bool = (Fix . Bool) <$> (true <|> false)
  where
    true :: Parser SBool
    true = do
      string "#t"
      return STrue

    false :: Parser SBool
    false = do
      string "#f"
      return SFalse

symbolStart :: Char -> Bool
symbolStart = isAlpha

symbolMiddle :: Char -> Bool
symbolMiddle c = isAlpha c || isDigit c || elem c "-_"

symbol :: Parser Expr
symbol = (Fix . Symbol) <$> do
  start <- some $ satisfy symbolStart
  rest <- many $ satisfy symbolMiddle
  return (start ++ rest)

ifParse :: Parser Expr
ifParse = Fix <$> (parens $ do
  reserved "if"
  bool <- token expr
  ifTrue <- token expr
  ifFalse <- token expr
  return $ If bool ifTrue ifFalse)
