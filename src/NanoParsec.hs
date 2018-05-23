{-# LANGUAGE InstanceSigs #-}

module NanoParsec (Parser(..), runParser, item, failure, combine, option, satisfy, oneOf, chainl, chainl1, char, natural, string, token,
                   reserved, spaces, digit, integer, parens) where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser {parse :: String -> [(a, String)]}

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
  [(res, [])] -> res
  [(_,res)]   -> error "Parser did not consume entire stream."
  _           -> error "Parser error."

item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s -> [(f a, b) | (a, b) <- p s]

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser $ \s -> [(a, s)]

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> (Parser p2) =
    Parser $ \s -> [(f a, s2) | (f, s1) <- p1 s, (a, s2) <- p2 s1]

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (Parser p) >>= k =
    Parser $ \s -> p s >>= (\(a, s') -> parse (k a) s')

failure :: Parser a
failure = Parser $ \s -> []

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser $ \s -> parse p s ++ parse q s

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = failure
  (<|>) = option

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
  if p c then
    return c
  else
    failure

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (chainl1 p op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest --do {a <- p; rest a}
  where
    rest a = (do
      f <- op
      b <- p
      rest (f a b)) <|> return a

char :: Char -> Parser Char
char c = satisfy (== c)

natural :: (Read a, Integral a) => Parser a
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

integer :: (Integral a, Read a) => Parser a
integer = do
  sign <- string "-" <|> return []
  digits <- some $ satisfy isDigit
  return $ read (sign ++ digits)

parens :: Parser a -> Parser a
parens p = do
  char '('
  x <- p
  char ')'
  return x
