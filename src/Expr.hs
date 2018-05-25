{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Expr where

import Data.List (intercalate)
import Data.IORef

-- Expression datatype
data Expr
  = Number Integer
  | Bool Bool
  | Symbol String
  | If Expr Expr Expr
  | Let [(String, Expr)] Expr
  | Lambda [String] Expr
  | Set String Expr
  | Begin [Expr]
  | AppExp [Expr]
  | List [Expr]
  | Prim String
  | Closure [String] Expr Env
  | Void
  deriving (Eq)

-- Show instance of Expr
instance Show Expr where
  show (Number n) = show n
  show (Bool b) = if b then "#t" else "#f"
  show (Symbol s) = s
  show (List xs) = "(" ++ intercalate " " (map show xs) ++ ")"
  show Void = ""

-- Environment Datatype
type Env = [(String, IORef Expr)]

-- Extend the environment
extend :: Env -> Env -> Env
extend = (++)

-- Get the value of a symbol in the environment
get :: String -> Env -> IORef Expr
get sym env = case lookup sym env of
  Just expr -> expr
  Nothing   -> error $ "Error: " ++ sym ++ " not in environment"

-- Initial Environment
initEnv :: IO Env
initEnv = sequence $ fmap sequence $ [("null", newIORef $ List [])] ++ map f ["+", "-", "*", "/", "list", "cons", "cdr", "car", "eq?"]
  where
    f = (,) <*> (newIORef . Prim)
