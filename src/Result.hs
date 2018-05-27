module Result where

import Expr

import Data.List (intercalate)
import Data.IORef

data Result
  = N Integer  -- number
  | B Bool     -- boolean
  | S String   -- symbol
  | P String   -- primitive
  | L [Result] -- list
  | C [String] Expr Env
  | D Env
  | Void       -- void

instance Eq Result where
  N x == N y = x == y
  B x == B y = x == y
  S x == S y = x == y
  P x == P y = x == y
  L x == L y = x == y

  _ == _ = False

instance Show Result where
  show (N n) = show n
  show (B b) = if b then "#t" else "#f"
  show (S s) = '`':s
  show (P p) = "<procedure:" ++ p ++ ">"
  show (L l) = "(" ++ intercalate " " (map show l) ++ ")"
  show (C _ _ _) = "<closure>"
  show _ = ""

-- Environment Datatype
type Env = [(String, IORef Result)]

-- Extend the environment
extend :: Env -> Env -> Env
extend = (++)

-- Get the value of a symbol in the environment
get :: String -> Env -> IORef Result
get sym env = case lookup sym env of
  Just expr -> expr
  Nothing   -> error $ "Error: " ++ sym ++ " not in environment"

-- Initial Environment
initEnv :: IO Env
initEnv = sequence $ fmap sequence $ [("null", newIORef $ L [])] ++ map f primitives
  where
    f = (,) <*> (newIORef . P)

primitives :: [String]
primitives = ["+", "-", "*", "/", "list", "cons", "cdr", "car", "eq?", "null?", "atom?", "print", "apply"]
