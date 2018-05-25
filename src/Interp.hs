module Interp where

-- TODO: separate Expr and results

import Expr
import Parse

import Control.Monad
import Control.Applicative
import Data.IORef

-- apply a procedure to its arguments
applyProc :: Expr -> [Expr] -> IO Expr
applyProc (Closure syms body env) args 
  | length syms /= length args = error "Wrong Number of Arguments"
  | otherwise = 
    let bindings = zip syms <$> mapM newIORef args in
        eval (liftA2 extend bindings $ pure env) body
applyProc (Prim x) args = return $ case x of
  "+" -> Number $ sum nums
  "-" -> Number $ head nums - (sum (tail nums))
  "*" -> Number $ foldr (*) 1 nums
  "/" -> Number $ foldr (/) 1 nums where (/) = div
  "list" -> List args
  "car" -> car args
  "cdr" -> cdr args
  "cons" -> cons args
  "eq?" -> eq args

  where
    assertNumber (Number n) = n
    assertNumber _ = error "Expected a Number"
    
    car [(List (x:_))] = x
    car _ = error "car error"

    cdr [(List (_:xs))] = List xs
    cdr _ = error "cdr error"

    cons [x, (List xs)] = List (x:xs)
    cons _ = error "cons error"

    nums = if null args then error "more args pls" else map assertNumber args

    eq [x, y] = Bool $ x == y
    eq _ = error "eq? error"

applyProc x _ = error $ show x

-- Evaluater Function
eval :: IO Env -> Expr -> IO Expr

-- evaulate a symbol
eval e (Symbol s) = do
  env <- e
  readIORef $ get s env

-- evaluate an if statement
eval env (If b true false) = do
  bool <- eval env b
  case bool of
    Bool True  -> eval env true
    Bool False -> eval env false
    _          -> error "Expected Boolean"

-- evaluate a let statement
eval env (Let bindings body) = do
  bindList <- forM bindings $ \(sym, val) -> do
    val' <- newIORef =<< eval env val
    return (sym, val')
  eval (extend bindList <$> env) body

-- evaluate a lambda expression
eval env (Lambda args body) = env >>= return . Closure args body

-- evaluate a set! statement
eval env (Set sym val) = do
  val' <- eval env val
  binding <- get sym <$> env
  writeIORef binding val'
  return Void

-- evaluate begin statement
eval env (Begin [])     = return Void
eval env (Begin [x])    = eval env x
eval env (Begin (x:xs)) = eval env x >> eval env (Begin xs)

-- evaluate application expression
eval env (AppExp (o:a)) = do
  op <- eval env o
  args <- mapM (eval env) a
  applyProc op args

eval env (AppExp []) = error "No procedure"

eval env x = return x
