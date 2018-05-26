module Interp where

import Expr
import Parse
import Result

import Control.Monad
import Control.Applicative
import Data.IORef
import GHC.Exts

-- apply a procedure to its arguments
applyProc :: Result -> [Result] -> IO Result
applyProc (C syms body env) args 
  | length syms /= length args = error "Wrong Number of Arguments"
  | otherwise = 
    let bindings = zip syms <$> mapM newIORef args in
        eval (liftA2 extend bindings $ pure env) body
applyProc (P x) args = return $ case x of
  "+" -> N $ sum nums
  "-" -> N $ head nums - (sum (tail nums))
  "*" -> N $ foldr (*) 1 nums
  "/" -> N $ foldr (/) 1 nums where (/) = div
  "list" -> L args
  "car" -> car args
  "cdr" -> cdr args
  "cons" -> cons args
  "eq?" -> eq args

  where
    assertNumber (N n) = n
    assertNumber _ = error "Expected a Number"

    nums = if null args then error "more args pls" else map assertNumber args
    
    car [(L (x:_))] = x
    car _ = error "car error"

    cdr [(L (_:xs))] = L xs
    cdr _ = error "cdr error"

    cons [x, (L xs)] = L (x:xs)
    cons _ = error "cons error"

    eq [x, y] = B $ x == y
    eq _ = error "eq? error"

applyProc x _ = error $ show x

-- Evaluater Function
eval :: IO Env -> Expr -> IO Result

-- evaulate a symbol
eval e (Symbol s) = do
  env <- e
  readIORef $ get s env

-- evaluate an if statement
eval env (If b true false) = do
  bool <- eval env b
  case bool of
    B True  -> eval env true
    B False -> eval env false
    _          -> error "Expected Boolean"

-- evaluate a let statement
eval env (Let bindings body) = do
  bindList <- forM bindings $ \(sym, val) -> do
    val' <- newIORef =<< eval env val
    return (sym, val')
  eval (extend bindList <$> env) body

-- evaluate a lambda expression
eval env (Lambda args body) = env >>= return . C args body

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

-- evaluate a definition
-- this code is awful jesus christ
eval env (Define sym val) = do
  val' <- eval env val
  valref <- newIORef val'
  def <- newIORef $ N 0

  env' <- ([(sym, def), (zeroWidth:sym, valref)] ++) <$> env
  eval (pure env') $ Set sym val
  return $ D $ (head env') : (tail (tail env'))
  where
    zeroWidth = '\8203'

  -- evaluate application expression
eval env (AppExp (o:a)) = do
  op <- eval env o
  args <- mapM (eval env) a
  applyProc op args

eval env (AppExp []) = error "No procedure"

eval env (Number n) = return $ N n
eval env (Bool b) = return $ B b
