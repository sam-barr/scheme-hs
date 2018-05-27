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
  | otherwise = do
    bindings <- zip syms <$> mapM newIORef args 
    eval (extend bindings env) body
applyProc (P x) args = case x of
  "+" -> return $ N $ sum nums
  "-" -> return $ N $ head nums - (sum (tail nums))
  "*" -> return $ N $ foldr (*) 1 nums
  "/" -> return $ N $ foldr (/) 1 nums where (/) = div
  "list"  -> return $ L args
  "car"   -> return $ car args
  "cdr"   -> return $ cdr args
  "cons"  -> return $ cons args
  "eq?"   -> return $ eq args
  "null?" -> return $ null' args
  "atom?" -> return $ atom args
  "print" -> print' args >> return Void
  "apply" -> apply args

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

    null' [L x] = B $ null x
    null' [_]   = B False
    null' _     = error "null? error"

    atom [L _] = B False
    atom [_]   = B True
    atom _     = error "atom? error"

    print' [x] = print x
    print' _   = error "print error"

    apply [o,L as] = applyProc o as
    apply _        = error "apply error"

applyProc x _ = error $ show x

-- Evaluater Function
eval :: Env -> Expr -> IO Result

-- evaulate a symbol
eval env (Symbol s) = do
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
  eval (extend bindList env) body

-- evaluate a lambda expression
eval env (Lambda args body) = return $ C args body env

-- evaluate a set! statement
eval env (Set sym val) = do
  val' <- eval env val
  let binding = get sym env
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

  let env'@(e:_:es) = (sym, def) :  (zeroWidth:sym, valref) : env
  eval env' $ Set sym val
  return $ D $ (e:es)
  where
    zeroWidth = '\8203'

  -- evaluate application expression
eval env (AppExp (o:a)) = do
  op <- eval env o
  args <- mapM (eval env) a
  applyProc op args

eval env (AppExp []) = error "No procedure"

-- numbers and boolean evaluate to themselves
eval env (Number n) = return $ N n
eval env (Bool b) = return $ B b

-- evaluate a quoted statement
eval env (Quote ex) = return $ evalQuote ex

-- how to evaluate a quoted statement (certain types of expressions will not appear as quoted statements)
evalQuote :: Expr -> Result
evalQuote (Number n)  = N n
evalQuote (Bool b)    = B b
evalQuote (Symbol s)  = S s
evalQuote (AppExp xs) = L $ map evalQuote xs
