{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Interp where

import Expr
import Parse
import Result

import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.IORef
import System.IO.Unsafe

-- IORef functions lifted to MonadIO
newRef :: MonadIO m => a -> m (IORef a)
newRef = liftIO . newIORef

readRef :: MonadIO m => IORef a -> m a
readRef = liftIO . readIORef

writeRef :: MonadIO m => IORef a -> a -> m ()
writeRef ref new = liftIO $ writeIORef ref new

-- perform function application
applyProc :: Result -> [Result] -> IO Result

-- apply a lambda expression
applyProc (C syms body env) args 
  | length syms /= length args = error "Wrong Number of Arguments"
  | otherwise = do
    bindings <- zip syms <$> mapM newRef args 
    runReaderT (eval body) (extend bindings env)

-- apply a primitive procedure
applyProc (P x) args = case x of
  "+" -> return $ N $ sum nums
  "-" -> return $ sub nums
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

    sub [n]    = N (-n)
    sub (x:xs) = N $ x - sum xs
    
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

fixRef :: String -> Result -> ReaderT a IO (String, IORef Result)
fixRef sym (C args body env) = do
    rec ref <- newRef $ C args body ((sym, ref) : env)
    return (sym, ref)
fixRef sym x = (sym, ) <$> newRef x

-- Evaluater Function
eval :: Expr -> ReaderT Env IO Result

-- evaulate a symbol
eval (Symbol s) = do
  env <- ask
  readRef $ get s env

-- evaluate an if statement
eval (If b true false) = do
  bool <- eval b
  case bool of
    B True  -> eval true
    B False -> eval false
    _       -> error "Expected Boolean"

eval (Cond []) = return Void
eval (Cond ((b, expr):bs)) = do
  bool <- eval b
  case bool of
    B True  -> eval expr
    B False -> eval $ Cond bs
    _       -> error "Expected Boolean"

-- evaluate a let statement
eval (Let bindings body) = do
  bindList <- forM bindings $ \(sym, val) -> do
    evaluated <- eval val
    ref <- newRef evaluated
    return (sym, ref)
  local (extend bindList) $ eval body

-- evaluate a lambda expression
eval (Lambda args body) = do
  env <- ask
  return $ C args body env

-- evaluate a set! statement
eval (Set sym val) = do
  evaluated <- eval val
  env <- ask
  let binding = get sym env
  writeRef binding evaluated
  return Void

-- evaluate begin statement
eval (Begin [])     = return Void
eval (Begin [x])    = eval x
eval (Begin (x:xs)) = eval x >> eval (Begin xs)

-- evaluate a definition
eval (Define sym val) = do
    evaluated <- eval val
    env <- ask
    binding <- fixRef sym evaluated
    return $ D (binding : env)

-- evaluate application expression
eval (AppExp (o:a)) = do
  op <- eval o
  args <- mapM eval a
  liftIO $ applyProc op args

eval (AppExp []) = error "No procedure"

-- numbers and boolean evaluate to themselves
eval (Number n) = return $ N n
eval (Bool b)   = return $ B b

-- evaluate a quoted statement
eval (Quote ex) = return $ evalQuote ex

-- how to evaluate a quoted statement (certain types of expressions will not appear as quoted statements)
evalQuote :: Expr -> Result
evalQuote (Number n)  = N n
evalQuote (Bool b)    = B b
evalQuote (Symbol s)  = S s
evalQuote (AppExp xs) = L $ map evalQuote xs
