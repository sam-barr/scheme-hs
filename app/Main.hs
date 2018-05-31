module Main where

import Control.Monad
import Control.Monad.Reader
import System.IO
import System.Exit
import System.Environment
import Data.Maybe

import Parse
import Interp
import Expr
import NanoParsec
import Result

-- run scheme
main :: IO ()
main = do
  args <- getArgs
  init <- loadFile $ listToMaybe args
  repl init

-- read - evaluate - print loop
repl :: Env -> IO ()
repl env = do
  str <- prompt
  newEnv <- interpret env $ runParser expr str
  repl newEnv

-- interpret a piece of code in an environment, print the result and return the resulting environment
interpret :: Env -> Expr -> IO Env
interpret env code = do
  res <- runReaderT (eval code) env
  printResult res
  case res of
    (D new) -> return new
    _       -> return  env

-- print a result
printResult :: Result -> IO ()
printResult result = unless (null str) $ putStrLn str
  where
    str = show result

-- prompt the user for input
prompt :: IO String
prompt = putStr "> " >>
         hFlush stdout >>
         getLine

-- load a file
loadFile :: Maybe FilePath -> IO Env
loadFile Nothing   = initEnv
loadFile (Just fp) = do
  contents <- readFile fp
  init <- initEnv
  foldM interpret init $ runParser file contents   
