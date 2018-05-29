module Main where

import Control.Monad
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

-- input - evaluate loop
repl :: Env -> IO ()
repl env = do
  str <- prompt
  newEnv <- interpret env $ runParser expr str
  repl newEnv

-- interpret a piece of code in an environment, print the result and return the resulting environment
interpret :: Env -> Expr -> IO Env
interpret env code = do
  res <- eval env code
  printResult res
  case res of
    (D new) -> return new
    _       -> return  env

-- print a result
printResult :: Result -> IO ()
printResult result = case show result of
  ""  -> return ()
  str -> putStrLn str

-- prompt the user for input
prompt :: IO String
prompt = do
    putStr "> "
    hFlush stdout
    getLine

-- load a file
loadFile :: Maybe FilePath -> IO Env
loadFile Nothing   = initEnv
loadFile (Just fp) = do
  contents <- readFile fp
  init <- initEnv
  foldM interpret init $ runParser file contents   
