{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Data.Fix

data SBool = STrue | SFalse deriving (Show)

data Expr' a
  = Number Integer
  | Bool SBool
  | Symbol String
  | If a a a
  deriving (Functor, Show)

type Expr = Fix Expr'
