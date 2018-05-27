module Expr where

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
  | Define String Expr
  | AppExp [Expr]
  | Quote Expr
  deriving (Eq, Show)
