module Ast (Expr(..), ExprType(..)) where

data Expr
  = EVar     String
  | ENat     Int
  | EBool    Bool
  | EApp     Expr Expr
  | ELam     String ExprType Expr
  | EPair    Expr Expr
  | EFst     Expr
  | ESnd     Expr
  | EInl     Expr ExprType
  | EInr     Expr ExprType
  | ECase    Expr String Expr String Expr
  deriving (Show, Eq, Ord)

data ExprType
  = TBool
  | TNat
  | TProd    ExprType ExprType
  | TSum     ExprType ExprType
  | TFun     ExprType ExprType
  deriving (Show, Eq, Ord)