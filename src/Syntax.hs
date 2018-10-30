{-|
The language's AST type definition.
-}
module Syntax where

import           Type (Type)

type Name = String

data Expr
  = Number Integer
  | Boolean Bool
  | Var String
  | BinOp Op
          Expr
          Expr
  | IfExpr Expr
           Expr
           Expr
  | LetBind Name
            Expr
            Expr
  | Apply Expr
          Expr
  | Lambda Name
           Expr
  deriving (Eq, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  | Greater
  | Lesser
  deriving (Eq, Show)
