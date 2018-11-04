{-|
The language's type system's types.
-}
module Type
  ( Type(..)
  , tyBoolean
  , tyNumber
  ) where

type Name = String

data Type
  = TyVar Name
  | TyConst Name
  | TyArrow Type
            Type
  deriving (Eq, Show)

tyNumber = TyConst "Number"

tyBoolean = TyConst "Boolean"
