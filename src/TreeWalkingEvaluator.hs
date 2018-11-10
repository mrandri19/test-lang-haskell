module TreeWalkingEvaluator
  ( eval
  , Context
  , Value(..)
  ) where

import           Data.Maybe (fromMaybe)
import           Syntax

-- TODO: try using small-step semantics
-- Right now we are using VClosure to capture the exection environment, I wonder
-- if using substitution would be better.
-- This is big-step because it maintains a data structure binding variables to values
-- small-step would mean substituting variables for values, it's much less efficient
type Context = [(Name, Value)]

data Value
  = VNumber Integer
  | VBoolean Bool
  | VClosure Name
             Expr
             Context
  deriving (Show, Eq)

-- TODO: convert to Except monad
eval :: Expr -> Context -> Value
eval expr ctx =
  case expr of
    Number n -> VNumber n
    Boolean b -> VBoolean b
    Var name -> fromMaybe failure $ lookup name ctx
      where failure =
              (error $
               "Couldn't find \"" ++ name ++ "\" in context: " ++ show ctx)
    BinOp op expr1 expr2 -> eval' op (eval expr1 ctx) (eval expr2 ctx)
      where eval' Plus (VNumber n1) (VNumber n2)    = VNumber $ n1 + n2
            eval' Minus (VNumber n1) (VNumber n2)   = VNumber $ n1 - n2
            eval' Times (VNumber n1) (VNumber n2)   = VNumber $ n1 * n2
            eval' Divide (VNumber n1) (VNumber n2)  = VNumber $ n1 `quot` n2
            eval' Greater (VNumber n1) (VNumber n2) = VBoolean $ n1 > n2
            eval' Lesser (VNumber n1) (VNumber n2)  = VBoolean $ n1 < n2
            eval' _ _ _                             = error "Type mismatch"
    IfExpr cond expr1 expr2 ->
      case eval cond ctx of
        VBoolean True  -> eval expr1 ctx
        VBoolean False -> eval expr2 ctx
        _              -> error "Type mimsmatch"
    LetBind name expr1 expr2 -> eval expr2 $ (name, eval expr1 ctx) : ctx
    Apply expr1 expr2 ->
      case eval expr1 ctx of
        VClosure name body ctx' -> eval body $ (name, eval expr2 ctx) : ctx'
        _                       -> error "Cannot apply to a non lambda"
    Lambda name expr -> VClosure name expr ctx
