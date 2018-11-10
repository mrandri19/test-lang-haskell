module Infer
  ( infer
  , InferError(..)
  , emptyContext
  , Context
  ) where

import           Control.Monad.Except       (throwError)
import           Control.Monad.State
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (fromMaybe)

import           Syntax

import           Debug.Trace

-- Syntax imports name too, they are equal for now but we want Type's one
import           Type                       hiding (Name)

-- | Represents a type equation where types S and T must be equal. S = T
type Constraint = (Type, Type)

-- | The context, which maps the name of a variable to its type
type Context = Map.Map Name Type

-- | The empty context, with no names bound to any type
emptyContext :: Context
emptyContext = Map.empty

-- | The possible errors encountered while solving the system of type equations
-- or while generating the constraints
data InferError
  = IncompatibleTypes Type
                      Type
  | VariableNotFound Name
  deriving (Eq, Show)

-- | The monad stack in which the constraint generation will occur. We want to
-- represent the possibility of an ConstraintError and the Stateful operation of getting
-- a new type variable, it can return a generic type `a`
type Infer a = ExceptT InferError (State Int) a

-- | Return a new type variable, updating the internal state
newTypeVariable :: Infer Type
newTypeVariable = do
  tyid <- get
  put (tyid + 1)
  return $ TyVar $ show tyid

-- | Generate the inferred type and constraints given an expression and a
-- context
genConstraints :: Expr -> Context -> Infer (Type, [Constraint])
genConstraints expr ctx =
  case expr of
    Number _ -> return (tyNumber, [])
    Boolean _ -> return (tyBoolean, [])
    Var name ->
      case Map.lookup name ctx of
        Nothing -> throwError $ VariableNotFound name
        Just ty -> return (ty, [])
    BinOp op expr1 expr2 -> do
      (ty1, cnst1) <- genConstraints expr1 ctx
      (ty2, cnst2) <- genConstraints expr2 ctx
      return (exprType op, [(ty1, tyNumber), (ty2, tyNumber)] ++ cnst1 ++ cnst2)
      where exprType Plus    = tyNumber
            exprType Minus   = tyNumber
            exprType Times   = tyNumber
            exprType Divide  = tyNumber
            exprType Greater = tyBoolean
            exprType Lesser  = tyBoolean
    IfExpr cond expr1 expr2 -> do
      (tycond, cnsxcond) <- genConstraints cond ctx
      (ty1, cnst1) <- genConstraints expr1 ctx
      (ty2, cnst2) <- genConstraints expr2 ctx
      return
        (ty2, [(tycond, tyBoolean), (ty1, ty2)] ++ cnsxcond ++ cnst1 ++ cnst2)
    LetBind name expr1 expr2
      -- TODO: add let polymorphism
     -> do
      (ty1, cnst1) <- genConstraints expr1 ctx
      (ty2, cnst2) <- genConstraints expr2 (Map.insert name ty1 ctx)
      return (ty2, cnst1 ++ cnst2)
    Apply expr1 expr2 -> do
      (ty1, cnst1) <- genConstraints expr1 ctx
      -- FIXME: we are generating the constraints for expr2 without putting into
      -- the context the information we already know about expr1
      (ty2, cnst2) <- genConstraints expr2 ctx
      ty <- newTypeVariable
      return (ty, [(ty1, TyArrow ty2 ty)] ++ cnst1 ++ cnst2)
    Lambda name body -> do
      ty <- newTypeVariable
      (ty1, cnst1) <- genConstraints body (Map.insert name ty ctx)
      return (TyArrow ty ty1, cnst1)

-- | Apply one type substitution to the type ty
tySubst :: Constraint -> Type -> Type
tySubst subst ty =
  case ty of
    TyConst _ -> ty
    TyVar k ->
      case subst of
        (TyVar k', x)
          | k' == k -> x
        _ -> ty
    TyArrow ty1 ty2 -> TyArrow (tySubst subst ty1) (tySubst subst ty2)

-- | Apply many type substitutions to the type ty
tySubstMany :: [Constraint] -> Type -> Type
tySubstMany substitutions ty =
  foldl (\acc subst -> tySubst subst acc) ty substitutions

-- | Return whether the type parameter a occurs in the type ty
occurs :: String -> Type -> Bool
occurs tyParam ty =
  case ty of
    TyConst "Number"  -> False
    TyConst "Boolean" -> False
    TyVar tyParam'    -> tyParam == tyParam'
    TyArrow ty1 ty2   -> occurs tyParam ty1 || occurs tyParam ty2

-- | Unify the system of type equations and return an array of substitutions
unify :: [Constraint] -> Either InferError [Constraint]
unify eq =
  let unify' :: [Constraint] -> [Constraint] -> Either InferError [Constraint]
      unify' equations substs =
        case equations of
          [] -> return substs
          ((t1, t2):xs)
            | t1 == t2 -> unify' xs substs
          ((TyArrow tyarg1 tyres1, TyArrow tyarg2 tyres2):xs) ->
            unify' ((tyarg1, tyarg2) : (tyres1, tyres2) : xs) substs
          ((TyVar a, ty):xs)
            | not (occurs a ty) -> unifyTyVar a ty xs substs
          ((ty, TyVar a):xs)
            | not (occurs a ty) -> unifyTyVar a ty xs substs
          ((ty1, ty2):_) -> Left $ IncompatibleTypes ty1 ty2
        where
          unifyTyVar a ty xs substs =
            let ts = tySubst (TyVar a, ty)
                eq' = map (\(ty1, ty2) -> (ts ty1, ts ty2)) xs
                substs' = (TyVar a, ty) : map (\(n, u) -> (n, ts u)) substs
             in unify' eq' substs'
   in unify' eq []

-- | Perform the inference
infer :: Expr -> Context -> Either InferError Type
infer expr ctx = do
  (ty, cnst) <- evalState (runExceptT (genConstraints expr ctx)) 0
  substitutions <- unify cnst
  -- Apply all of the substitutions to the final type
  return $ tySubstMany substitutions ty
