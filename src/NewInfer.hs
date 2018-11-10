-- TODO: non mi piace, ho solo seguito diehl + wikipedia. Non capisco alcune
-- cose sulle sostituzioni e voglio capire come implementare l'algoritmo
module NewInfer () where
import Syntax hiding (Name)

import           Control.Monad.State
import           Control.Monad.Trans.Except (ExceptT, runExceptT)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

type Name = String -- TODO: use Text instead

-- | The id of a type variable
newtype Id = Id Int
  deriving (Show, Eq, Ord)

-- | A monotype (tau)
data Type
  = TVar Id -- Type Variables (alpha)
  | TArrow Type -- Type application of arity 2
           Type
  | TBool -- Type applications of arity 0
  | TNumber
  deriving (Show, Eq, Ord)

-- | A polytype (sigma)
data Scheme =
  Forall [Id] -- Quantifier
         Type
  deriving (Show, Eq, Ord)

-- | The context gamma
newtype Context = Context (Map.Map Name Scheme)
  deriving (Show, Eq, Ord)

remove :: Context -> Name -> Context
remove (Context ctx) name = Context (Map.delete name ctx)

-- | A substitution TVar |-> Type
type Substitution = Map.Map Id Type

-- S = []
emptySubstitution = Map.empty

-- Compose 2 substitutions
-- S_1 o S_2 = S_1(S_2) U S_1
compose s1 s2 = Map.map (subst s1) s2 `Map.union` s1

-- | The class to describe the functions that Context, Scheme and Type will
-- have to implement
class Types a where
  free :: a -> Set.Set Id
  subst :: Substitution -> a -> a

instance Types Type where
  -- free(alpha) = {alpha}
  free (TVar id) = Set.singleton id
  -- free(C t_1 ... t_n) = Union_i (free(t_i))
  free (TArrow t1 t2) = free t1 `Set.union` free t2
  free TBool = Set.empty
  free TNumber = Set.empty

  -- [alpha -> tau] alpha = tau
  -- [alpha -> tau_1] tau_2 = tau_2
  subst s t@(TVar id) = Map.findWithDefault t id s
  -- S(C t_1 ... t_n) = C S(t_1) ... S(t_n)
  subst s (TArrow t1 t2) = subst s t1 `TArrow` subst s t2
  subst _ TBool = TBool
  subst _ TNumber = TNumber

instance Types Scheme where
  -- free(forall alphas.tau) = free(tau) \ alphas
  free (Forall tvars t) = free t `Set.difference` Set.fromList tvars

  -- S(forall alphas.tau) = forall alphas.S\s(tau)
  subst s (Forall tvars t) = Forall tvars $ subst s' t
                               where s' = foldr Map.delete s tvars
                               -- Removing substitutions that contain tvars
                               -- from the substitutions for tau.
                               -- It's equivalent to the rule
                               -- s === [beta -> upsilon]
                               -- upsilon /= alpha, foreach alpha in alphas
                               -- TODO: verify. This doesnt not protect us from what
                               -- the rule `alpha not in free variables of upsilon`
                               -- but theres no need since all of the free variables
                               -- are unique, since they are generated in
                               -- ascending order


instance Types Context where
  -- free(gamma) = U free(sigma), foreach sigma in gamma
  free (Context ctx) = free $ Map.elems ctx

  -- S(gamma) = { S(sigma) foreach sigma in gamma }
  subst s (Context ctx) = Context $ Map.map (subst s) ctx

  -- To make the previous rules work with lists
instance Types a => Types [a] where
  free   = foldr (Set.union . free) Set.empty
  subst s xs = map (subst s) xs

data TypeError
  = IncompatibleTypes Type
                      Type
  | VariableNotFound Name

type Infer a = ExceptT TypeError (State Id) a

freshTVar :: Infer Type
freshTVar = do
  s@(Id i) <- get
  put $ Id (i+1)
  return $ TVar s


-- | Checks if the type variable alpha occurs in the free variables of the type beta
occursInFreeVariables :: Types a => Id -> a -> Bool
occursInFreeVariables alpha beta = alpha `Set.member` free beta


-- TODO: figure out if we want to use algorithm J or W or the one
-- from: A modern eye on ML type inference by Francois Pottier

type Constraint = (Type, Type)
genConstraints :: Expr -> Context -> Infer (Type, [Constraint])
genConstraints expr ctx = undefined

infer expr ctx =
  -- (ty, cnst) <- evalState (runExceptT (genConstraints expr ctx)) 0
  evalState (runExceptT (genConstraints expr ctx)) (Id 0)
  -- substitutions <- unify cnst
  -- Apply all of the substitutions to the final type
  -- return $ tySubstMany substitutions ty