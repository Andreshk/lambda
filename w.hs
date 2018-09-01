module W where
-- Algorithm W Step by Step
-- Original implementation: Martin Grabmueller
--   (https://github.com/wh5a/Algorithm-W-Step-By-Step)

-- Complete implementation of the classic algorithm W for
-- Hindley-Milner polymorphic type inference in Haskell, used
-- as the basis of the type checkers of languages like ML or
-- Haskell. For a very readable presentation of this algorithm
-- and possible variations and extensions read also Heeren[2002].

import qualified Data.IntMap.Lazy as Map -- used for representing contexts (also sometimes called environments)
import qualified Data.IntSet      as Set -- used for representing sets of type variables, etc.
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.State (StateT, runStateT, put, get)
import Data.List (intercalate)
import Lambda (Lambda(..), name, showCtx) -- the compressed lambda terms

infixr :->
data Type = T Int
          | Type :-> Type
          deriving Eq

-- Order of a type, as in "higher-order function"
order :: Type -> Int
order (T _) = 0
order (t1 :-> t2) = max (order t1 + 1) (order t2)

-- A type scheme "forall a_1,...,a_n.t" is a type in which a number of 
-- polymorphic type variables are bound to a universal quantifier.
data Scheme = Scheme [Int] Type

-- Determining the free type variables of a type and applying a substituion.
-- Implemented in a type class, because they'll also be needed for contexts, etc.
class Types a where
  ftv   :: a -> Set.IntSet
  apply :: Subst -> a -> a

instance Types Type where
  ftv (T n)    = Set.singleton n
  ftv (t1 :-> t2) = ftv t1 `Set.union` ftv t2

  apply (Subst s) (T n) = case Map.lookup n s of
                               Nothing -> T n
                               Just t  -> t
  apply s (t1 :-> t2) = (apply s t1) :-> (apply s t2)

-- A substitution only replaces free type variables, so the
-- quantified type variables in a type scheme are not affected.
instance Types Scheme where
  ftv (Scheme vars t) = (ftv t) `Set.difference` (Set.fromList vars)
  apply (Subst s) (Scheme vars t) = Scheme vars (apply (Subst $ foldr Map.delete s vars) t)

-- It will occasionally be useful to extend the Types methods to lists.
instance Types a => Types [a] where
  apply s = map (apply s)
  ftv l   = foldr Set.union Set.empty (map ftv l)

-- Substitutions are finite mappings from type variables to types.
newtype Subst = Subst (Map.IntMap Type)

nullSubst :: Subst
nullSubst = Subst Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst (Subst s1) (Subst s2) = Subst $ (apply (Subst s1) <$> s2) `Map.union` s1

-- Contexts (or type environments), called $\Gamma$ in the text,
-- are mappings from term variables to their respective type schemes.
-- Maps the shifted DeBruijn index of a bound variable to its scheme
-- (see the comment below).
newtype Context = Context (Map.IntMap Scheme)

nullCtx :: Context
nullCtx = Context Map.empty

-- Each new bound variable is added with a fake negative DeBruijn
-- index, instead of increasing the DeBruijn indices of all other
-- variables by 1. On lookup the index searched is shifted down,
-- in order to simulate containing a range of [0..n-1].
ctxInsert :: Int -> Context -> TI (Context, [Type])
ctxInsert k (Context g) =
  do let n = Map.size g
         indices = map negate [n..n+k-1]
     tvs <- mapM (const newTypeVar) indices
     let g' = Context $ foldr (\(idx,t) -> Map.insert idx (Scheme [] t)) g (zip indices tvs)
     return (g', tvs)

ctxLookup :: Int -> Context -> Maybe Scheme
ctxLookup i (Context g) = Map.lookup (i + 1 - (Map.size g)) g

instance Types Context where
  ftv (Context g)     = ftv (Map.elems g)
  apply s (Context g) = Context (apply s <$> g)

-- Several operations, for example type scheme instantiation, require
-- fresh names for newly introduced type variables. This is implemented
-- by using an appropriate monad which takes care of generating fresh
-- names. It is also capable of passing a dynamically scoped environment,
-- additional error handling, or performing I/O by adding more transformers.
-- The ExceptT pretty much acts as a Reader, collecting the "stack trace"
-- of errors during type inference of a term.
newtype TIState = TIState Int

type TI a = ExceptT String (StateT TIState IO) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t = runStateT (runExceptT t) (TIState 0) >>= return

newTypeVar :: TI Type
newTypeVar =
  do (TIState c) <- get
     put $ TIState (c + 1)
     return $ T c

-- Generates human-friendly names for type variables
typename :: Int -> String
typename n = ("abc"!!(rem n 3)) : (if n < 3 then "" else show $ div n 3)

-- Replace all bound type variables in a type scheme with fresh type variables.
-- Fun fact: const newTypeVar is not really a constant function.
instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) =
  do nvars <- mapM (const newTypeVar) vars
     let s = Map.fromList (zip vars nvars)
     return $ apply (Subst s) t

-- This is the unification function for types, returning the most
-- general unifier for two given types. A unifier of t1 and t2 is
-- a substitution S such that (apply S t1) == (apply S t2). The
-- function varBind attempts to bind a type variable to a type
-- and return that binding as a subsitution, but avoids binding
-- a variable to itself and performs the occurs check, responsible
-- for circularity type errors.
mgu :: Type -> Type -> TI Subst
mgu (l :-> r) (l' :-> r') =
  do s1 <- mgu l l'
     s2 <- mgu (apply s1 r) (apply s1 r')
     return (s1 `composeSubst` s2)
mgu (T u) t = varBind (T u) t
mgu t (T u) = varBind (T u) t

varBind :: Type -> Type -> TI Subst
varBind (T u) t
  | t == T u          = return nullSubst
  | u `Set.member` ftv t = throwError $ "  occurs check fails: " ++ typename u ++ " vs. " ++ show t
  | otherwise            = return $ Subst (Map.singleton u t)

-- Inferring the type of an expression. The context must contain bindings
-- for all free variables of the expressions. The returned substitution
-- records the type constraints imposed on type variables by the
-- expression, and the returned type is the type of the expression.
-- The third argument is used to silence the error reporting in some cases.
ti :: Context -> Lambda -> Bool -> TI (Subst, Type)
ti g (Var i) _ =
  case ctxLookup i g of
    Nothing    -> throwError $ "  unbound variable: " ++ show (name i)
    Just sigma -> do t <- instantiate sigma
                     return (nullSubst, t)
ti g (Ap [t]) _ = ti g t True
ti g (Ap ts) b =
  do tv <- newTypeVar
     (s1, t1) <- ti g (Ap $ init ts) False
     (s2, t2) <- ti (apply s1 g) (last ts) True
     s3 <- mgu (apply s2 t1) (t2 :-> tv)
     return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
  `catchError`
  (\err -> throwError $ if b then err ++ "\n  in " ++ showCtx' g (Ap ts) else err)
ti g (L k t) _ =
  do (g', tvs) <- ctxInsert k g
     (s1, t1) <- ti g' t True
     return (s1, foldr ((:->) . apply s1) t1 tvs)
  `catchError`
  (\err -> throwError $ err ++ "\n  in " ++ showCtx' g (L k t))

-- This simple test function tries to infer the type for the given
-- expression. If successful, it prints the expression together with its
-- type, otherwise, it prints the error message. The helper function
-- calls ti and applies the returned substitution to the returned type.
infer :: Lambda -> IO ()
infer t = printRes =<< (runTI $ (return . uncurry apply) =<< ti nullCtx t True)
  where printRes (Left err, _) = putStrLn $ show t ++ "\n" ++ err ++ "\n"
        printRes (Right ty, _) = putStrLn $ show t ++ " :: " ++ show ty ++ "\n"

-- Tests
e0, e1, e2, e3, e4, e5, e6 :: Lambda
e0 = L 3 (Ap [Var 2, Var 0, Ap [Var 1, Var 0]])
e1 = L 1 (Var 0)
e2 = L 2 (Var 1)
e3 = L 2 (Var 0)
e4 = L 1 (Ap [Var 0, e0, e2])
e5 = L 3 (Ap [Var 0, Var 4])
e6 = L 2 (Ap [Var 0, Var 1, Ap [Var 0, Var 0], Var 0, Var 1])

-- Main Program
main :: IO ()
main = mapM_ infer [e0, e1, e2, e3, e4, e5, e6]

-- Pretty-printing
showCtx' :: Context -> Lambda -> String
showCtx' (Context g) = showCtx names n
  where (names, n) = let n = Map.size g in (name <$> reverse [0..n-1], n)

-- The -> operator is right-associative, so a->(b->c) is the same as
-- a->b->c and is printed this way, whereas (a->b)->c is different
instance Show Type where
  show (T i) = typename i
  show (t :-> t1) = show' t ++ " -> " ++ show t1             -- no brackets by default...
    where show' (t1 :-> t2) = "(" ++ show (t1 :-> t2) ++ ")" -- ...unless the left arg is a function
          show' t = show t

instance Show Scheme where
  show (Scheme vars t) = "forall " ++ intercalate "," (map show vars) ++ ". " ++ show t
