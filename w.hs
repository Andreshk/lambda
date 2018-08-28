-- Algorithm W Step by Step, by Martin Grabmueller

-- Complete implementation of the classic algorithm W for
-- Hindley-Milner polymorphic type inference in Haskell, used
-- as the basis of the type checkers of languages like ML or
-- Haskell. For a very readable presentation of this algorithm
-- and possible variations and extensions read also Heeren[2002].

import qualified Data.HashMap.Lazy as Map -- used for representing contexts (also sometimes called environments)
import qualified Data.HashSet      as Set -- used for representing sets of type variables, etc
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.State (StateT, runStateT, put, get)
import Data.List (intercalate)

data Expr = EVar Int
          | EApp Expr Expr
          | EAbs Int Expr

data Type = TVar Int
          | Type :-> Type
          deriving Eq

-- A type scheme "forall a_1,...,a_n.t" is a type in which a number of 
-- polymorphic type variables are bound to a universal quantifier.
data Scheme = Scheme [Int] Type

-- Determining the free type variables of a type and applying a substituion.
-- Implemented in a type class, because they'll also be needed for contexts, etc.
class Types a where
  ftv   :: a -> Set.HashSet Int
  apply :: Subst -> a -> a

instance Types Type where
  ftv (TVar n)    = Set.singleton n
  ftv (t1 :-> t2) = ftv t1 `Set.union` ftv t2

  apply (Subst s) (TVar n) = case Map.lookup n s of
                               Nothing -> TVar n
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
newtype Subst = Subst (Map.HashMap Int Type)

nullSubst :: Subst
nullSubst = Subst Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst (Subst s1) (Subst s2) = Subst $ (apply (Subst s1) <$> s2) `Map.union` s1

-- Contexts (or type environments), called $\Gamma$ in the text,
-- are mappings from term variables to their respective type schemes.
newtype Context = Context (Map.HashMap Int Scheme)

nullCtx :: Context
nullCtx = Context Map.empty

ctxLookup :: Int -> Context -> Maybe Scheme
ctxLookup i (Context g) = Map.lookup (i + 1 - (Map.size g)) g

instance Types Context where
  ftv (Context g)     = ftv (Map.elems g)
  apply s (Context g) = Context (apply s <$> g)

-- Abstracts a type over all type variables, which are
-- free in the type, but not free in the given context.
generalize :: Context -> Type -> Scheme
generalize g t = Scheme vars t
  where vars = Set.toList ((ftv t) `Set.difference` (ftv g))

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
     return $ TVar c

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
-- general unifier for two given types. A unifier is a substitution
-- S such that (apply S t1) == (apply S t2). The function varBind
-- attempts to bind a type variable to a type and return that binding
-- as a subsitution, but avoids binding a variable to itself and
-- performs the occurs check, responsible for circularity type errors.
mgu :: Type -> Type -> TI Subst
mgu (l :-> r) (l' :-> r') =
  do s1 <- mgu l l'
     s2 <- mgu (apply s1 r) (apply s1 r')
     return (s1 `composeSubst` s2)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t

varBind :: Int -> Type -> TI Subst
varBind u t
  | t == TVar u          = return nullSubst
  | u `Set.member` ftv t = throwError $ "  occurs check fails: " ++ typename u ++ " vs. " ++ show t
  | otherwise            = return $ Subst (Map.singleton u t)

-- Inferring the type of an expression. The context must contain bindings
-- for all free variables of the expressions. The returned substitution
-- records the type constraints imposed on type variables by the
-- expression, and the returned type is the type of the expression.
ti :: Context -> Expr -> TI (Subst, Type)
ti g (EVar i) =
  case ctxLookup i g of
    Nothing    -> throwError $ "  unbound variable: " ++ show (name i)
    Just sigma -> do t <- instantiate sigma
                     return (nullSubst, t)
ti g (EApp e1 e2) =
  do tv <- newTypeVar
     (s1, t1) <- ti g e1
     (s2, t2) <- ti (apply s1 g) e2
     s3 <- mgu (apply s2 t1) (t2 :-> tv)
     return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
  `catchError`
  (\err -> throwError $ err ++ "\n  in " ++ showCtx g (EApp e1 e2))
ti (Context g) (EAbs k e) =
  do -- Negative numbers are inserted as fake DeBruijn indices, instead of increasing all other indices
     let n = Map.size g
         indices = map negate [n..n+k-1]
     tvs <- mapM (const newTypeVar) indices
     let g' = Context (foldr (\(idx,tv) -> Map.insert idx (Scheme [] tv)) g (zip indices tvs))
     (s1, t1) <- ti g' e
     return (s1, foldr ((:->) . apply s1) t1 tvs)
  `catchError`
  (\err -> throwError $ err ++ "\n  in " ++ showCtx (Context g) (EAbs k e))

-- This simple test function tries to infer the type for the given
-- expression. If successful, it prints the expression together with its
-- type, otherwise, it prints the error message. The helper function
-- calls ti and applies the returned substitution to the returned type.
infer :: Expr -> IO ()
infer e =
  do (res, _) <- runTI $ (return . uncurry apply) =<< ti nullCtx e
     case res of
       Left err -> putStrLn $ show e ++ "\n" ++ err ++ "\n"
       Right t  -> putStrLn $ show e ++ " :: " ++ show t ++ "\n"

-- Tests
e0, e1, e2, e3, e4, e5, e6 :: Expr
e0 = EAbs 3 $ EApp (EApp (EVar 2) (EVar 0)) (EApp (EVar 1) (EVar 0))
e1 = EAbs 1 $ EVar 0
e2 = EAbs 2 $ EVar 1
e3 = EAbs 2 $ EVar 0
e4 = EAbs 1 $ EApp (EApp (EVar 0) e0) e2
e5 = EAbs 3 $ EApp (EVar 0) (EVar 4)
e6 = EAbs 1 $ EApp (EVar 0) (EAbs 1 $ EApp (EVar 0) (EVar 0))

-- Main Program
main :: IO ()
main = mapM_ infer [e0, e1, e2, e3, e4, e5, e6]

-- Pretty-printing
name n = ("uvwxyz"!!(rem n 6)) : (if n < 6 then "" else (show (div n 6)))

instance Show Expr where
  show = showCtx nullCtx

showCtx (Context g) e = show' ctx n e
  where (ctx,n) = let n = Map.size g in (map name $ reverse [0..n-1], n)
        show' ctx n (EVar i)     = (if i < n then ctx!!i else name i)
        show' ctx n (EApp e1 e2) = showBr ctx n e1 ++ showBr ctx n e2
        show' ctx n (EAbs k e)   = "lambda[" ++ intercalate "," names ++ "]" ++ show' ctx' (n+k) e
          where names = map name [n..n+k-1]
                ctx' = reverse names ++ ctx
        showBr ctx n e@(EVar _) = show' ctx n e
        showBr ctx n e = "(" ++ show' ctx n e ++ ")"

instance Show Type where
  show (TVar n)  = typename n
  show (t :-> s) = show' t ++ " -> " ++ show s
    where show' (t :-> s) = "(" ++ show (t :-> s) ++ ")"
          show' t = show t

instance Show Scheme where
  show (Scheme vars t) = "forall " ++ intercalate "," (map show vars) ++ ". " ++ show t
