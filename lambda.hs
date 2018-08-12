{-# LANGUAGE CPP #-} -- C preprocessor for OS detection with ifdef
module Lambda where
import Data.List (sort,nub,intercalate)
import Data.Functor ((<$>))

data Lambda = Var Int
            | Ap [Lambda]  -- application over many arguments: xyzw instead of (((xy)z)w)
            | L Int Lambda -- repeated abstraction: λ[u,v,w]... instead of λ[u]λ[v]λ[w]...
  deriving Eq -- alpha-equivalence is syntactic equivalence

-- Generate a human-friendly variable name from an integer: u,v,w,x,y,z,u1,v1,w1,...
name :: Int -> String
name n = ("uvwxyz"!!(rem n 6)) : (if n < 6 then "" else (show (div n 6)))

-- Pretty-print a λ where supported - i.e. everywhere, except Windows
lambda :: String
#ifdef mingw32_HOST_OS
lambda = "lambda"
#else
lambda = "λ"
#endif

-- We start with an empty context first, but simulate it later when creating a new
-- name for a free variable. Upon reaching a bound variable, we give it a new name
-- and push that name to the front of the context to preserve the deBruijn indices
-- of all variables. Invariant: n == length ctx.
instance Show Lambda where
  show = diese [] 0
    where diese :: [String] -> Int -> Lambda -> String -- from "diese" in music (♯)
          diese ctx n (Var i) = (if i < n then ctx!!i else name i)
          diese ctx n (Ap ts) = concatMap (braceDiese ctx n) ts
          diese ctx n (L k t) = lambda ++ "[" ++ (intercalate "," names) ++ "]" ++ diese ctx' (n+k) t
            where names = map name [n..n+k-1]
                  ctx' = reverse names ++ ctx
          -- braceDiese does the same as diese, except put braces around complex terms (non-variables)
          braceDiese ctx n t@(Var _) = diese ctx n t
          braceDiese ctx n t = "(" ++ diese ctx n t ++ ")"

-- Pretty-print a term to make its structure more easily visible. To-do: DOT
pretty :: Lambda -> IO () -- disclaimer: not actually pretty
pretty = putStrLn . pretty' 0
  where pretty' :: Int -> Lambda -> String -- pretty-print at a given indentation level
        pretty' n (Var i) = replicate n ' ' ++ "Var " ++ (show i) ++ "\n"
        pretty' n (Ap ts) = replicate n ' ' ++ "Ap\n" ++ concatMap (pretty' (n+2)) ts
        pretty' n (L k t) = replicate n ' ' ++ "L " ++ (show k) ++ "\n" ++ pretty' (n+2) t

-- Some classic combinators.
i, k, ks, s, w, om, y, j :: Lambda
i  = L 1 (Var 0) -- I
k  = L 2 (Var 1) -- K
ks = L 2 (Var 0) -- K*
s  = L 3 (Ap [Var 2, Var 0, Ap [Var 1, Var 0]]) -- S
w  = L 1 (Ap [Var 0, Var 0])  -- omega 
om = Ap [w,w] -- the irreducible Omega term
y  = L 1 (Ap [L 1 (Ap [Var 1, Ap [Var 0, Var 0]]), L 1 (Ap [Var 1, Ap [Var 0, Var 0]])]) -- fixed-point Y-combinator
j  = L 1 (Ap [Var 0, s, k]) -- Chris Barker's iota combinator: jj = I, j(jj) = K*, j(j(jj)) = K, j(j(j(jj))) = S
        
-- Increase or decrease all free variables with k, knowing their indices are >= d
arrow :: Int -> Int -> Lambda -> Lambda
arrow k d (Var i) = Var $ if i >= d then i+k else i
arrow k d (Ap ts) = Ap $ map (arrow k d) ts
arrow k d (L n t) = L n $ arrow k (d+n) t -- with each new bound variable all other indices increase by 1

-- Substitution helper functions
up :: Int -> Lambda -> Lambda
up k = arrow k 0    -- bump all free variables with k, knowing their indices start from 0
down :: Lambda -> Lambda
down = arrow (-1) 1 -- reduce all free variables by 1, knowing their indices have been bumped to >=1

-- Nameless lambda substitution
subst :: Int -> Lambda -> Lambda -> Lambda
subst i n (Var j) = if i == j then n else (Var j)
subst i n (Ap ts) = Ap $ map (subst i n) ts
subst i n (L k t) = L k $ (subst (i+k) (up k n) t)

-- Substitution examples:
l1, l2 :: Lambda
l1 = L 1 (Ap [Var 0, L 1 (Ap [Var 1, Var 0, Var 3])]) -- λ[x]x(λ[y]xyz)
l2 = L 1 (Ap [Var 0, Var 1])                          -- λ[u]uv
-- subst 1 l2 l1 -> λ[x]x(λ[y]xy(λ[u]uv)) or something alpha-equivalent to it :)

-- Reductions may simplify the more complex terms
sanitize :: Lambda -> Lambda
sanitize (Ap [t]) = t -- un-apply, if 0 arguments
sanitize (L 0 t) = t  -- un-lambda, if no variables bound
sanitize t = t

-- Traverse the list in depth, looking to reduce only the
-- left-most regex, and return Nothing if no regex is found.
reduceLeftMost :: [Lambda] -> Maybe [Lambda]
reduceLeftMost [] = Nothing
reduceLeftMost (t:ts) = case betaStep t of Just t' -> Just (t':ts)
                                           Nothing -> (t:) <$> reduceLeftMost ts

-- Normal reduction strategy, corresponding to lazy evaluation.
betaStep :: Lambda -> Maybe Lambda
betaStep (Ap ((L k m):n:ns)) = Just (sanitize $ Ap (result:ns))
  where result = down $ subst 0 n (sanitize $ L (k-1) m)
betaStep (Ap ts) = Ap <$> reduceLeftMost ts
betaStep (L k t) = L k <$> betaStep t
betaStep t       = Nothing -- nothing to reduce, t is a variable

-- Reduction to beta-normal form, if such exists
-- (otherwise, return the same term, for simplicity)
beta :: Lambda -> Lambda
beta t = case betaStep t of Nothing -> t
                            Just t' -> if t == t' then t else beta t'

-- Check for lambda term structure correctness
valid :: Lambda -> Bool
valid (Var i) = i >= 0
valid (Ap ts) = not (null ts) && not (null $ tail ts) && all valid ts
valid (L _ (L _ _)) = False
valid (L k t) = k > 0 && valid t

-- List every step of the beta reduction process
betaSteps :: Lambda -> [Lambda]
betaSteps t
  | res == Nothing = [t]
  | t == t'        = [t]
  | otherwise      = t : betaSteps t'
  where res = betaStep t
        (Just t') = res -- lazily evaluated

-- If all we need is to display the steps
betaSteps_ :: Lambda -> IO ()
betaSteps_ = mapM_ print . betaSteps
