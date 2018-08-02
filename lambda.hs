{-# LANGUAGE CPP #-} -- C preprocessor for OS detection with ifdef
import Data.List (sort,nub,foldl1,intercalate)
import Data.Functor ((<$>))

gamma :: [Int] -> [Int]
gamma l = makeSet $ [0,1,2] ++ [ x+y | x<-l, y<-l, x>y, x<=2*y ]
  where makeSet = sort . nub

data Lambda = Var Int
            | Ap Lambda Lambda
            | L Lambda
  deriving Eq -- alpha-equivalence is syntactic equivalence

-- Short-hand for applying a term on a list of arguments by listing
-- all terms successively: ap' [x,y,z] = (Ap (Ap x y) z) <=> xyz
ap' :: [Lambda] -> Lambda
ap' = foldl1 Ap

-- Generate a human-friendly variable name from an integer: u,v,w,x,y,z,u1,v1,w1,...
name :: Int -> String
name n = ("uvwxyz"!!(rem n 6)) : (if n < 6 then "" else (show (div n 6)))

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
  show = octoth [] 0
    where octoth :: [String] -> Int -> Lambda -> String -- from "octothorpe" (#)
          octoth ctx n (Var i)    = (if i < n then ctx!!i else name i)
          octoth ctx n t@(Ap _ _) = concatMap (braceOctoth ctx n) $ gatherAps t
          octoth ctx n t          = lambda ++ "[" ++ (intercalate "," names) ++ "]" ++ octoth ctx' n' t'
            where (names, ctx', n', t') = gatherLambdas [] ctx n t
          -- If there happen to be multiple consecutive applications of a single term,
          -- this makes a list of the term and its arguments for pretty-printing later:
          -- gatherAps (Ap (Ap (Ap x y) z) w) -> [x,y,z,w] -> "xyzw" instead of "(((x)(y))(z))(w)"
          gatherAps (Ap m n) = gatherAps m ++ [n]
          gatherAps t = [t]
          -- Analogously, on multiple consecutive abstractions, f.e. (L (L ...)), gatherLambdas walks
          -- down the tree and in one iteration gathers the names of the successively bound variables.
          -- This results in "lambda[x,y,z]..." instead of "lambda[x]lambda[y]lambda[z]..."
          gatherLambdas names ctx n (L m) = let newName = name n
                                            in gatherLambdas (names ++ [newName]) (newName:ctx) (n+1) m
          gatherLambdas names ctx n t = (names, ctx, n, t)
          -- braceOctoth does the same as octoth, except put braces around complex terms (non-variables)
          braceOctoth ctx n t@(Var _) = octoth ctx n t
          braceOctoth ctx n t = "(" ++ octoth ctx n t ++ ")"


-- Classic, naive reference implementation
show' :: Lambda -> String
show' = octoth' []
  where octoth' :: [String] -> Lambda -> String
        octoth' ctx (Var i)  = (if i < length ctx then ctx!!i else name i)
        octoth' ctx (Ap m n) = "(" ++ octoth' ctx m ++ ")(" ++ octoth' ctx n ++ ")"
        octoth' ctx (L m)    = "lambda" ++ "[" ++ newName ++ "]"++ octoth' (newName:ctx) m -- no λ :(
          where newName = name $ length ctx

-- Some classic combinators.
i, k, ks, s, w, om, y, j :: Lambda
i  = L (Var 0)     -- I
k  = L (L (Var 1)) -- K
ks = L (L (Var 0)) -- K*
s  = L (L (L (Ap (Ap (Var 2) (Var 0)) (Ap (Var 1) (Var 0))))) -- S
w  = L (Ap (Var 0) (Var 0))  -- omega 
om = Ap w w -- the irreducible Omega term
y  = L (Ap (L (Ap (Var 1) (Ap (Var 0) (Var 0)))) (L (Ap (Var 1) (Ap (Var 0) (Var 0))))) -- fixed-point Y-combinator
j  = L (Ap (Ap (Var 0) s) k) -- Chris Barker's iota combinator: jj = I, j(jj) = K*, j(j(jj)) = K, j(j(j(jj))) = S
        
-- Increase or decrease all free variables with k, knowing their indices are >= d
arrow :: Int -> Int -> Lambda -> Lambda
arrow k d (Var i)  = Var $ if i >= d then i+k else i
arrow k d (Ap m n) = Ap (arrow k d m) (arrow k d n)
arrow k d (L m)    = L (arrow k (d+1) m) -- with a new bound variable all other indices increase by 1

-- Bump all free variables with 1, knowing their indices start from 0
up :: Lambda -> Lambda
up = arrow 1 0

-- Reduce all free variables by 1, knowing their indices have been bumped to >=1
down :: Lambda -> Lambda
down = arrow (-1) 1

-- Nameless lambda substitution
subst :: Int -> Lambda -> Lambda -> Lambda
subst i n (Var j)    = if i == j then n else (Var j)
subst i n (Ap m1 m2) = Ap (subst i n m1) (subst i n m2)
subst i n (L m)      = L (subst (i+1) (up n) m)

-- Substitution examples:
l1, l2 :: Lambda
l1 = L (Ap (Var 0) (L (Ap (Ap (Var 1) (Var 0)) (Var 3)))) -- lambda[x]x(lambda[y]xyz)
l2 = L (Ap (Var 0) (Var 1))                               -- lambda[u]uv
-- subst 1 l2 l1 -> lambda[x]x(lambda[y]xy(lambda[u]uv)) or something alpha-equivalent to it :)

-- Normal reduction strategy, corresponding to lazy evaluation.
betaStep :: Lambda -> Maybe Lambda
betaStep (Ap (L m) n) = Just (down $ subst 0 n m) -- upper-most,
betaStep (Ap m n)     = case betaStep m of Just m' -> Just $ Ap m' n -- left-most redex
                                           Nothing -> Ap m <$> betaStep n
betaStep (L m)        = L <$> betaStep m
betaStep t            = Nothing -- nothing to reduce, t is a variable

-- Reduction to beta-normal form, if such exists
beta :: Lambda -> Lambda
beta t = case betaStep t of Nothing -> t
                            Just t' -> beta t'
