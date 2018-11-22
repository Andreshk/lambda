{-# LANGUAGE TupleSections #-}
module NLambda (ps) where
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.List (elemIndex, sort, nub, (\\))
import Lambda (Lambda(..))

newtype Name = Name String deriving (Eq, Ord)
instance Show Name where show (Name n) = show n

-- Named lambda term
data NLambda = NVar Name         -- a variable with a name
             | NAp [NLambda]     -- application of multiple terms
             | NL [Name] NLambda -- repeated abstraction
  deriving Show -- temporary

-- Parsing (and discarding) a single character
psChar :: Char -> String -> Maybe String
psChar _ "" = Nothing
psChar c (c':cs) = if c == c' then Just cs else Nothing

-- A name can only be one of x,y,z,u,v,w, perhaps followed by a number
psName :: String -> Maybe (Name, String)
psName "" = Nothing
psName (c:cs)
  | c `elem` "uvwxyz" = Just (Name (c:num), rest)
  | otherwise         = Nothing
  where (num, rest) = span (`elem` ['0'..'9']) cs

-- Parse a single variable
psVar :: String -> Maybe (NLambda, String)
psVar str = do
  (name, rest) <- psName str
  return (NVar name, rest)

-- Parse and discard the literal "lambda"
psLambda :: String -> Maybe String
psLambda str = if "lambda" == take 6 str then Just $ drop 6 str else Nothing

-- Parse a list of comma-separated names
psArgs :: String -> Maybe ([Name], String)
psArgs str = do
  (n, rest) <- psName str -- at least one name
  case ((psChar ',' rest) >>= psArgs)
    of Just (ns, rest') -> return (n:ns, rest')
       Nothing          -> return ([n], rest)

-- Parse a "lambda[..].." expression in pieces
psLam :: String -> Maybe (NLambda, String)
psLam str = do
  (names, rest) <- (psLambda str >>= (psChar '[') >>= psArgs)
  (t, rest) <- ((psChar ']' rest) >>= psTerm)
  return (compress names t, rest)
  where compress ns (NL ns' t') = NL (ns++ns') t' -- take care of "lambda[..]lambda[..].."
        compress ns t = NL ns t

-- Brackets can be placed around each term, to indicate order of operations
psBr :: String -> Maybe (NLambda, String)
psBr str = do
  (t, rest) <- ((psChar '(' str) >>= psTerm)
  (t,) <$> psChar ')' rest

-- A "simple" term is either a variable, a lambda, or something in brackets
psSimple :: String -> Maybe (NLambda, String)
psSimple str = (psVar str)
           <|> (psLam str)
           <|> (psBr str)

-- Simple terms next to each other form a list
psList :: String -> Maybe ([NLambda], String)
psList str = do
  (fst, rest) <- psSimple str -- again, at least one
  case (psList rest)
    of Just (ts, rest) -> return (fst:ts, rest)
       Nothing         -> return ([fst], rest)

-- A lambda term is just a list of "simpler" terms. However, a list with
-- one term is just that term, otherwise it's application of multiple terms
psTerm :: String -> Maybe (NLambda, String)
psTerm str = do
  (ts, rest) <- psList str
  return (simplify ts, rest)
  where simplify [t] = t -- a single term
        simplify ((NAp ts):ts') = NAp (ts++ts') -- compressed application of multiple terms
        simplify ts = NAp ts

-- Parse a string, consisting only of a single lambda term,
-- and immediately convert to an unnamed lambda term.
ps :: String -> Maybe Lambda
ps str = do
  (t, "") <- psTerm str
  return (bemolle t)

-- Free variables of a term, sorted for convenience.
-- There may be a more efficient implementation.
fv :: NLambda -> [Name]
fv (NVar n)  = [n]
fv (NAp ts)  = sort . nub $ concatMap fv ts
fv (NL ns t) = (fv t) \\ ns

-- bemolle (♭): Named to nameless conversion.
-- The initial context is the set of the free variables in the term.
bemolle :: NLambda -> Lambda
bemolle t = bemolle' (fv t) t
  where bemolle' ctx (NVar n)  = Var (find n ctx)
        bemolle' ctx (NAp ts)  = Ap (bemolle' ctx <$> ts)
        bemolle' ctx (NL ns t) = L k (bemolle' ctx' t)
          where k = length ns
                ctx' = (reverse ns) ++ ctx
        find n ns = fromJust $ elemIndex n ns
