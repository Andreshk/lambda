{-# LANGUAGE TupleSections #-}
module NLambda (NLambda(..), ps, bemolle) where
import Control.Applicative ((<|>))
import Data.List (elemIndex)
import Lambda (Lambda(..))

-- Named lambda term
data NLambda = NVar String         -- a variable with a name
             | NAp [NLambda]       -- application of multiple terms
             | NL [String] NLambda -- repeated abstraction
  deriving Show -- temporary

-- Parsing (and discarding) a single character
psChar :: Char -> String -> Maybe String
psChar _ "" = Nothing
psChar c (c':cs) = if c == c' then Just cs else Nothing

-- A name can only be one of x,y,z,u,v,w, perhaps followed by a number
psName :: String -> Maybe (String, String)
psName "" = Nothing
psName (c:cs)
  | c `elem` "uvwxyz" = Just (c:num, rest)
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
psArgs :: String -> Maybe ([String], String)
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

-- Parse a string, consisting only of a single lambda term
ps :: String -> Maybe NLambda
ps str = (psTerm str >>= psEof)
  where psEof (t, rest) = if null rest then Just t else Nothing

-- bemolle (♭): Named to nameless conversion. Invariant: n == length ctx
bemolle :: NLambda -> Lambda
bemolle = bemolle' [] 0
  where bemolle' ctx n (NVar v)  = Var $ find v ctx
        bemolle' ctx n (NAp ts)  = Ap $ map (bemolle' ctx n) ts
        bemolle' ctx n (NL ns t) = L k (bemolle' ctx' (n+k) t)
          where k = length ns
                ctx' = (reverse ns) ++ ctx
        find n ns = let (Just idx) = elemIndex n ns in idx
