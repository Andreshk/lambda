{-# LANGUAGE TupleSections #-}
module NLambda (NTerm(..), ps) where
import Control.Applicative ((<|>))

-- Named lambda term
data NTerm = NVar String       -- a variable with a name
           | NAp [NTerm]       -- application of multiple terms
           | NL [String] NTerm -- repeated abstraction
  deriving Show -- temporary

-- Parsing (and discarding) a single character
psChar :: Char -> String -> Maybe String
psChar _ "" = Nothing
psChar c (c':cs) = if c == c' then Just cs else Nothing

-- A name can only be one of x,y,z,u,v,w, perhaps followed by a number
psName :: String -> Maybe (String, String)
psName "" = Nothing
psName (c:cs)
  | c `elem` "uvwxyz" = Just (name, rest)
  | otherwise = Nothing
  where (num, rest) = span (`elem` ['0'..'9']) cs
        name = c:num

-- Parse a single variable
psVar :: String -> Maybe (NTerm, String)
psVar str = do
  (name, rest) <- psName str
  return $ (NVar name, rest)

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
psLam :: String -> Maybe (NTerm, String)
psLam str = do
  (names, rest) <- (psLambda str >>= (psChar '[') >>= psArgs)
  (t, rest) <- ((psChar ']' rest) >>= psTerm)
  return (NL names t, rest)

-- Brackets can be placed around each term, to indicate order of operations
psBr :: String -> Maybe (NTerm, String)
psBr str = do
  (t, rest) <- ((psChar '(' str) >>= psTerm)
  (t,) <$> psChar ')' rest

-- A "simple" term is either a variable, a lambda, or something in brackets
psSimple :: String -> Maybe (NTerm, String)
psSimple str = (psVar str)
           <|> (psLam str)
           <|> (psBr str)

-- Simple terms next to each other form a list
psList :: String -> Maybe ([NTerm], String)
psList str = do
  (fst, rest) <- psSimple str -- again, at least one
  case (psList rest)
    of Nothing    -> return ([fst], rest)
       Just (l,r) -> return (fst:l, r)

-- A lambda term is just a list of "simpler" terms. However, a list with
-- one term is just that term, otherwise it's application of multiple terms
psTerm :: String -> Maybe (NTerm, String)
psTerm str = do
  (ts, rest) <- psList str
  return (simplify ts, rest)
  where simplify [t] = t      -- a single term
        simplify ts  = NAp ts -- application of multiple terms

-- Parse a string, consisting only of a single lambda term
ps :: String -> Maybe NTerm
ps str = (psTerm str >>= psEof)
  where psEof (t, rest) = if null rest then Just t else Nothing
