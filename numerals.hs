module Numerals where
import Lambda (Lambda(..), beta, k, ks)
import qualified Lambda as D (pretty, valid, betaSteps_) -- for debugging purposes

-- Make a Church numeral from an integer (negative numbers are zeroed)
c :: Int -> Lambda
c n = L 2 (rep $ max n 0)
  where rep 0 = Var 0
        rep n = Ap [Var 1, rep (n-1)]

-- Extract the integer from a Church numeral. Returns a Maybe, because
-- not every lambda term is beta-equivalent to a Church numeral.
toInt :: Lambda -> Maybe Int
toInt t = case beta t of L 2 t' -> toInt' t'
                         _      -> Nothing
  where toInt' (Ap [Var 1, t]) = succ <$> toInt' t
        toInt' (Var 0)         = Just 0
        toInt' _               = Nothing

-- Successor
cs :: Lambda
cs = L 3 (Ap [Var 2, Var 1, Ap [Var 1, Var 0]])

-- Addition, multiplication & exponentiation
cPlus, cMult, cExpt :: Lambda
cPlus = L 4 (Ap [Var 3, Var 1, Ap [Var 2, Var 1, Var 0]])
cMult = L 4 (Ap [Var 3, Ap [Var 2, Var 1], Var 0])
cExpt = L 2 (Ap [Var 0, Var 1])

-- Haskell versions of the above combinators. Really helpful during manual testing.
cs' :: Lambda -> Lambda
cs' n = Ap [cs,n]
cPlus', cMult', cExpt' :: Lambda -> Lambda -> Lambda
cPlus' n m = Ap [cPlus,n,m]
cMult' n m = Ap [cMult,n,m]
cExpt' n m = Ap [cExpt,n,m]

-- Church booleans
cTrue, cFalse :: Lambda
cTrue  = k
cFalse = ks

-- Again, not every lambda term is beta-equivalent to a Church boolean.
toBool :: Lambda -> Maybe Bool
toBool t
  | t' == k   = Just True
  | t' == ks  = Just False
  | otherwise = Nothing
  where t' = beta t

-- Negation, disjunction & conjunction
cNot, cOr, cAnd :: Lambda
cNot = L 1 (Ap [Var 0, cFalse, cTrue])
cOr  = L 2 (Ap [Var 1, cTrue, Var 0])
cAnd = L 2 (Ap [Var 1, Var 0, cFalse])

-- More combinators as real-world-functions
cNot' :: Lambda -> Lambda
cNot' p = Ap [cNot,p]
cOr', cAnd' :: Lambda -> Lambda -> Lambda
cOr'  p q = Ap [cOr ,p,q]
cAnd' p q = Ap [cAnd,p,q]

-- Zero check
cz :: Lambda
cz = L 1 (Ap [Var 0, Ap [cTrue, cFalse], cTrue])
-- Zero-check as function
cz' :: Lambda -> Lambda
cz' n = Ap [cz,n]

-- Simple unit tests
tests :: Bool
tests = and [ toInt (cs' (c 2)) == Just 3
            , toInt (cPlus' (c 2) (c 3)) == Just 5
            , toInt (cMult' (c 2) (c 3)) == Just 6
            , toInt (cExpt' (c 2) (c 3)) == Just 8
            , toBool (cNot' cFalse) == Just True
            , toBool (cNot' cTrue)  == Just False
            , toBool (cOr'  cTrue cFalse) == Just True
            , toBool (cAnd' cTrue cFalse) == Just False
            , toBool (cz' (c 0)) == Just True
            , toBool (cz' (c 3)) == Just False
            ]
