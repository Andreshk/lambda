module Numerals where
import Lambda (Lambda(..), beta, k, ks)
import qualified Lambda as D (pretty, valid, betaSteps_) -- for debugging purposes

c :: Int -> Lambda
c n = L 2 (rep $ max n 0)
  where rep 0 = Var 0
        rep n = Ap [Var 1, rep (n-1)]

toInt :: Lambda -> Maybe Int
toInt t = case beta t of L 2 t' -> toInt' t'
                         _      -> Nothing
  where toInt' (Ap [Var 1, t]) = succ <$> toInt' t
        toInt' (Var 0)         = Just 0
        toInt' _               = Nothing

cs :: Lambda
cs = L 3 (Ap [Var 2, Var 1, Ap [Var 1, Var 0]])

cPlus, cMult, cPow :: Lambda
cPlus = L 4 (Ap [Var 3, Var 1, Ap [Var 2, Var 1, Var 0]])
cMult = L 4 (Ap [Var 3, Ap [Var 2, Var 1], Var 0])
cPow  = L 2 (Ap [Var 0, Var 1])

cs' :: Lambda -> Lambda
cs' n = Ap [cs,n]
cPlus', cMult', cPow' :: Lambda -> Lambda -> Lambda
cPlus' n m = Ap [cPlus,n,m]
cMult' n m = Ap [cMult,n,m]
cPow'  n m = Ap [cPow ,n,m]

cTrue, cFalse :: Lambda
cTrue  = k
cFalse = ks

toBool :: Lambda -> Maybe Bool
toBool t
  | t' == k   = Just True
  | t' == ks  = Just False
  | otherwise = Nothing
  where t' = beta t

cNot, cOr, cAnd :: Lambda
cNot = L 1 (Ap [Var 0, cFalse, cTrue])
cOr  = L 2 (Ap [Var 1, cTrue, Var 0])
cAnd = L 2 (Ap [Var 1, Var 0, cFalse])

cNot' :: Lambda -> Lambda
cNot' p = Ap [cNot,p]
cOr', cAnd' :: Lambda -> Lambda -> Lambda
cOr'  p q = Ap [cOr ,p,q]
cAnd' p q = Ap [cAnd,p,q]

cz :: Lambda
cz = L 1 (Ap [Var 0, Ap [cTrue, cFalse], cTrue])

cz' :: Lambda -> Lambda
cz' n = Ap [cz,n]

tests :: Bool
tests = and [ toInt (cs' (c 2)) == Just 3
            , toInt (cPlus' (c 2) (c 3)) == Just 5
            , toInt (cMult' (c 2) (c 3)) == Just 6
            , toInt (cPow'  (c 2) (c 3)) == Just 8
            , toBool (cNot' cFalse) == Just True
            , toBool (cNot' cTrue)  == Just False
            , toBool (cOr'  cTrue cFalse) == Just True
            , toBool (cAnd' cTrue cFalse) == Just False
            ]
