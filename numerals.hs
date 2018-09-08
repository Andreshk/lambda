module Numerals where
import Lambda (Lambda(..), beta, k, ks)

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

cs :: Lambda -> Lambda
cs n = L 2 (Ap [n, Var 1, Ap [Var 1, Var 0]])

cPlus, cMult, cPow :: Lambda -> Lambda -> Lambda
cPlus n m = L 2 (Ap [n, Var 1, Ap [m, Var 1, Var 0]])
cMult n m = L 2 (Ap [n, Ap [m, Var 1], Var 0])
cPow n m = Ap [m, n]

cTrue, cFalse :: Lambda
cTrue  = k
cFalse = ks

toBool :: Lambda -> Maybe Bool
toBool t
  | t' == k   = Just True
  | t' == ks  = Just False
  | otherwise = Nothing
  where t' = beta t

cNot :: Lambda -> Lambda
cNot p = Ap [p, cFalse, cTrue]

cOr, cAnd :: Lambda -> Lambda -> Lambda
cOr p q = Ap [p, cTrue, q]
cAnd p q = Ap [p, q, cFalse]

cz :: Lambda -> Lambda
cz n = Ap [n, Ap [cTrue, cFalse], cTrue]
