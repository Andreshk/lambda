module Numerals where
import Lambda (Lambda(..), beta)

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

cPlus :: Lambda -> Lambda -> Lambda
cPlus n m = L 2 (Ap [n, Var 1, Ap [m, Var 1, Var 0]])

cMult :: Lambda -> Lambda -> Lambda
cMult n m = L 2 (Ap [n, Ap [m, Var 1], Var 0])
