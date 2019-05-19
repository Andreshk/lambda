module Numerals where
import Control.Applicative (liftA2)
import Lambda (Lambda(..), beta, (=~=), k, ks)

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

-- Boolean combinators as real-world-functions
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

-- Parity checks
cEven, cOdd :: Lambda
cEven = L 1 (Ap [Var 0, cNot, cTrue])
cOdd  = L 1 (Ap [cNot, Ap [cEven, Var 0]])
-- Parity checks as functions
cEven', cOdd' :: Lambda -> Lambda
cEven' n = Ap [cEven,n]
cOdd'  n = Ap [cOdd ,n]

-- Constructing a pair & accessing its two components
cCons, cHead, cTail :: Lambda
cCons = L 3 (Ap [Var 0, Var 2, Var 1])
cHead = L 1 (Ap [Var 0, cTrue])
cTail = L 1 (Ap [Var 0, cFalse])

-- Pair operations as functions
cCons' :: Lambda -> Lambda -> Lambda
cCons' p q = Ap [cCons,p,q]
cHead', cTail' :: Lambda -> Lambda
cHead' z = Ap [cHead,z]
cTail' z = Ap [cTail,z]

-- Extract a Haskell integer pair from a lambda
-- term, representing a pair of integers.
toIntPair :: Lambda -> Maybe (Int,Int)
toIntPair z = case beta z of L 1 (Ap [Var 0,p,q]) -> liftA2 (,) (toInt p) (toInt q)
                             _                    -> Nothing

-- Decreasing a positive integer by one (zero is left unchanged)
cPrev :: Lambda
cPrev = L 1 (Ap [cTail, Ap [Var 0, nextPair, Ap [cCons,c 0,c 0]]])
  where nextPair = L 1 (Ap [cCons, Ap [cs, Ap[cHead, Var 0]],
                                           Ap[cHead, Var 0]])
cPrev' :: Lambda -> Lambda
cPrev' n = Ap [cPrev,n]

-- Church numerals subtraction. Again, returns 0 instead of a negative result.
cMinus :: Lambda
cMinus = L 2 (Ap [Var 0, L 1 (Ap [cPrev, Var 0]), Var 1])
cMinus' :: Lambda -> Lambda -> Lambda
cMinus' n m = Ap [cMinus,n,m]

-- Finally, an equality check
cEqual :: Lambda
cEqual = L 2 (Ap [cAnd, Ap [cz, Ap [cMinus, Var 0, Var 1]],
                        Ap [cz, Ap [cMinus, Var 1, Var 0]]])
cEqual' :: Lambda -> Lambda -> Lambda
cEqual' n m = Ap [cEqual,n,m]

-- If you have one comparison operator, you have them all.
-- The remaining 5 are left as an exercise to the Reader.
cLess :: Lambda
cLess = L 2 (Ap [cz, Ap [cMinus, Ap[cs, Var 1], Var 0]])
cLess' :: Lambda -> Lambda -> Lambda
cLess' n m = Ap [cLess,n,m]

-- Factorial! Becomes slow after n = 4, though...
cFact :: Lambda
cFact = L 1 (Ap [cTail, Ap [Var 0, nextPair, Ap [cCons,c 0,c 1]]])
  where -- next pair = \p -> let h = fst p; t = snd t in letBody h t
        nextPair = L 1 (Ap [letBody, Ap [cHead,Var 0], Ap [cTail,Var 0]])
        letBody  = L 2 (Ap [cCons, Ap [cs, Var 1],
                                   Ap [cMult, Ap [cs, Var 1], Var 0]])
cFact' :: Lambda -> Lambda
cFact' n = Ap [cFact,n]

-- Integral division by 2
cDiv2 :: Lambda
cDiv2 = L 1 (Ap [cTail, Ap [Var 0, nextPair, Ap [cCons,c 0,c 0]]])
  where -- nextPair = \p -> let s = succ (fst p); q = snd p in letBody s q
        nextPair = L 1 (Ap [letBody, Ap [cs, Ap [cHead,Var 0]], Ap [cTail,Var 0]])
        letBody  = L 2 (Ap [cCons, Var 1, Ap [cEven, Var 1, Ap [cs,Var 0], Var 0]])
cDiv2' :: Lambda -> Lambda
cDiv2' n = Ap [cDiv2,n]

-- Square root (rounded down to the nearest integer)
-- Takes a noticable time even for n = 4 or 5
cSqrt :: Lambda
cSqrt = L 1 (Ap [cPrev, Ap [cTail, Ap [Var 0, nextPair, Ap [cCons,Var 0,c 1]]]])
  where -- this let-construction is basically pattern-matching a pair's components
        nextPair = L 1 (Ap [letBody, Ap [cHead,Var 0], Ap [cTail,Var 0]])
        letBody  = L 2 (Ap [cLess, Var 1, Ap [cMult,Var 0,Var 0],
                            Ap [cCons, Var 1, Var 0],
                            Ap [cCons, Var 1, Ap [cs,Var 0]]])
cSqrt' :: Lambda -> Lambda
cSqrt' n = Ap [cSqrt,n]

-- Integral modulo
cMod :: Lambda
cMod = L 2 (Ap [cHead, Ap [Var 0, nextPair, Ap [cCons, Var 1, Var 0]]])
  where nextPair = L 1 (Ap [letBody, Ap [cHead, Var 0], Ap [cTail, Var 0]])
        letBody  = L 2 (Ap [cLess, Var 1, Var 0,
                            Ap [cCons, Var 1, Var 0],
                            Ap [cCons, Ap [cMinus, Var 1, Var 0], Var 0]])
cMod' :: Lambda -> Lambda -> Lambda
cMod' m n = Ap [cMod,m,n]

-- Fibonacci sequence
cFib :: Lambda
cFib = L 1 (Ap [cHead, Ap [Var 0, nextPair, Ap [cCons, c 0, c 1]]])
  where nextPair = L 1 (Ap [letBody, Ap [cHead, Var 0], Ap [cTail, Var 0]])
        letBody  = L 2 (Ap [cCons, Var 0, Ap [cPlus, Var 0, Var 1]])
cFib' :: Lambda -> Lambda
cFib' n = Ap [cFib,n]

-- Simple unit tests
tests :: Bool
tests = and [ cs' (c 2) =~= (c 3)
            , cPlus' (c 2) (c 3) =~= (c 5)
            , cMult' (c 2) (c 3) =~= (c 6)
            , cExpt' (c 2) (c 3) =~= (c 8)
            , cNot' cFalse =~= cTrue
            , cNot' cTrue  =~= cFalse
            , cOr'  cTrue cFalse =~= cTrue
            , cAnd' cTrue cFalse =~= cFalse
            , cz' (c 0) =~= cTrue
            , cz' (c 3) =~= cFalse
            , cEven' (c 2) =~= cTrue
            , cOdd'  (c 4) =~= cFalse
            , cHead' (cCons' (c 2) (c 3)) =~= (c 2)
            , cTail' (cCons' (c 2) (c 3)) =~= (c 3)
            , cPrev' (c 3) =~= (c 2)
            , cPrev' (c 0) =~= (c 0)
            , cMinus' (c 5) (c 3) =~= (c 2)
            , cMinus' (c 2) (c 3) =~= (c 0)
            , cEqual' (c 2) (c 3) =~= cFalse
            , cEqual' (c 5) (c 5) =~= cTrue
            , cLess'  (c 2) (c 3) =~= cTrue
            , cLess'  (c 3) (c 3) =~= cFalse
            , cLess'  (c 5) (c 3) =~= cFalse
            , cFact' (c 3) =~= (c 6)
            , cFact' (c 4) =~= (c 24)
            , cDiv2' (c 8) =~= (c 4)
            , cDiv2' (c 7) =~= (c 3)
            , cDiv2' (c 0) =~= (c 0)
            , cSqrt' (c 0) =~= (c 0)
            , cSqrt' (c 3) =~= (c 1)
            , cSqrt' (c 4) =~= (c 2)
            , cMod' (c 5) (c 2) =~= (c 1)
            , cMod' (c 4) (c 2) =~= (c 0)
            , cFib' (c 0) =~= (c 0)
            , cFib' (c 1) =~= (c 1)
            , cFib' (c 5) =~= (c 5)
            ]
