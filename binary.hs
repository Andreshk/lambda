module Binary (encode,decode) where
import Control.Applicative ((<|>))
import Data.BitVector (BitVector, ones, zeros, (#), fromBits, size,
                       msb1, complement, least, (!.), (==.), nil)
import Lambda (Lambda(..),i,k,ks,s,w,om,y,j)

-- A variable with DeBruijn index n is encoded to n+1 ones and a zero
-- (10,110,1...10). Abstraction is encoded to 00, and application to 01.
encode :: Lambda -> BitVector
encode (Var n) = ones (n+1) # zeros 1
encode (Ap ts) = foldl1 (\x y -> (fromBits [False,True]) # x # y) (map encode ts)
encode (L k t) = zeros (2*k) # encode t

-- Get the number of leading bits of the corresponding type
leading :: Char -> BitVector -> Int
leading '0' bv = (size bv - msb1 bv - 1)
leading '1' bv = (size bv - msb1 bv' - 1)
  where bv' = complement bv -- to-do: don't flip the entire bitvector

-- Drop the most-significant k bits from a bitvector
dropBV :: Int -> BitVector -> BitVector
dropBV k bv
  | rest <= 0 = nil
  | otherwise = least rest bv
  where rest = (size bv - k)

-- Decodes (if possible) a variable, and returns it with the rest of the encoded bitvector
decVar :: BitVector -> Maybe (Lambda, BitVector)
decVar bv
  | ones == 0 = Nothing
  | otherwise = Just (Var (ones-1), rest)
  where ones = leading '1' bv
        rest = dropBV (ones+1) bv

-- Decodes a repeated abstraction
decL :: BitVector -> Maybe (Lambda, BitVector)
decL bv
  | k == 0    = Nothing
  | otherwise = do (t,rest) <- decTerm body; return (L k t, rest)
  where k = (leading '0' bv) `div` 2
        body = dropBV (2*k) bv

-- Decodes an application, and compresses the returned term (if possible)
decAp :: BitVector -> Maybe (Lambda, BitVector)
decAp bv
  | prefixedBy01 = decAp' $ dropBV 2 bv
  | otherwise    = Nothing
  where prefixedBy01 = size bv >= 2 && not (bv !. 0) && bv !. 1
        decAp' bv = do
          (m,temp) <- decTerm bv
          (n,rest) <- decTerm temp
          case m of Ap ts -> Just (Ap (ts++[n]),rest)
                    _     -> Just (Ap [m,n], rest)

-- Decoding a term in this system is straight-forward
decTerm :: BitVector -> Maybe (Lambda, BitVector)
decTerm bv = (decVar bv)
         <|> (decAp bv)
         <|> (decL bv)

-- The user-facing decoding function needs to decode the entire
-- given bitvector, not just some prefix (with garbage afterwards).
decode :: BitVector -> Maybe Lambda
decode str = do
  (t,rest) <- decTerm str
  if rest ==. nil then Just t else Nothing

-- Check whether decoding an encoded term gives the same term
checkTerm :: Lambda -> Bool
checkTerm t = decode (encode t) == Just t

-- If the most common combinators are encoded/decoded
-- correctly, what else could go wrong?
testEncodeDecode :: Bool
testEncodeDecode = all checkTerm [i,k,ks,s,w,om,y,j]
