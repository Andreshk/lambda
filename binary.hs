module Binary (encode,decode) where
import Control.Applicative ((<|>))
import Lambda (Lambda(..),i,k,ks,s,w,om,y,j)

-- A variable with DeBruijn index n is encoded to n+1 ones and a zero
-- (10,110,1...10). Abstraction is encoded to 00, and application to 01.
encode :: Lambda -> String
encode (Var n) = replicate (n+1) '1' ++ ['0']
encode (Ap ts) = foldl1 (\x y -> "01" ++ x ++ y) (map encode ts)
encode (L k t) = replicate (2*k) '0' ++ encode t

-- Decodes (if possible) a variable, and returns it with the rest of the encoded string
decVar :: String -> Maybe (Lambda, String)
decVar str
  | ones == 0 = Nothing
  | otherwise = Just (Var (ones-1), rest)
  where ones = length $ takeWhile (=='1') str
        rest = drop (ones+1) str

-- Decodes a repeated abstraction
decL :: String -> Maybe (Lambda, String)
decL str
  | k == 0    = Nothing
  | otherwise = do (t,rest) <- decTerm body; return (L k t, rest)
  where k = (`div`2) . length $ takeWhile (=='0') str
        body = drop (2*k) str

-- Decodes an application, and compresses the returned term (if possible)
decAp :: String -> Maybe (Lambda, String)
decAp ('0':'1':str) = do
    (m,temp) <- decTerm str
    (n,rest) <- decTerm temp
    case m of Ap ts -> Just (Ap (ts++[n]),rest)
              _     -> Just (Ap [m,n], rest)
decAp _ = Nothing

-- Decoding a term in this system is straight-forward
decTerm :: String -> Maybe (Lambda, String)
decTerm str = (decVar str)
          <|> (decAp str)
          <|> (decL str)

-- The user-facing decoding function needs to decode the entire
-- given string, not just some prefix (with garbage afterwards).
decode :: String -> Maybe Lambda
decode str = do (t,"") <- decTerm str; return t

-- Check whether decoding an encoded term gives the same term
checkTerm :: Lambda -> Bool
checkTerm t = decode (encode t) == Just t

-- If the most common combinators are encoded/decoded
-- correctly, what else could go wrong?
testEncodeDecode :: Bool
testEncodeDecode = all checkTerm [i,k,ks,s,w,om,y,j]
