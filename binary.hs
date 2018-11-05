module Binary where
import Control.Applicative ((<|>))
import Lambda --(Lambda(..))

-- ^n = 1^n+1 ++ 0
-- \M = 00 ++ M
-- MN = 01 M N

encode :: Lambda -> String
encode (Var n) = replicate (n+1) '1' ++ ['0']
encode (Ap ts) = foldl1 (\x y -> "01" ++ x ++ y) (map encode ts)
encode (L k t) = replicate (2*k) '0' ++ encode t

decVar :: String -> Maybe (Lambda, String)
decVar str
  | ones == 0 = Nothing
  | otherwise = Just (Var (ones-1), rest)
  where ones = length $ takeWhile (=='1') str
        rest = drop (ones+1) str

decL :: String -> Maybe (Lambda, String)
decL str
  | k == 0    = Nothing
  | otherwise = (decTerm body >>= \(t,rest) -> return (L k t, rest))
  where k = (`div`2) . length $ takeWhile (=='0') str
        body = drop (2*k) str

decAp :: String -> Maybe (Lambda, String)
decAp ('0':'1':str) = do
    (m,rest) <- decTerm str
    (n,rest) <- decTerm rest
    case m of Ap ts -> Just (Ap (ts++[n]),rest)
              _     -> Just (Ap [m,n], rest)
decAp _ = Nothing

decTerm :: String -> Maybe (Lambda, String)
decTerm str = (decVar str)
          <|> (decAp str)
          <|> (decL str)

decode :: String -> Maybe Lambda
decode str = (decTerm str >>= decEof)
  where decEof (t, rest) = if null rest then Just t else Nothing

testEncodeDecode :: Bool
testEncodeDecode = all (\t -> decode (encode t) == Just t) [i,k,ks,s,w]
