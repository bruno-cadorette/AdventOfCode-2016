module Day7 where


import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Set as Set
import Data.Bifunctor
import Data.Either

mapBoth f = bimap f f

containsMatching :: Eq a => [a] -> [a] -> Bool
containsMatching [] _ = False
containsMatching (x:xs) ys
    |elem x ys = True
    |otherwise = containsMatching xs ys
            


pairBy4With f (a:b:c:d:xs) = f a b c d : pairBy4With f (b:c:d:xs)
pairBy4With f xs = []

pairBy3With f (a:b:c:xs) = f a b c : pairBy3With f (b:c:xs)
pairBy3With f xs = []

seqSupportTLS :: String -> Bool
seqSupportTLS = or . pairBy4With (\a b c d -> (a, b) == (d, c) && (a,b) /= (c, d)) 

keepAba :: Eq a => [(a,a,a)] -> [(a,a,a)]
keepAba = filter (\(a,b,c) -> a == c && a /= b)

toBab = map (\(a, b, c) -> (b, a, b))

parseIp = parse (some (fmap Left innerParse <|> fmap Right outer)) ""

solve f = do
    t <- readFile "day7.txt"
    return $ fmap (length . filter f) $ mapM (parseIp) $ lines t
innerParse :: Parser String 
innerParse = between (char '[') (char ']') (some lowerChar)
outer = some lowerChar
isTLS xs =
    let (i, o) = partitionEithers xs
    in all (not . seqSupportTLS) i && any seqSupportTLS o
    

isSSL xs = 
    let (i, o) = mapBoth (concatMap (keepAba . pairBy3With (,,))) $ partitionEithers xs
    in containsMatching (toBab i) o
  
part1 = solve isTLS
part2 = solve isSSL