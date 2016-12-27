module Day9 where

import Data.List
import Data.Bifunctor
import Data.Maybe
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Lexer (integer)

data Compress = Str Int | Marker Int Int Int deriving (Show)

compressLength (Str i) = i
compressLength (Marker _ _ i) = i

--Trouver ce que les x prochains charactere represente
--Si Str, n * x
--Sinon c'est une list de compress n fois

compressMarker = do
    char '('
    n <- fromInteger <$> integer
    char 'x'
    times <- fromInteger <$> integer
    char ')'
    return $ Marker n times (3 + (length $ show n ++ show times))

replica = do 
    (Marker n times _) <- compressMarker
    r <- count n asciiChar
    return $ length $ concat $ replicate times r

parseNormal :: Parser Int
parseNormal = do 
    a <- length <$> some upperChar 
    b <- option 0 replica 
    return $ a + b
    
part1 = parse (sum <$> some (parseNormal <|> replica)) "" <$>  readFile "day9.txt"
  
getNChar 0 xs = ([], xs)
getNChar n (x:xs)
    |n >= compressLength x = first (x :) $ getNChar (n - (compressLength x)) xs
    |otherwise = ([Str n], Str (compressLength x - n) : xs)
    
part2 = parse (decompressLength <$> some ((Str . length) <$> some upperChar <|> compressMarker)) "" <$> readFile "day9.txt"
decompressLength [] = 0
decompressLength (Str i : xs) = i + decompressLength xs
decompressLength (Marker n times l : xs) =
    let (ch, xs') = getNChar n xs
    in  times * (decompressLength ch) + decompressLength xs'