{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Crypto.Hash.MD5
import Data.List
import Data.Char
import Debug.Trace
import Data.ByteString.Base16
import Data.Foldable
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B


between n x m = n <= x && x <= m
key = "reyedfim"

isValid :: Int -> B.ByteString -> Bool
isValid n i = possibleResult == (B.take n i)
    where possibleResult = B.replicate n '0'
    
showB :: Show a => a -> B.ByteString
showB = B.pack . show

getHashString :: Show a => B.ByteString -> a -> B.ByteString
getHashString key = encode . hash . B.append key .showB 

validHashes = fmap (B.drop 5) $ filter (isValid 5) $ fmap (getHashString key) [0..]

toTuple str = 
    let (a:b:xs) = B.unpack str
    in (a,b)
    
updateList m ((i, x):xs) = 
    if M.keys m' == [0..7] then 
        M.elems m' 
    else
        updateList m' xs
    where m' = M.insertWith (\a b -> b) (digitToInt i) x m

part1 = take 8 $ fmap B.head $ validHashes
part2 = updateList mempty $ allDigits
    where
        allDigits = filter (\(a,b) -> isDigit a && between 0 (digitToInt a) 7) $ fmap toTuple $ validHashes 
        