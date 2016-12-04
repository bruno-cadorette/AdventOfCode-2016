module Day4 where

import Text.Megaparsec
import Data.List
import Data.Maybe
import Data.Either
import Data.Char
import Data.Ord
import Debug.Trace
import qualified Data.Map as Map

data Room = Room { name :: [String], sector :: Int, checksum :: String } deriving (Show)

getLetterIndex x = ord x - ord 'a' 

decrypt room = room { name = newName }
    where 
        newName = fmap (fmap computeNewIndex) $ name room
        computeNewIndex x =['a'..'z'] !! ((sector room + getLetterIndex x) `mod` 26)

checkRoomName _ [] = True
checkRoomName map (x:xs) = fromMaybe False $ do  
    maxValue <- pure $ snd $ head map
    actualValue <- lookup x map
    return $ if actualValue == maxValue then
        checkRoomName (delete (x, actualValue) map) xs
    else
        False
    
roomParser :: Parsec Dec String Room
roomParser = do 
    (codes, sectorId) <- (\a -> (init a, read $ last a)) <$> sepBy (some alphaNumChar) (char '-')
    checksum <- between (char '[') (char ']') (some lowerChar)
    return $ Room codes sectorId checksum
    
    
commonMap (Room n _ check) = sortBy (comparing  (Down . snd))  $ fmap (\x -> (head x, length x)) $ group $ sort $ concat n
    
isReal room = checkRoomName (commonMap room) (checksum room)
        
        
       
readInput = rights . fmap (parse roomParser "") . lines 

solve f =  (f . filter isReal) . readInput <$> readFile "day4.txt"

part1 = solve (sum . fmap sector)
part2 = solve (filter (isInfixOf "north". concat . intersperse " " . name) . fmap decrypt)
