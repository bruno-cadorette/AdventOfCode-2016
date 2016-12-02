{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Data.Bifunctor
import Control.Monad

data Direction = U | D | L | R deriving (Read, Show)

class Keypad a where
    isValid :: a -> Bool
    moveFinger :: a -> Direction -> a
 
newtype P1 = P1 (Int, Int) deriving (Show)
newtype P2 = P2 (Int, Int) deriving (Show)

moveFingerImpl a D = second ((+)1) a
moveFingerImpl a U = second (subtract 1) a
moveFingerImpl a L = first (subtract 1) a 
moveFingerImpl a R = first ((+)1) a 

included n x m = n <= x && x <= m

p2Square = do
    t <- liftM2 (,) [0..4] [-2..2]
    guard $ notElem t [(0, 1), (0, 2), (1, 2), (3, 2), (4, 2), (4, 1), (0, -1), (0, -2), (1, -2), (3,-2), (4, -2), (4,-1)]
    return t
    
instance Keypad P1 where
    isValid (P1 (x, y)) = verify x && verify y
        where verify v = included (-1) v 1
    moveFinger (P1 x) = P1 . moveFingerImpl x
    
instance Keypad P2 where
    isValid (P2 v) = elem v p2Square
    moveFinger (P2 x) = P2 . moveFingerImpl x
    
move :: Keypad a => a -> Direction -> a
move b d = 
    if isValid b' then b' else b
    where b' = moveFinger b d
    

readProblem :: String -> [[Direction]]
readProblem = fmap (fmap (\x -> read [x])) . lines

findLineCode :: Keypad a => a -> [Direction] -> a
findLineCode b xs = foldl move b xs

part1 = solve $ P1 (0,0)
part2 = solve $ P2 (0,0)

solve base = drop 1 . scanl findLineCode base . readProblem <$> readFile "day2.txt"
