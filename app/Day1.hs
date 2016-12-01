module Day1 where
import Data.List
import Data.Bifunctor
data Direction = North | East | South | West deriving (Eq, Show, Enum)
data TurnInput  = R Int | L Int

inputToDirection t d = replicate (turnDistance t) $ turn t d

turnDistance (R i) = i
turnDistance (L i) = i

type Coord = (Int, Int)

turn (R _) West = North
turn (R _) e = succ e
turn (L _) North = West
turn (L _) e = pred e

move :: Direction -> Coord -> Coord
move North = first ((+) 1)
move East = second ((+) 1)
move South = first (\a -> a - 1)
move West = second (\a -> a - 1)

getDistance (a, b) = abs a + abs b

firstTwice [] = Nothing
firstTwice (x:xs)
    | elem x xs = Just x
    | otherwise = firstTwice xs

pathInput = [R 3, L 5, R 1, R 2, L 5, R 2, R 3, L 2, L 5, R 5, L 4, L 3, R 5, L 1, R 3, R 4, R 1, L 3, R 3, L 2, L 5, L 2, R 4, R 5, R 5, L 4, L 3, L 3, R 4, R 4, R 5, L 5, L 3, R 2, R 2, L 3, L 4, L 5, R 1, R 3, L 3, R 2, L 3, R 5, L 194, L 2, L 5, R 2, R 1, R 1, L 1, L 5, L 4, R 4, R 2, R 2, L 4, L 1, R 2, R 53, R 3, L 5, R 72, R 2, L 5, R 3, L 4, R 187, L 4, L 5, L 2, R 1, R 3, R 5, L 4, L 4, R 2, R 5, L 5, L 4, L 3, R 5, L 2, R 1, R 1, R 4, L 1, R 2, L 3, R 5, L 4, R 2, L 3, R 1, L 4, R 4, L 1, L 2, R 3, L 1, L 1, R 4, R 3, L 4, R 2, R 5, L 2, L 3, L 3, L 1, R 3, R 5, R 2, R 3, R 1, R 2, L 1, L 4, L 5, L 2, R 4, R 5, L 2, R 4, R 4, L 3, R 2, R 1, L 4, R 3, L 3, L 4, L 3, L 1, R 3, L 2, R 2, L 4, L 4, L 5, R 3, R 5, R 3, L 2, R 5, L 2, L 1, L 5, L 1, R 2, R 4, L 5, R 2, L 4, L 5, L 4, L 5, L 2, L 5, L 4, R 5, R 3, R 2, R 2, L 3, R 3, L 2, L 5]

path = concat $ snd  $ mapAccumL (\t d -> let xs = inputToDirection d t in (head xs, xs)) North pathInput

computePath = scanl (flip move) (0,0) path
        
part1 = getDistance (last computePath)

part2 = getDistance <$> firstTwice computePath
        