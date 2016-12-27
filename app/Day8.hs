module Day8 where

import Data.Array
import Data.List
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Lexer (integer)

data RotateWay = Row | Column deriving (Show, Eq)

data Op = Rect Integer Integer | Rotate RotateWay Integer Integer deriving (Show, Eq)

edges x y = ((0,0), (x, y)) 

toIndex Row i x = (x, i)
toIndex Column i x = (i, x)

maxIndex Row = fst
maxIndex Column = snd


rect a b = [((x,y), True)| x <- [0..(a-1)], y <- [0..(b-1)]]

newArray x y = listArray (edges x y) (repeat False)

update (Rect a b) arr = arr // rect a b
update (Rotate w i l) arr = arr // rotateUpdate w i l arr
        
rotateUpdate w i l arr =  zip indexes newValues
    where
        newValues = genericTake (m+1) $ genericDrop (m+1-l) $ cycle $ map (arr !) indexes
        indexes = map (toIndex w i) [0 .. m]
        m = maxIndex w $ snd $ bounds arr
rectangle :: Parser Op        
rectangle = do
    string "rect"
    space
    a <- integer
    char 'x'
    b <- integer
    return $ Rect a b

rotate :: Parser Op
rotate = do
    string "rotate"
    space
    dir <- (Column <$ string "column") <|> (Row <$ string "row")
    space
    char 'x' <|> char 'y'
    char '='
    a <- integer
    string " by "
    b <- integer
    return $ Rotate dir a b

getData f = fmap f . mapM (parse (rotate <|> rectangle) "") . lines 

solve f = getData f <$> readFile "day8.txt"

display arr = concatMap (\y -> (map (\x -> if arr ! (x, y) then '#' else '.') [0..x]) ++ "\n")  [0..y] 
    where
        (x,y) = snd $ bounds arr

countLighted = sum . fmap (\x -> if x then 1 else 0)

part1 = solve (countLighted . foldl (flip update) (newArray 49 5)) 
part2 = solve (display . foldl (flip update) (newArray 49 5)) 