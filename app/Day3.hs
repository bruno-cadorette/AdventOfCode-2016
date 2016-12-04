module Day3 where

import Data.List
import Data.List.Split

isValidTriangle = all (\[a,b,c] -> a + b > c) .  permutations
readInput = fmap (fmap read . filter (not . null) . splitOn " ") . lines 


solve f = length . filter isValidTriangle . f . readInput <$> readFile "day3.txt"

part1 = solve id
     
part2 = solve $ concatMap transpose . chunksOf 3
