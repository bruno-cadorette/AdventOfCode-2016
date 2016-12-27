module Day6 where
import Data.List
import Data.Ord

commonMap = fmap (\x -> (head x, length x)) . group . sort

readInput = lines <$> readFile "day6.txt"

solve f = fmap (fst . f (comparing snd) . commonMap) . transpose <$> readInput

part1 = solve maximumBy
part2 = solve minimumBy