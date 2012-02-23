import Data.List (nub)
isPandigital x
  |length x == length (nub x) && length x == 9 && head(quicksort x) /= '0' = True
  |otherwise = False

concatProduct :: Int -> [Int]-> String
concatProduct x [] = []
concatProduct x xs = (show(x * head xs)) ++ concatProduct x (tail xs)
ranges n = [[1..x]| x<- [2..n]]
answer = maximum [concatProduct x y| x<- [1..10000], y <- ranges 20, isPandigital (concatProduct x y) == True]