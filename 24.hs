permutations [] = [[]]
permutations xs = concat [map (x:) (permutations $ filter (x/=) xs) | x <- xs]
(permutations [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])!!999999