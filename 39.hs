import Data.List (elemIndex)
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

answer = head $ perims !! indexMax
    where  perims = group
                    $ quicksort [n*p | p <- pTriples, n <- [1..1000 `div` p]]
           counts = map length perims
           Just indexMax = elemIndex (maximum counts) $ counts
           pTriples = [p |
                       n <- [1..floor (sqrt 1000)],
                       m <- [n+1..floor (sqrt 1000)],
                       even n || even m,
                       gcd n m == 1,
                       let a = m^2 - n^2,
                       let b = 2*m*n,
                       let c = m^2 + n^2,
                       let p = a + b + c,
                       p < 1000]