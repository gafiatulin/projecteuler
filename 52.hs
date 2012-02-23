quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  
createMultpl n = [n*x| x <-[1..6]]
numToString n = map quicksort (map show (createMultpl n))
answer = head [x|x<-[1, 2..], length( nub (numToString x))==1]