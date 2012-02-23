quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 
product32 x y =  (show x) ++ (show y) ++ (show (x*y))
conditions32 x y
  |length (nub (product32 x y) ) == length(product32 x y) && length(product32 x y) == 9 && head (quicksort ((show x) ++ (show y) ++ (show (x*y)))) /= '0' = True
  |otherwise = False
pt1 = sum (nub [x*y| x<-[1..9], y<-[1000..9999], conditions32 x y == True])
pt2 = sum (nub [x*y| x<-[10..99], y<-[100..1000], conditions32 x y == True])
answer = pt1 + pt2
