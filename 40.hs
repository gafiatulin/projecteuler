numbers = [show x | x <- [1..]]
list [] = []
list (x:xs) = x ++ list xs
answer = product [digitToInt (list(take 1000001 numbers)!!(x-1) )|x <- [1,10,100, 1000, 10000, 100000, 1000000]]