fib = fst . fib2
fib2 0 = (1, 1)
fib2 1 = (1, 2)
fib2 n
 | even n    = (a*a + b*b, c*c - a*a)
 | otherwise = (c*c - a*a, b*b + c*c)
 where (a,b) = fib2 (n `div` 2 - 1)
       c     = a + b
fib1 n = fib (n - 1)
head [x| x <- [1..], length (show (fib1 x)) > 999]