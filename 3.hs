minus (x:xs) (y:ys) = case (compare x y) of 
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys 
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs

primes m = 2 : eratos [3,5..m] where
	eratos []     = []
	eratos (p:xs) = p : eratos (xs `minus` [p, p+2*p..m])

maximum [x| x <- primes (floor (sqrt 600851475143)), 600851475143 `mod` x == 0]