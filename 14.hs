chain :: (Integral a) => a ->  [a]
chain n 
	| n == 1 = [1]
	| even n = n:(chain (div n 2))
	| otherwise = n:(chain ((3*n)+1))

findMax x
	| length x == 1 = head x
	| otherwise = max (findMax (take (div (length x) 2) x)) (findMax (drop (div (length x) 2) x))

findMax [ (length (chain x), x)| x <- [1, 3..1000000]]