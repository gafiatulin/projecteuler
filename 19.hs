daySince = [daySinceJan1ofYear (x, y)| x <- [1901..2000], y <- [1..12]]
daySince1901Jan1ToBegginingOfYear :: (Integral a)=> a -> a
daySince1901Jan1ToBegginingOfYear x
	| x == 1901 = 0
	| rem (x-1) 4 == 0 = (daySince1901Jan1ToBegginingOfYear (x-1) + 366)
	| otherwise = (daySince1901Jan1ToBegginingOfYear (x-1) + 365)
daySinceJan1ofYear :: (Integral a)=> (a, a) -> a
daySinceJan1ofYear (x, y)
	| y == 1 = daySince1901Jan1ToBegginingOfYear x
	| or (map ((y-1) ==) [1,3,5,7,8,10]) = daySinceJan1ofYear (x, (y-1)) + 31
	| or (map ((y-1) ==) [4,6,9,11]) = daySinceJan1ofYear (x, (y-1)) + 30
	| (y - 1) == 2 = (daySinceJan1ofYear (x, (y-1)) + (if rem x 4 == 0 then 29 else 28))
length (filter (== 5) [rem x 7|x <- daySince]) -- Jan 1, 1901 - Tuesday. So Sunday - Tuesday = 5