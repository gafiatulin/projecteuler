divisorsSum :: Int -> Int
divisorsSum n = sum [ x | x <- [1,2..(div n 2) + 1], ((rem n x) == 0)]
functDList = [ x | x <- [1..10000], divisorsSum (divisorsSum x) ==  x, x /= divisorsSum x]
answer = sum functDList