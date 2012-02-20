isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

triangleNumbers = [div (x * (x+1)) 2 | x <- [1, 2..] ]
upperLimit :: Int -> Int 
upperLimit x = (isqrt x) + 1

divisorsCount :: Int -> Int
divisorsCount n = 2 * length [ x | x <- [1,2..upperLimit n], ((rem n x) == 0)]

table = [ (divisorsCount x, x)| x <- triangleNumbers]
snd (head [x |x <- table, fst x > 500])