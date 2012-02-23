primes :: [Integer]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
    where
        factor _ [] = []
        factor m (p:ps) | p*p > m        = [m]
                        | m `mod` p == 0 = p : factor (m `div` p) (p:ps)
                        | otherwise      = factor m ps

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = case (primeFactors n) of
                (_:_:_)   -> False
                _         -> True

permutation [] = [[]]
permutation xs = [x:ys | x <- xs, ys <- permutation (delete x xs)]
numbersToN '1' = ['1']
numbersToN x = x:(numbersToN (intToDigit ((digitToInt x)-1)))
possibleNum =  [read((permutation (numbersToN x))!!y)::Integer| x <- ['5'..'9'], y <-[0 .. (factorial (digitToInt x))-1]]
answer = maximum [x| x<- possibleNum, isPrime x]