isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = case (primeFactors n) of
                (_:_:_)   -> False
                _         -> True

primes :: [Integer]
primes = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
    where
        factor _ [] = []
        factor m (p:ps) | p*p > m        = [m]
                        | m `mod` p == 0 = p : factor (m `div` p) (p:ps)
                        | otherwise      = factor m ps
num = take 78499 primes
num2 = maximum [sum (take z (drop x num))| x <-[3,4..], z <- [23,25..], sum (take z (drop x num)) <1000000, isPrime (sum (take z (drop x num)))]