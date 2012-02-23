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
endTruncs x = [read(take n (show x))::Integer|n<-[1..length(show x)]]
startTruncs x = [read(drop n (show x))::Integer|n<-[0..length(show x)-1]]
truncs x = nub (startTruncs x ++ endTruncs x)
isTruncatablePrime x
  | and (map isPrime (truncs x)) == True = True 
  | otherwise = False
truncatablePrimes = take 11 [x| x <- (drop 4 primes), isTruncatablePrime x == True]
answer = sum truncatablePrimes
