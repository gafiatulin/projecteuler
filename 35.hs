import Control.Monad (replicateM)
canBeCircularPrimeList = [1,3,7,9]

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

listToInt n = foldl (\x y -> 10*x+y) 0 n
rot n l = y ++ x where (x,y) = splitAt n l
allrots l = map (\x -> rot x l) [0..(length l)-1]
isCircular l =  all (isPrime . listToInt) $ allrots l
circular 1 = [[2],[3],[5],[7]]  -- a slightly special case
circular n = filter isCircular $ replicateM n canBeCircularPrimeList
 
problem_35 = length $ concatMap circular [1..6]