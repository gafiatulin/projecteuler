answer = snd (maximum [(recurringCycle n, n) | n <- primes 1000])
    where  recurringCycle d = remainders d 10 []
           remainders d 0 rs = 0
           remainders d r rs = let r' = r `mod` d
                               in case elemIndex r' rs of
                                    Just i  -> i + 1
                                    Nothing -> remainders d (10*r') (r':rs)