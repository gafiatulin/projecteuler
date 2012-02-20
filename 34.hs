factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
sum [x| x <-[3,5..1000000], sum [factorial (digitToInt y)|y<-(show x)] == x]