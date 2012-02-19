lcmMy :: (Integral a) => [a] -> a
lcmMy [x] = x
lcmMy (x:xs) = lcm x (lcmMy xs)
lcmMy [1..20]