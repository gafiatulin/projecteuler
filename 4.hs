set = [100..999]
palindrom :: (Integral a) => a -> Bool
palindrom x = if show x == reverse (show x) then True else False
maximum [ x*y | x <- set, y <- set, palindrom (x*y) == True]