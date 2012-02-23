num2bin :: Int -> String
num2bin n
  | n >= 0     =  concatMap show $ reverse $ n2b n
  | otherwise  =  error "num2bin: negative number"
  where n2b 0  =  []
        n2b n  =  n `mod` 2 : n2b (n `div` 2)
answer = sum [x| x <-[1..999999], show x == reverse (show x), num2bin x == reverse (num2bin x)]