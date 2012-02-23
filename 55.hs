isPalindrome x
  | x == reverse x = True
  |otherwise = False

magic :: Integer->Integer->Integer
magic a b
  | b == 50 = -1
  |isPalindrome (show a) == True && b == 1 = magic (a + read (reverse (show a)) ::Integer) (b+1)
  | isPalindrome (show a) == True = b-1
  | otherwise = magic (a + read (reverse (show a)) ::Integer) (b+1)

answer = [magic x 1|x<-[1..10000], (magic x 1 ) < 0]