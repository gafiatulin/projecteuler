digitToInt :: Char -> Int
digitToInt c
  | isDigit c            =  fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
  | otherwise            =  error "Char.digitToInt: not a digit"
sumOfFifthPowersOfDigits x = sum (map (^5) (map digitToInt (show x))) 
answer = sum [x|x <-[2, 3..200000], sumOfFifthPowersOfDigits x == x]