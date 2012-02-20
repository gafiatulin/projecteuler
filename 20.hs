isDigit c                =  c >= '0' && c <= '9'

digitToInt :: Char -> Int
digitToInt c
  | isDigit c            =  fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' =  fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' =  fromEnum c - fromEnum 'A' + 10
  | otherwise            =  error "Char.digitToInt: not a digit"
stringToListOfTnt = map digitToInt

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

sum [x | x <- (stringToListOfTnt (show (factorial 100)))]