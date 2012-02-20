first20Num = ["one", "two", "three", "four", "five","six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",          "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty"]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]
lettersInText n
	| n < 20 = length (last (take n first20Num))
	| n < 100 && rem n 10 == 0 = length (last (take (div n 10) tens))
	| n < 100 = lettersInText (rem n 10) + lettersInText (n - (rem n 10))
	| n < 1000 && rem n 100 == 0 = length (last (take (div n 100) first20Num)) + length "hundred"
	| n < 1000 = lettersInText (rem n 100) + lettersInText (n - (rem n 100)) + 3
	| n == 1000 = 3+ length "thousand"

sum [lettersInText x| x <-[1..1000]]