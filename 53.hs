binC n k = div (factorial n) ((factorial k)*(factorial (n-k)))
cList = [binC n k| k <- [1..100], n <- [k..100], (binC n k) > 1000000]
answer = length cList