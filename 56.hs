numbers = [x^y| x<-[1..99], y<-[1..100]]
value x = sum(map digitToInt (show x))
answer = maximum [value x| x<- numbers]