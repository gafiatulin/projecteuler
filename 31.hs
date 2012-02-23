currencySum a b c d e f g h = a + 2*b + 5*c + 10*d + 20*e + 50*f + 100*g + 200*h
answer = length [a| 
    a<-[0..200], 
    b<-[0..(div (200-a) 2)+1], 
    c<-[0..(div (200-(a+2*b)) 5)+1], 
    d<-[0..(div (200-(a+2*b+5*c)) 10)+1], 
    e<-[0..(div (200-(a + 2*b + 5*c + 10*d)) 20)+1], 
    f<-[0..(div (200-(a + 2*b + 5*c + 10*d + 20*e)) 50)+1], 
    g<-[0..2], 
    h<-[0..1],
    currencySum a b c d e f g h == 200]