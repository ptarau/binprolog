loop(Max):-N is 1<<Max,loop(N,0).

loop(0,_):-write(done),nl.
loop(N,X):-N>0,N1 is N-1,loop(N1,c(X,N)).

