fibo(N,X):-N=<1,!,X=1.
fibo(N,X):-N1 is N-1,N2 is N-2,fibo(N1,X1),fibo(N2,X2),X is X1+X2.

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).
        
f_iter(Max,N,R):-range(_,1,Max),fibo(N,R),fail.
f_iter(_,N,R):-fibo(N,R).

go(Mes):-
	I=50,N=16,
	statistics(runtime,_),
	statistics(global_stack,[H1,_]),
	statistics(trail,[TR1,_]),
	f_iter(I,N,R),
	statistics(runtime,[_,T]),
	statistics(global_stack,[H2,_]),
	statistics(trail,[TR2,_]),
	H is H2-H1,TR is TR2-TR1,
	nl,write(Mes=[time=T,heap=H,trail=TR,fibo(N,R)]),nl.

go:-go('BMARK_fibo:').

fibonacci(N,R) :- slide_fibo(0,N,0,1,R).

slide_fibo(To,To,_X,Y,R) :- R is Y.
slide_fibo(From,To,X,Y,R) :- 
  From<To, 
  NextFrom is From+1,
  Z is X+Y, 
  slide_fibo(NextFrom,To,Y,Z,R).