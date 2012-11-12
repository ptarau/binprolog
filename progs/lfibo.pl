:-write('Program: lfibo.pl'),nl.
:-write('Author: Paul Tarau'),nl.
:-write('fibonacci(40) program with constant time lemmas'),nl.
:-write('executed 10000 times'),nl.

go:-
        I=10000,N=40,
        statistics(runtime,_),
        statistics(global_stack,[H1,_]),
        statistics(trail,[TR1,_]),
        f_iter(I,N,R),
        statistics(runtime,[_,T]),
        statistics(global_stack,[H2,_]),
        statistics(trail,[TR2,_]),
        H is H2-H1,TR is TR2-TR1,
        bb,
        write([time=T,heap=H,trail=TR,fibo(N,R)]),nl.

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).
        
f_iter(Max,N,R):-range(_,1,Max),fibo(N,R),fail.
f_iter(_,N,R):-fibo(N,R).

fibo(N,1):-N<2,!.
fibo(N,Y):-
	N1 is N-1, N2 is N-2,
	fibo_lemma(fibo,N1,Y1),
	fibo_lemma(fibo,N2,Y2),
	Y is Y1+Y2.

% optimized lemma: <P,I> --> X (P,I,O must be atomic)
fibo_lemma(P,I,O):-val(P,I,X),!,X=O.
fibo_lemma(P,I,O):-functor(G,P,2),arg(1,G,I),G,!,
	arg(2,G,O),
	def(P,I,O).
