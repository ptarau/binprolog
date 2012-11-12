/* Prolog version of hanoi benchmark */

go:-go(16).

time(T):-statistics(runtime,[T,_]).

go(N) :- time(T1),han(N,1,2,3),time(T2),T is T2-T1,write(T),nl.

han(N,_,_,_) :- N=<0,!.
han(N,A,B,C) :- N>0,
        N1 is N - 1,
	%write(A+B=C),nl,
        han(N1,A,C,B),
        han(N1,C,B,A).

