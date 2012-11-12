/*----------------------------------------------------------------------------
Program:  Knight's Tour
Author:   E. Tick
Date:     September 10 1989
Notes:
% adapted and optimized for BinProlog: Paul Tarau, August 1992
% changed semantics: the knight has to cover the board completely

1. To run:
    ?- go(N,T,S).	
for input N (side of board), output T is execution time, and S is the number 
of solutions.  Usually we run go(5,X,Y).

----------------------------------------------------------------------------
:- sequential.
:- parallel move/2.
*/

p:-[knight].


go:-go(5).

go(N):-time(_),init(N,M),knight(M,1,1),!,time(T),
	write(time=T),nl,statistics,show(N).

init(N,_):-
	for(I,1,N),
		for(J,1,N),
			bb_def(I,J,_NewVar),
	fail.
init(N,M):-
	M is N*N.

knight(0,_,_) :- !.
knight(K,A,B) :-
		K1 is K-1,
		val(A,B,K),
		move(Dx,Dy),
		step(K1,A,B,Dx,Dy).

step(K1,A,B,Dx,Dy):-
    C is A + Dx,
    D is B + Dy,
    knight(K1,C,D).

% wam.c: strange bug when cells are not cast to (int) in arith ops!!!
show(N):-
	cwrite('The Board'),nl,
	for(I,1,N),
		nl,
		for(J,1,N),
			val(I,J,V),
			X is 1-V // 10,
			cwrite(' '),tab(X),cwrite(V),
	fail.
show(_):-nl.

move( 2, 1).
move( 2,-1).
move(-2, 1).
move(-2,-1).
move( 1, 2).
move(-1, 2).
move( 1,-2).
move(-1,-2).

time(T) :- statistics(runtime,[_,T]).
