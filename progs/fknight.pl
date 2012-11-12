p:-compile('fknight.pl').

/* 
   adapted by Paul Tarau from (partial) KnightTour benchmark originally
   written by Evan Tick 
*/

go:-go(5).

go(N):-
	time(_),
	make_board(N,Board,NbMoves),
	knight(NbMoves,1,1,N,Board),!,
	time(T),
	nl,write('BMARK_fknight'=[time(T),'N'=N]),nl,
        statistics,show(N,Board).

make_board(N,Board,M):-
	M is N*N,
	functor(Line,line,N),
	findall(Line,range(_,1,N),LBoard),
	Board=..[board|LBoard].

val(I,J,Val,N,Board):-
	I>0,I=<N,J>0,J=<N,
	arg(I,Board,Line),
	arg(J,Line,Val).

knight(0,_,_,_,_) :- !.
knight(K,A,B,N,Board) :-
		K1 is K-1,
		val(A,B,K,N,Board),
		move(Dx,Dy),
		step(K1,A,B,Dx,Dy,N,Board).

step(K1,A,B,Dx,Dy,N,Board):-
    C is A + Dx,
    D is B + Dy,
    knight(K1,C,D,N,Board).

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).

show(N,Board):-
	range(I,1,N),
		range(J,1,N),
			val(I,J,V,N,Board),
			write(' '),X is 1-V//10,tab(X),write(V),
			(J=N->nl;true),
	fail.
show(_,_):-nl.

move( 2, 1).
move( 2,-1).
move(-2, 1).
move(-2,-1).
move( 1, 2).
move(-1, 2).
move( 1,-2).
move(-1,-2).


time(T) :- statistics(runtime,[_,T]).

