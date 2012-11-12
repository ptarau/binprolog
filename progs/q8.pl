% Variant of a program by T. Fruhwrith

go:-go('BMARK_q8:').

go(Mes):-
	statistics(runtime,_),queens(8,_),fail
;	statistics(runtime,[_,T]),write(Mes=time(T)),nl.

queens(N,Qs):- gen_list(N,Qs), place_queens(N,Qs,_,_).

gen_list(0,[]).
gen_list(N,[_|L]):-
        N>0, M is N-1,
        gen_list(M,L).

place_queens(0,_,_,_).
place_queens(I,Cs,Us,[_|Ds]):-
        I>0, J is I-1,
        place_queens(J,Cs,[_|Us],Ds),
        place_queen(I,Cs,Us,Ds).

place_queen(I,[I|_],[I|_],[I|_]).
place_queen(I,[_|Cs],[_|Us],[_|Ds]):-
        place_queen(I,Cs,Us,Ds).
