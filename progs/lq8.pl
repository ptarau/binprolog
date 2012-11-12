make_board([l1,l2,l3,l4,l5,l6,l7,l8],[c1,c2,c3,c4,c5,c6,c7,c8]):-
	lval(c1,on,[A8 -B1 ,A9 -B2 ,A10-B3 ,A11-B4 ,A12-B5 ,A13-B6 ,A14-B7 ,A15-B8 ]),
	lval(c2,on,[A7 -B2 ,A8 -B3 ,A9 -B4 ,A10-B5 ,A11-B6 ,A12-B7 ,A13-B8 ,A14-B9 ]),
	lval(c3,on,[A6 -B3 ,A7 -B4 ,A8 -B5 ,A9 -B6 ,A10-B7 ,A11-B8 ,A12-B9 ,A13-B10]),
	lval(c4,on,[A5 -B4 ,A6 -B5 ,A7 -B6 ,A8 -B7 ,A9 -B8 ,A10-B9 ,A11-B10,A12-B11]),
	lval(c5,on,[A4 -B5 ,A5 -B6 ,A6 -B7 ,A7 -B8 ,A8 -B9 ,A9 -B10,A10-B11,A11-B12]),
	lval(c6,on,[A3 -B6 ,A4 -B7 ,A5 -B8 ,A6 -B9 ,A7 -B10,A8 -B11,A9 -B12,A10-B13]),
	lval(c7,on,[A2 -B7 ,A3 -B8 ,A4 -B9 ,A5 -B10,A6 -B11,A7 -B12,A8 -B13,A9 -B14]),
	lval(c8,on,[A1 -B8 ,A2 -B9 ,A3 -B10,A4 -B11,A5 -B12,A6 -B13,A7 -B14,A8 -B15]).

queens(LCs):-
	make_board(Ls,Cs),
	queens(Ls,Cs,LCs).
	
queens([],[],[]).
queens([L|Ls],OldCs,[L-C|LCs]):-
	select(C,OldCs,NewCs),
	lval(C,on,Diags),
	mark(Ls,L,Diags),
	queens(Ls,NewCs,LCs).

select(X,[X|Xs],Xs).
select(X,[Y|Xs],[Y|Ys]):-select(X,Xs,Ys).

mark([],L,[L-L|_]).
mark([_|Ls],L,[_|Diags]):-mark(Ls,L,Diags).

test(T):-
	statistics(runtime,_),
	(
		queens(Qs),
		% write(Qs),nl,
		fail
	;
		true
	),
	statistics(runtime,[_,T]).

go:-test(T),write(T),nl.

