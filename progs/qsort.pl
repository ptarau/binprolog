p:-[qsort].

go:-go('BMARK_qsort:').

go(Mes):-
	write('use bp -h4000 -t1000 -s1000'),nl,
	list(L),augment(L,L3),augment(L3,L9),augment(L9,L27),
	statistics(global_stack,[H1,_]),
	statistics(trail,[TR1,_]),
	time(_),
		qsort(L27,_Sorted,[]),
	time(T),
	statistics(global_stack,[H2,_]),H is H2-H1,
	statistics(trail,[TR2,_]),TR is TR2-TR1,
	write(Mes=[time(T),heap(H),trail(TR)]),nl.

go1:-L=[3,4,1,2,1,2,5,1,3,0,9,7],qsort(L,R,[]),write(L-R),nl.

on(X,[X|_]).
on(X,[_|Xs]):-on(X,Xs).

augment(L,R):-findall(X,(on(Y,L),(X is Y+1;Y=X;X is Y-1)),R).

list([27,74,17,33,94,18,46,83,65, 2,
		32,53,28,85,99,47,28,82, 6,11,
		55,29,39,81,90,37,10, 0,66,51,
		 7,21,85,27,31,63,75, 4,95,99,
		11,28,61,74,18,92,40,53,59, 8]).

qsort([],S,S).
qsort([Y|L],S1,S3) :- partition(L,Y,S1,S3).

partition([],Y,[Y|S],S).
partition([X|L],Y,S1,S3) :- 
	partition1(L,Y,L1,L2,X),
	qsort(L1,S1,[Y|S2]),
	qsort(L2,S2,S3).

partition1(L,Y,[X|L1],L2,X) :-
	X =< Y,!,
	partition2(L,Y,L1,L2).
partition1(L,Y,L1,[X|L2],X) :-
	partition2(L,Y,L1,L2).

partition2([],_,[],[]).
partition2([X|L],Y,L1,L2) :- partition1(L,Y,L1,L2,X).

time(T):-statistics(runtime,[_,T]).
