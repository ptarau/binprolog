app([],Ys,Ys).
app([A|Xs],Ys,[A|Zs]):-
  app(Xs,Ys,Zs).

nrev([],[]).
nrev([X|Xs],R):-
  nrev(Xs,T),
  det_append(T,[X],R). % deterministic append

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).

integers([],I,I):-!.
integers([I0|L],I0,I):-I0<I,I1 is I0+1,integers(L,I1,I).

empty_for(It):-range(_,1,It),true,fail.
empty_for(_).

full_for(It,L):-range(_,1,It),nrev(L,_),fail.
full_for(_,_).

bm(It,Len,Time,Lips):-
	integers(L,0,Len),
	statistics(runtime,_),
	empty_for(It),
	statistics(runtime,[_,T1]),
	full_for(It,L),
	statistics(runtime,[_,T2]),
	TimeMS is T2-T1, Time is TimeMS//10,
	L1 is Len+1,
	L2 is Len+2,
	P is L1*L2,
	LI is P//2,
	Temp is It*LI,
	LIs is 100*Temp,
	Lips is LIs//Time. 

htest(N,H,T,S):-
        integers(Is,0,N),
        statistics(global_stack,[H1,_]),
        statistics(trail,[T1,_]),
        statistics(local_stack,[S1,_]),
        nrev(Is,_),
        statistics(global_stack,[H2,_]),
        statistics(trail,[T2,_]),
        statistics(local_stack,[S2,_]),
        H is H2-H1,T is T2-T1,S is S2-S1.

rtest(N,[time,T,lips,L,heap,H,trail,Tr,stack,S]):-
	bm(1000,N,T,L),htest(N,H,Tr,S).

test(X):-rtest(100,X).

go:-test(X),write(X),nl.

