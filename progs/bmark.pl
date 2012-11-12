timer(T):-statistics(runtime,[T,_]).

test_times(It,Goal):- range(_,1,It),Goal,fail.
test_times(_,_).

dummy.
dummy(_).
dummy(_,_).
dummy(_,_,_).
dummy(_,_,_,_).
dummy(_,_,_,_,_).

make_dummy(Goal,Dummy):-Goal=..[_|Xs],Dummy=..[dummy|Xs].

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).

bmark(It,Goal,Time):-
	make_dummy(Goal,Dummy),
	timer(T0),
	test_times(It,Dummy),
	timer(T1),
	test_times(It,Goal),
	timer(T2),
	Time is (T2-T1)-(T1-T0).


rusage(Goal,H,Tr,S):-
        statistics(global_stack,[H1,_]),
        statistics(trail,[T1,_]),
        statistics(local_stack,[S1,_]),
        Goal,
        statistics(global_stack,[H2,_]),
        statistics(trail,[T2,_]),
        statistics(local_stack,[S2,_]),
        H is H2-H1,Tr is T2-T1,S is S2-S1.

go(Mes,It,Goal):-
	bmark(It,Goal,Time),
	functor(Goal,F,N),
        nl,write(Mes=[goal=F/N,iterations=It,time=Time]),nl,
	rusage(Goal,H,Tr,S),
	nl,write(Mes=[heap=H,trail=Tr,stack=S]),nl.
