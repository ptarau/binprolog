go:-    N=16, 
        statistics(runtime,_), 
        queens(N,L), statistics(runtime,[_,Y]), 
        write(L), nl,
        write('BMARK_queens:'=[time(Y),'N'=N]), nl.




queens(N,Qs):-
        range(1,N,Ns),
        queens(Ns,[],Qs).


queens([],Qs,Qs).
queens(UnplacedQs,SafeQs,Qs):-
        sel(UnplacedQs,UnplacedQs1,Q),
        not_attack(SafeQs,Q),
        queens(UnplacedQs1,[Q|SafeQs],Qs).


not_attack(Xs,X):-not_attack(Xs,X,1).

not_attack([],_,_):-!.
not_attack([Y|Ys],X,N):-
        X =\= Y+N, 
        X =\= Y-N,
        N1 is N+1,
        not_attack(Ys,X,N1).

sel([X|Xs],Xs,X).
sel([Y|Ys],[Y|Zs],X):-
        sel(Ys,Zs,X).

range(N,N,[N]):- !.
range(M,N,[M|Ns]):-
        M < N,
        M1 is M+1,
        range(M1,N,Ns).

