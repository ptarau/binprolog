go:-go(1000000),go1(1000000),go2(10000).

go(N):-loop(N),statistics.

loop(0).
loop(N):-N>0,N1 is N-1,make_garbage(N1,_),loop(N1).

make_garbage(X,g(X)).

go1(N):-loop1(N,dummy),statistics.

loop1(0,_).
loop1(N,X):-N>0,N1 is N-1,make_garbage(X,X1),loop1(N1,X1).

go2(N) :-
        mkfreelist(N,L),
        ctime(A),
        (mmc(N,L), fail ; true),
        ctime(B),
        X is B - A,
        write(time(X)), nl, fail
;       statistics.

mmc(N,L) :-
        N > 0,
        M is N - 1,
        mmc(M,L),
        !.
mmc(_,L) :-
        mkground(L).

mkfreelist(N,L) :-
        (N = 0 ->
            L = []
        ;
            NN is N - 1,
            L = [_|R],
            mkfreelist(NN,R)
        ).

mkground([]).
mkground([a|R]) :-
        mkground(R).

