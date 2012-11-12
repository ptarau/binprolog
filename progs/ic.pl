ic([a,b,c]).
ic([d,e]).

ab([a,c,f]).
ab([a,d]).
ab([b,c,d]).

violates(Abduced,IC):-select(X,Abduced,More),select(Y,More,_),member(X,IC),member(Y,IC).

inconsistent(Abduced):-ic(IC),violates(Abduced,IC).

select(X,[X|Xs],Xs).
select(X,[Y|Xs],[Y|Ys]):-select(X,Xs,Ys).

explanation(Abduced):- ab(Abduced), \+inconsistent(Abduced).

go:-explanation(Abduced),write(Abduced),nl,fail.
go.
