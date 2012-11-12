% derived from a program by Thomas Conway

travel(Places, Route) :-
        findall(Cost - Order,
                ( cperm(Places, Order), dist(Order, Cost)),
                Solns),
        sort(Solns, [_ - Route|_]).

cperm([First|Is],CircPs):-perm([First|Is],Ps),append(Ps,[First],CircPs).

perm([],[]).
perm([X|Xs],Zs):-
	perm(Xs,Ys),
	insert(X,Ys,Zs).

insert(X,Ys,[X|Ys]).
insert(X,[Y|Ys],[Y|Zs]):-
	insert(X,Ys,Zs).

dist(Ps,Dist):-foldl(dist2,0,Ps,Dist).

dist2(P,P,0):-!.
dist2(P1,P2,D):- D is (P1+P2).

ints(N,Is):-findall(I,for(I,1,N),Is).

go(N):-ctime(T1),ints(N,Ps),travel(Ps,Route),ctime(T2),T is T2-T1,
  write(time(T)+route(Route)),nl.

go:-go(7).
