% tools

id(X,X).

compose(F,G,X,Z):-call(F,X,Y),call(G,Y,Z).

% basic operations on the monad of Lists

unitList(X,[X]).

bindList([],_,[]).
bindList([X|Xs],K,R):-
  call(K,X,Ys),
  bindList(Xs,K,Zs),
  append(Ys,Zs,R).

% derived operations on the monad of Lists

joinList(Xss,Xs) :- bindList(Xss,id,Xs).

mapList(F,Xs,Ys):-bindList(Xs,compose(F,unitList),Ys).


% test predicates and data

filterList(P,X,Xs) :- call(P,X), !, unitList(X,Xs).
filterList(_,_,[]).

flip(F,X,Y):-call(F,Y,X).

dupList(X,[X,X]).

%pair(X,Y,R):-
%  unitList(X,Xs),
%  unitList(Y,Ys),
% bindList([Xs,Ys],id,R).

mapcan(F,Xs,R):-bindList(Xs,F,R).

test(L,R):-bindList(L,dupList,R).
test(L,R):-bindList(L,compose(joinList,compose(dupList,dupList)),R).
test(L,R):-bindList(L,unitList,R).
test(L,R):-joinList([L,L,L,L],R).
test(L,R):-mapList(unitList,L,Xss),joinList(Xss,R).
test(L,R):-mapList(filterList(>(3)),L,Xss),joinList(Xss,R).
test(L,R):-mapList(+(10),L,R).
test(L,R):-mapList(compose(unitList,flip(unitList)),L,R).

go:-test([1,2,3,4],R),write(R),nl,fail.
