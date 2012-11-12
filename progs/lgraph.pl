path(X,X,[X]).
path(X,Z,[X|Xs]):-linked(X,Y),path(Y,Z,Xs).

linked(X,Y):-c(X,Ys),member(Y,Ys).

go(Xs):-
  c(1,[2,3]) -: c(2,[1,4]) -: c(3,[1,5]) -: c(4,[1,5]) -:
  path(1,5,Xs).

go:-go(Xs),write(Xs),nl,fail.
