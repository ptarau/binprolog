
p:-[algraph].

path(X,X,[X]).
path(X,Z,[X|Xs]):-c(X,Y),path(Y,Z,Xs).

% data

go(Xs):-
  (c(1,X1):-c1(X1)) -: 
  (c(2,X2):-c2(X2)) -: 
  (c(3,X3):-c3(X3)) -: 
  (c(4,X4):-c4(X4)) -:
  path(1,5,Xs).

c1(2).
c1(3).

c2(1).
c2(4).

c3(1).
c3(5).

c4(1).
c4(5).

% test

go:-go(Xs),write(Xs),nl,fail.
