my_member(X,[X|_]).
my_member(X,[_|Xs]):-
   my_member(X,Xs).
