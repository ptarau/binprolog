path(G,X,Z,[X|Xs]) :- 
   linked(G,X,Ys)->member(Y,Ys),path(G,Y,Z,Xs)
;  X=Z,Xs=[].

linked(G,X,Ys):-val(G,X,N-Ys),var(N),N=deja_vu.

new_graph(G):-
  lval(G,1,_-[2,3]), 
  lval(G,2,_-[1,4]),
  lval(G,3,_-[1,5]),
  lval(G,4,_-[1,5]).

walk(G,Xs):-
  new_graph(G),
  path(G,1,5,Xs).

go:-walk(g,Path),write(Path),nl,fail.

% lval defines a 2 keyed backtrackable global variable
% val gets the value of a 2 keyed backtrackable global variable if defined
