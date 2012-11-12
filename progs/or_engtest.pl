/* 2 engines are created and run in parallel */
/* both fully backtrack through their search-tree */
/* the result of the consumer is kept through a side-effect (assert) */


new_engine(G,X,E):-
  create_engine(100,100,100,E),
  load_engine(E,G,X).

p(P):-new_engine(append(X,Y,[A,A]),sol(X,Y),P).

consume(P,[X|Xs]):-ask_engine(P,A),!,copy_term(A,X),consume(P,Xs).
consume(_,[]).

keep(P):-consume(P,Xs),assert(sol(P,Xs)).

c(P,C):-new_engine(keep(P),_,C).

go:-p(A),c(A,B),
  multitask_engines(100),
  retract(sol(P,Xs)),
  !,
  destroy_engine(A),
  destroy_engine(B),
  write(P+Xs),nl.

  
