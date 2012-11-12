go:-G=max([3,4.66,1,-0.5],_Max),G,write(G),nl.

max(Xs,X):-bestof(X, >, member(X,Xs)).

% The following is an efficient implementation
% of bestof/3 using the blackboard.
% You add it to extra.pl and type ?-boot. to integrate it in the kernel
% if you plan to use it.

% true if X is an answer of Generator such that
% X Rel Y for every other answer Y of Generator
bestof(X,Closure,Generator):-
  copy_term(X,Y),
  Closure=..L1,
  det_append(L1,[X,Y],L2),
  Test=..L2,
  bestof0(X,Y,Generator,Test).

bestof0(X,Y,Generator,Test):-
  inc_level(bestof,Level),
  Generator,
  update_bestof(Level,X,Y,Test),
  fail.
bestof0(X,_,_,_):-
  dec_level(bestof,Level),
  val(bestof,Level,X),
  rm(bestof,Level).

% uses Rel to compare New with so far the best answer
update_bestof(Level,New,Old,Test):-
  val(bestof,Level,Old),!,
  Test,
  bb_set(bestof,Level,New).
update_bestof(Level,New,_,_):-
  bb_let(bestof,Level,New).

% ensure correct implementation of embedded calls to bestof/3
inc_level(Obj,X1):-val(Obj,Obj,X),!,X1 is X+1,bb_set(Obj,Obj,X1).
inc_level(Obj,1):-bb_def(Obj,Obj,1).

dec_level(Obj,X):-val(Obj,Obj,X),X>0,X1 is X-1,bb_set(Obj,Obj,X1).

