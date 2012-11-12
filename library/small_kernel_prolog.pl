% Kernel Prolog - defining Prolog in terms of Horn Clauses + Engines
% see demo at http://pc87043.csci.unt.edu/bp_cgi/kernel_prolog/query.html


% interface with existing BinProlog functionality

new_engineK(Goal,AnswerPattern,Handle):-
  open_engine(Goal,AnswerPattern,Handle).

new_answerK(Engine,Answer):-
  ask_engine(Engine,X)->Answer=the(X)
; Answer=no.

% basic kernel Prolog definitions
first_solutionK(X,G,Answer):-
  new_engineK(G,X,E),
  new_answerK(E,R),
  destroy_engine(E), % ensures early gc
  Answer=R.

onceK(G):-first_solutionK(G,G,the(G)).

notK(G):-first_solutionK(_,G,no).

copy_termK(X,CX):-first_solutionK(X,true,the(CX)).

ifK(Cond,Then,Else):-
  first_solutionK(successful(Cond,Then),Cond,R),
  select_then_else(R,Cond,Then,Else).

findallK(X,G,Xs):-
   new_engineK(G,X,E),
   new_answerK(E,Answer),
   collect_all_answersK(Answer,E,Xs).

collect_all_answersK(no,_,[]).
collect_all_answersK(the(X),E,[X|Xs]):-
  new_answerK(E,Answer),
  collect_all_answersK(Answer,E,Xs).

% this shows that meta cals can be seen as instances of engine operations
callK(Goal):-
  new_engineK(Goal,Goal,E),
  collect_callK(E,Goal).

collect_callK(E,Goal):-
  new_answerK(E,the(Answer)),
  collect_moreK(E,Answer,Goal).

collect_moreK(_,Answer,Answer).
collect_moreK(E,_,Answer):-collect_callK(E,Answer).

varK(X):-copy_termK(X,a),copy_termK(X,b).

variant_ofK(Term,Variant):-
   copy_termK(Term,T1),
   copy_termK(Variant,T2),
   numbervarsK(T1,T),
   numbervarsK(T2,T).

engine_memberK(E,Answer):-
  new_answerK(E,the(X)),
  other_engine_memberK(E,X,Answer).

other_engine_memberK(_,Answer,Answer).
other_engine_memberK(E,_,Answer):-
  engine_memberK(E,Answer).

engine_unionK(Es,Answer):-
  member(E,Es),
  engine_memberK(E,Answer).

memo_transformerK(BaseEngine,list_engine(Xs)):-
  findallK(X,engine_memberK(BaseEngine,X),Xs).

new_memo_engineK(list_engine(Xs),E):-
  new_engineK(member(X,Xs),X,E). 

% data-structure to engine transformer
list_engineK(Xs,E):-
  new_engineK(member(X,Xs),X,E).     

