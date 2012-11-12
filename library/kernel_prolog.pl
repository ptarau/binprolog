% Kernel Prolog - defining Prolog in terms of Horn Clauses + Engines
% see demo at http://pc87043.csci.unt.edu/bp_cgi/715/kernel_prolog/query.html

% interface with existing BinProlog functionality

new_engineK(Goal,AnswerPattern,Handle):-
  open_engine(Goal,AnswerPattern,Handle).

new_answerK(Engine,Answer):-
  ask_engine(Engine,X)->Answer=the(X)
; Answer=no.

% basic kernel Prolog definitions

% returns the(X) or no as first solution of G
first_solutionK(X,G,Answer):-
  new_engineK(G,X,E),
  new_answerK(E,R),
  destroy_engine(E), % ensures early gc
  Answer=R.

% succeeds by binding G to its first solution of fails
onceK(G):-first_solutionK(G,G,the(G)).

% succeeds withount binding G, if G fails
notK(G):-first_solutionK(_,G,no).

% creates a copy of G with variables uniformly
% substituted with new variables not occuring
% in the current resolvent)
copy_termK(X,CX):-first_solutionK(X,true,the(CX)).

% true if X is currently a free varaible
varK(X):-copy_termK(X,a),copy_termK(X,b).

% executed Then if Cond succeeds otherwise executes Else
ifK(Cond,Then,Else):-
  first_solutionK(successful(Cond,Then),Cond,R),
  select_then_elseK(R,Cond,Then,Else).

% selects and calls the Then or Else part of a conditional
select_then_elseK(the(successful(Cond,Then)),Cond,Then,_):-callK(Then).
select_then_elseK(no,_,_,Else):-callK(Else).

% multiple test variant of if-then-else
if_anyK(Cond,Then,Else):-
  new_engineK(Cond,Cond,Engine),
  new_answerK(Engine,Answer),
  select_then_or_else(Answer,Engine,Cond,Then,Else).

select_then_or_else(no,_,_,_,Else):-Else.
select_then_or_else(the(BoundCond),Engine,Cond,Then,_):-
  backtrack_over_then(BoundCond,Engine,Cond,Then).

backtrack_over_then(Cond,_,Cond,Then):-Then.
backtrack_over_then(_,Engine,Cond,Then):-
  new_answerK(Engine,the(NewBoundCond)),
  backtrack_over_then(NewBoundCond,Engine,Cond,Then).

% this shows that meta cals can be seen as instances of engine operations
callK(Goal):-
  new_engineK(Goal,Goal,E),
  collect_callK(E,Goal).

collect_callK(E,Goal):-
  new_answerK(E,the(Answer)),
  collect_moreK(E,Answer,Goal).

collect_moreK(_,Answer,Answer).
collect_moreK(E,_,Answer):-collect_callK(E,Answer).

% if G has a finite number of solutions 
% returns a list Xs of copies of X each
% instantiated correspondingly
findallK(X,G,Xs):-
   new_engineK(G,X,E),
   new_answerK(E,Answer),
   collect_all_answersK(Answer,E,Xs).

% collects all answers of an Engine
collect_all_answersK(no,_,[]).
collect_all_answersK(the(X),E,[X|Xs]):-
  new_answerK(E,Answer),
  collect_all_answersK(Answer,E,Xs).

% disjunction - as usual
orK(A,_):-callK(A).
orK(_,B):-callK(B).

% conjunction - as usual
andK(A,B):-callK(A),callK(B).

% check if two terms are variants - numbervarsK to be defined
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

% emulating dynamic database operations - for
% simplicity, in asserta order

assertK(Clause,Engines,[E|Engines]):-
  new_engineK(repeat,Clause,E).

linear_assertK(Clause,Engines,[E|Engines]):-
  new_engineK(true,Clause,E).

clauseK(Engines,Head,Body):-
  member(E,Engines),
  new_answerK(E,the((Head:-Body))).

% basic reflective meta-interpreter

solveK(G):-
  onceK(reduceK(G,NewG)),
  NewG.

% reflective reducer
% the simplest such beast is: reduceK(X,X).

% reduceK(G,_):-println(trace(G)),fail.
reduceK(G,G):-is_builtin(G).
reduceK(','(A,B),','(solveK(A),solveK(B))).
reduceK(G,','(clause(G,Gs),solveK(Gs))).

/*
% extended meta-circular interpreter with
% emulated dynamic clauses

% given a set of engines Es representing
% dynamic clauses, solve the goal G
% based on reflection and use of new clauses

solveK(Es,G):-
  onceK(reduceK(Es,G,NewG)),
  callK(Es,NewG).

reduceK(_,G,G):-is_builtin(G).
reduceK(Es,','(A,B),andK(solveK(Es,A),solveK(Es,B))).
reduceK(Es,G,andK(clauseK(Es,G,Gs),solveK(Es,Gs))).

callK(_,G):-callK(G).
callK(Es,G):-clauseK(Es,G,B),callK(Es,B).
*/

% extended meta-circular interpreter with
% emulated dynamic clauses

% given a set of engines Es representing
% dynamic clauses, solve the goal G
% based on reflection and use of new clauses

solveK(Es,G):-
  onceK(reduceK(Es,G,NewG)),
  callK(Es,NewG).

reduceK(_,G,G):-is_builtin(G).
reduceK(Es,','(A,B),','(solveK(Es,A),solveK(Es,B))).
reduceK(Es,G,','(clauseK(Es,G,Gs),solveK(Es,Gs))).

callK(_,G):-callK(G).
callK(Es,G):-clauseK(Es,G,B),callK(Es,B).
