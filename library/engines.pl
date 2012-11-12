etests:-
  exec_tests([find_all_test,task_test,engine_test]).


ask_or_kill(Handle,Answer):-ask_engine(Handle,Answer),!.
ask_or_kill(Handle,_):-destroy_engine(Handle),fail.

% engine based findall
find_all(X,Goal,Xs):-
  default_engine_params(H,S,T),
  find_all(H,S,T,X,Goal,Xs).

find_all(H,S,T,X,Goal,Xs):-
  new_engine(H,S,T,Goal,X,Handle),
  collect_answers(Handle,Xs),
  destroy_engine(Handle).

collect_answers(Handle,[X|Xs]):-ask_engine(Handle,A),!,
  copy_term(A,X),
  collect_answers(Handle,Xs).
collect_answers(_,[]).

new_engine(H,S,T,Goal,Answer,Handle):-
  create_engine(H,S,T,Handle),
  load_engine(Handle,Goal,Answer).

call_and_fail(Goal):-
  default_engine_params(H,S,T),
  call_and_fail(H,S,T,Goal).

call_and_fail(H,S,T,Goal):-
  new_engine(H,S,T,Goal,X,Handle),
  ( ask_engine(Handle,X),fail
   ;destroy_engine(Handle)
  ).


% mcall(Goal): memoized call for Goal
% builds an engine for answer, if needed, then reuses it
% within the same or different or branches for
% variants of Goal
% a better implementation would be lazy, but
% its semantics should be the same

mcall(Goal):-
  vars_of(Goal,A),
  ( asserted(answer_to(G,As)),variant_of(Goal,G)->true
  ; find_all(A,Goal,As),
    assert(answer_to(Goal,As))
  ),
  member(A,As).
  

% engine based once/1
call_once(Goal):-
  default_engine_params(H,S,T),
  call_once(H,S,T,Goal).

call_once(H,S,T,Goal):-
  copy_term(Goal,G),
  new_engine(H,S,T,G,G,Handle),
  ask_engine(Handle,X),
  copy_term(X,Goal),
  destroy_engine(Handle).

default_engine_params(512,64,64).

find_all_test:-
  As=[A,B,B,A],
  find_all(X,member(s(X),As),Xs),write(As+Xs),nl,
  findall(X,member(s(X),As),Ys),write(As+Ys),nl.


engine_test:-
  call_and_fail((X=f(X),write(X),nl)),
  nl,write('engine dies gracefully'),nl,
  call_once(member(A,[B,B])),
  write(A+B),nl.

exec_tests([]).
exec_tests([G|Gs]):-
  write(test=G),nl,
  (G->write(suceeds);write(fails)),
  nl,
  exec_tests(Gs).

create_task_loop(E):-
  create_engine(E),
  load_engine(E,task_loop(E,X),X).

task_loop(E,X):-
  retract1(task_on_engine(E,Goal,X))->once(Goal)
 ;!,asserta(empty_engine(E)),fail.
task_loop(E,X):-
   task_loop(E,X).

add_task(E,Goal,X):-
   ( retract1(empty_engine(E))->load_engine(E,task_loop(E,X),X)
   ; true
   ),
   assertz(task_on_engine(E,Goal,X)).

ask_task_loop(E,X):-ask_engine(E,X).

destroy_task_loop(E):-
   retractall(task_on_engine(E,_,_)),
   retractall(empty_engine(E)),
   destroy_engine(E).


task_test:-
   create_task_loop(E),
   write('creating engine'(E)),nl,
   add_task(E,A=1,A),
   add_task(E,A=2,A),
   add_task(E,A=3,A),
   ask_task_loop(E,X),write(X),nl,
   ask_task_loop(E,Y),write(Y),nl,
   add_task(E,A=4,A),
   ask_task_loop(E,Z),write(Z),nl,
   ask_task_loop(E,U),write(U),nl,
   (
   \+ ask_task_loop(E,_)->destroy_task_loop(E)
   ; write('unable to destroy'(E))
   ).

