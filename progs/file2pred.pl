p:-compile(file2pred).

sics:-
  compile('../src/oper'),
  compile('../src/other'),
  compile('../src/init'),
  compile('../src/co'),
  compile('../src/bin'),
  init_other_sicstus.

go:-
  ctime(T1),
%    go('../progs/boyer.pl'),
%   go('../progs/file2pred.pl'),
  go('../src/wam.pro'),
  ctime(T2),T is T2-T1,write(time=T),nl.

go(File):-
  see(File),
  get_a_predicate(FN,Cs),
  nl,write('% '),write(FN),nl,
  member(C,Cs),
  portray_clause(C),
  fail
; seen.

rr_read_clause(C):-
  get_a_predicate(FN,Cs),
  ttyout((write('% '),write(FN),nl)),
  member(C,Cs).


get_a_predicate(FN,Cs):-
  read_predicate(FN,Cs),
  ( FN=end_of_file/0->! %,fail
  ; true
  ).
get_a_predicate(FN,Cs):-
  get_a_predicate(FN,Cs).

read_predicate(FN,[C|Cs]):-
  rclause(FN,C),
  ( FN=end_of_file/0->Cs=[]
  ; get_all_clauses(FN,Cs)
  ).

get_all_clauses(FN,Cs):-findall(C,get_a_clause(FN,C),Cs).

get_a_clause(FN,C):-
   radd(NewFN,C),
   ( 
     (NewFN = FN, NewFN\==(:-)/1) -> true
   ; !, bb_def(left,over,NewFN-C),fail
   ).
get_a_clause(FN,C):-
   get_a_clause(FN,C).

rclause(FN,C):-bb_val(left,over,FN-C),bb_rm(left,over),!.
rclause(FN,C):-radd(FN,C).

radd(FN,C):-read_clause(C),get_pred(C,FN).

get_pred((H:-_),F/N):-!,functor(H,F,N).
get_pred((::-(H,_)),F/N1):-!,functor(H,F,N),N1 is N-1.
get_pred(H,F/N):-functor(H,F,N).


'::-'(a(1,C),b(C)).
a(2).

b.

:-write(a),nl.
:-write(b),nl.

c(X)-->{a(X)},[ok].
d(X)-->{X=3}.

d(S,T):-c(2,S,T).
ax(X)-->c(X);d(X).

ax:-ax(X,Xs,[]),write(X+Xs),nl,fail.
