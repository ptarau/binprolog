% BinProlog compatibilty package for other Prologs.
%
% It allows execution of BinProlog-style bboard operations
% which do not depend on the semantics (logical or immediate udate)
% of the underlying assert/retarct operations: therefore it ensures
% that code using it will be highly portable.
%
% Tested on: Aquarius 1.0 SB-Prolog 3.1, C-Prolog 1.4, SWI-prolog.

:-dynamic(bb/3).
:-G=bb('$empty','$empty','$empty'),assert(G),retract(G),!.

term2key(T,NewT):-nonvar(T),functor(T,Tf,Tn),functor(NewT,Tf,Tn).

bb_def(N,K,_):-bb(N,K,_),!,fail.
bb_def(N,K,X):-bb_assert(N,K,X).

bb_val(N,K,X):-nonvar(N),nonvar(K),bb(N,K,Y),!,X=Y.

bb_set(N,K,X):-nonvar(N),nonvar(K),retract(bb(N,K,_)),!,
  bb_assert(N,K,X).
bb_set(N,_,_):-
  bb_error(N).

bb_rm(N,K):-nonvar(N),nonvar(K),
  ( retract(bb(N,K,_))->true
    ; bb_error(N)
  ).

bb_error(T):-functor(T,F,N),
  write(F/N),
  write(' ??? bb_def/3 expected before bb_set/3, bb_rm/2'),nl,
  fail.

bb_let(N,K,_):-bb_rm(N,K),fail.
bb_let(N,K,X):-bb_set(N,K,X).

bb_def(K,X):-bb_def(K,K,X).

bb_set(K,X):-bb_set(K,K,X).

bb_val(K,X):-bb_val(K,K,X).

bb_rm(K):-bb_rm(K,K).

bb_let(K,X):-bb_let(K,K,X).

bb:-
  bb(N,K,X),functor(N,Nf,Nn),functor(K,Kf,Kn),
  write(Nf/Nn+Kf/Kn=X),nl,fail
; nl.

bb_assert(N,K,X):-
  term2key(N,NewN),
  term2key(K,NewK),
  assert(bb(NewN,NewK,X)).

go:-
  bb_def(a,13),bb_set(a,10),bb_val(a,X),write(X),nl,fail
; bb_set(b,5)
; bb_def(f(1,1),g(2,2)),fail
; bb_val(f(a,a),X),write(X),nl,fail
; bb_rm(c,d)
; bb_def(_,_,_)
; bb.

p:-reconsult('bb.pl').
