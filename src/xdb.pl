%:-db_hook_on.
%:-[assertbm].

xbug:-quiet(1),db_hook_on,x_trace(asserta(a(1))),x_trace(asserta(a(2))),listing.
    
xtest:-
  db_hook_on,
  assert(a(1,1)),
  assert(a(1,2)),
  assert(a(2,2)),
  assert(a(3)),
  assert(b(2)),
  assert((b(X):-a(X))),
  assert((b(X):-a(_,X))),
  listing,
  retractall(a(1,_X)),
  println(after_retractall),
  listing,
  true.

x_listing:-
  x_listing(_),
  hash_get(K,C),println(K:C),
  fail
; true.

x_listing(F/N):-
  (functor(H,F,N)->true;true),
  x_clause(H,B),
  functor(H,F,N),
  pp_clause((H:-B)),
  fail
; true.

x_consult(F):-
  foreach(
    term_of(F,T),
    if(T=(':-'(G)),G,x_assertz(T))
  ).

x_asserta(C0):- ##x_trace(x_assert(a,C0),TTT),TTT.

x_assertz(C0):- ##x_trace(x_assert(z,C0),TTT),TTT.

x_assert(Op,C0):-
  ensure_clause(C0,H,B),
  functor(H,F,N),
  x_dynamic_pred(H,Pred),
  C=(H:-B),
  gensym_no(Pred,R),
  x_map_op(Op,Pred,R,C),
  ( N>0,x_indexable(1,H,A1),
    x_assert_op(Op,'$arg'(1,F,N,A1),R),
    fail
  ; N>1,x_indexable(2,H,A2),
    x_assert_op(Op,'$arg'(2,F,N,A2),R),
    fail
  ; N>1,x_indexable(1,H,A1),x_indexable(2,H,A2),
    x_assert_op(Op,'$arg'(1,2,F,N,A1,A2),R),
    fail
  ; true
  ).

x_map_op(a,D,K,R):-map_put(D,K,R).
x_map_op(z,D,K,R):-map_put(D,K,R).

x_assert_op(a,K,R):-hash_push(K,R).
x_assert_op(z,K,R):-hash_put(K,R).

x_indexable(I,H,_A):-val(I,H,'$unindexed'),!,fail.
x_indexable(I,H,A):-arg(I,H,A),atomic(A),!.
x_indexable(I,H,A):-bb_let(I,H,'$unindexed'),!,
  mmap_put(unx,I,H=>A),
  fail.

x_indexed(I,H,_A):-val(I,H,'$unindexed'),!,fail.
x_indexed(I,H,A):-arg(I,H,A),atomic(A).

x_dynamic(F/N):-
  functor(H,F,N),
   %##x_trace(
     x_dynamic_pred(H,_)
   %,TTT),TTT
  .

x_dynamic_pred(H,D):-nonvar(H),x_is_dynamic_pred(H,X),!,D=X.
x_dynamic_pred(H,D):-functor(H,F,N),functor(H0,F,N),map_new(D),map_put('$dynamic',H0,D).

x_is_dynamic(H):- 
  %##x_trace(
    x_is_dynamic_pred(H,_)
  %,TTT),TTT
  .

x_is_dynamic_pred(H,D):-map_get('$dynamic',H,D).

x_clause(H,B):-x_clause(H,B,_Ref,_A).

x_clause(H,B,Ref):-x_clause(H,B,Ref,_A).

x_clause(H,B,Ref,A):-
  x_clause0(H,Ref,A),
  x_is_dynamic_pred(H,Pred),
  map_get(Pred,Ref,(H:-B)).
   
x_clause0(H,R,A):-
  functor(H,F,N),
  !,
  x_clause_bound(F,N,H,R,A).
x_clause0(H,R,_A):-
  ##x_trace(x_clause_unbound(H,R),TT),TT.
  
x_clause_is_indexed(F,N,H,Arg):- 
   N>1,x_indexed(1,H,A1),x_indexed(2,H,A2),
   !, 
   Arg='$arg'(1,2,F,N,A1,A2).
x_clause_is_indexed(F,N,H,Arg):-
   N>0,x_indexed(1,H,A1),
   !,
   Arg='$arg'(1,F,N,A1).
x_clause_is_indexed(F,N,H,Arg):-
   N>1,x_indexed(2,H,A2),
   Arg='$arg'(2,F,N,A2).
   
x_clause_bound(F,N,H,C,Arg):-
  x_clause_is_indexed(F,N,H,Arg),
  !,
  ##x_trace(x_clause_indexed(Arg,C),TT),TT.
x_clause_bound(_F,_N,H,C,_):- 
  ##x_trace(x_clause_unindexed(H,C),TT),TT.

x_clause_indexed(Arg,C):-hash_get(Arg,C).  

x_clause_unindexed(H,C):-x_clause_search(H,C).
x_clause_unbound(H,C):-x_clause_search(H,C).

x_clause_search(H,C):-   
   x_is_dynamic_pred(H,Pred),
   map_key(Pred,C).

x_gc_args(H,Ref):-
  functor(H,F,N),
  x_retract_args(F,N,H,Ref,_Arg),
  fail.
x_gc_args(_H,_Ref).
       
x_retract_args(F,N,H,C,Arg):- 
  N>1,x_indexed(1,H,A1),x_indexed(2,H,A2),
  Arg='$arg'(1,2,F,N,A1,A2),
  hash_rm(Arg,C).
x_retract_args(F,N,H,C,Arg):-
  N>0,x_indexed(1,H,A1),
  Arg='$arg'(1,F,N,A1),
  hash_rm(Arg,C).
x_retract_args(F,N,H,C,Arg):-
  N>1,x_indexed(2,H,A2),
  Arg='$arg'(2,F,N,A2),
  hash_rm(Arg,C).

x_retract(H):- x_retract(H,_B).
 
x_retract(H,B):-
  x_clause(H,B,Ref),
  x_is_dynamic_pred(H,Pred),map_rm(Pred,Ref),
  ##x_trace(x_gc_args(H,Ref),TT),TT.

x_retractall(H):-
  ##x_trace(x_retract_each(H),TTT),TTT,
  ##x_trace(x_finalize(H),RRR),RRR.

x_abolish(F/N):-
  functor(H,F,N),
  x_retractall(H),
  ( map_get('$dynamic',H,D),
    map_clear(D),
    map_rm('$dynamic',H)
  ->true
  ; true
  ).
  
x_finalize(H):-  
  functor(H,_,N),
  for(I,1,N),
  arg(I,H,A),
  nonvar(A),
  !.
x_finalize(H):-
  nonvar(H),
  % println(trace=H),
  x_finalize_known(H).

x_finalize_known(H):-  
  x_is_dynamic_pred(H,Pred),
  !,
  % map_show(Pred),
  map_clear(Pred),
  init_gensym(Pred).
 
x_retract_each(H):-
  x_retract(H),
  fail.
x_retract_each(_).  

x_gc:-
  ctime(T0),
  call_ifdef(hash_gc,true),
  ctime(T1),
  call_ifdef(mmaps_gc,true),
  ctime(T2),
  call_ifdef(maps_gc,true),
  ctime(T3),
  T10 is T1-T0,
  T21 is T2-T1,
  T32 is T3-T2,
  quiet(Q),
  if(Q<2,println(time=x_gc(hash_gc=T10,mmaps_gc=T21,maps_gc=T32)),true),
  fail.
x_gc.


ensure_clause((H:-B),H,B):-!.
ensure_clause(H,H,true).

/*
x_listing:-this_db(Db),mmap_show(Db).
x_listing(F/N):-this_db(Db),functor(H,F,N),foreach(mmap_get(Db,H,C),pp_clause(C)).
x_consult(F):-foreach(term_of(F,T),if(T=(':-'(G)),G,x_assertz(T))).
x_asserta(C):-this_db(D),ensure_clause(C,H,B),mmap_push(D,H,(H:-B)).
x_assertz(C):-this_db(D),ensure_clause(C,H,B),mmap_put(D,H,(H:-B)).
x_retract(H):-x_retract(H,_B).
x_retract(H,B):-this_db(D),mmap_rm(D,H,(H:-B)).
x_retractall(H):-x_retract(H),fail.
x_retractall(_). %:-this_db(D),map_gc(D).  
x_clause(H,B):-this_db(D),mmap_get(D,H,(H:-B)).
x_is_dynamic(H):-this_db(D),val(D,H,_).
x_dynamic(F/N):-functor(H,F,N),x_asserta(H),x_retract(H),!.
x_gc.
*/
