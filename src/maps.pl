% global 2key -> 1 value associations

global_get(A,B,X):-global_get0(A,B,X,_,_).

global_rm(A,B):-global_get0(A,B,_,K,Xs),!,g_trim(K,Xs).
global_rm(_,_).

g_trim(K,[]):-!,bb_rm('$global',K).
g_trim(K,Xs):-bb_set('$global',K,Xs).

global_get0(A,B,X,K,Ys):-
  g_key(A,B,X, K,T), 
  bb_val('$global',K,Xs),
  !,
  g_sel(T,Xs,Ys).
  
global_set(A,B,X):-
  nonvar(X),
  g_key(A,B,X, K,T),
  g_set_k(K,T).
  
g_set_k(K,T):-  
  bb_val('$global',K,Xs),
  !,
  g_update(T,Xs,NewXs),
  bb_set('$global',K,NewXs).
g_set_k(K,T):-
  bb_def('$global',K,[T]).

g_key(A,B,X, K,G-X):-G='$2'(A,B),deep_hash(G,16,1999,K).
 
g_update(G-X,Xs,Ys):-g_sel(G-_,Xs,NewXs),!,Ys=[G-X|NewXs].
g_update(T,Xs,[T|Xs]).

g_sel(G-X,[G1-Y|Xs],Xs):-G==G1,!,X=Y.
g_sel(T,[S|Xs],[S|Ys]):-g_sel(T,Xs,Ys).

% maps with multiple values

mmap_new(D):-gensym('$mmap',D).

mmaps_iterate(D):-M='$mmap',K='$mkeys',val(gensym,M,Last),for(I,1,Last),symcat(M,I,D),val(K,D,_).
mmaps_clean:-foreach(mmaps_iterate(D),mmap_clear(D)),init_gensym('$mmap').
mmaps_gc:-foreach(mmaps_iterate(D),mmap_gc(D)).
mmaps_show:-foreach(mmaps_iterate(D),mmap_show(D)).

mmap_put(D,K,V):-val(D,K,_),!,addq(D,K,V).
mmap_put(D,K,V):-addq(D,K,V),addq('$mkeys',D,K).

mmap_push(D,K,V):-val(D,K,_),!,pushq(D,K,V).
mmap_push(D,K,V):-pushq(D,K,V),addq('$mkeys',D,K).

mmap_get(D,K,V):-mmap_key(D,K),cmembq(D,K,V).

mmap_rm_one(D,K,X):-mmap_key(D,K),cdelq(D,K,X,X).

mmap_rm(D,K,X):-mmap_key(D,K),cdelq_any(D,K,X).

mmap_rm_all(D,K):-val(D,K,_),rm(D,K).

/*
mmap_rm(D,K,X):-mmap_key(D,K),mmap_rm(D,K,X,X).

mmap_rm(D,K,X,NewR):-cdelq(D,K,X,R),mmap_rm_any1(D,K,X,R,NewR).

mmap_rm_any1(_D,_K,_X,R,R).
mmap_rm_any1(D,K,X,_R,NewR):-mmap_rm(D,K,X,NewR).
*/

mmap_gc(D):-
  membq('$mkeys',D,K),
  mmap_gc_one(D,K),
  fail.
mmap_gc(D):-
  \+membq('$mkeys',D,_),
  val('$mkeys',D,_),
  rm('$mkeys',D),
  fail.  
mmap_gc(_).  

mmap_gc_one(D,K):- \+membq(D,K,_),cdelq('$mkeys',D,K,_),fail.
mmap_gc_one(D,K):- \+membq(D,K,_),mmap_rm_all(D,K).

% implicitely also performs mmap_gc for D
mmap_clear(D):-membq('$mkeys',D,K),rm(D,K),fail.
mmap_clear(D):-val('$mkeys',D,_),rm('$mkeys',D),fail.
mmap_clear(_).

mmap_key(D,K):-nonvar(K),!,val(D,K,_).
mmap_key(D,K):-cmembq('$mkeys',D,K).

mmap_show(D):-mmap_get(D,K,V),println(D:K=>V),fail;nl.

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

ensure_clause((H:-B),H,B):-!.
ensure_clause(H,H,true).
*/

map_new(D):-gensym('$map',D).

maps_iterate(D):-M='$map',K='$keys',val(gensym,M,Last),for(I,1,Last),symcat(M,I,D),val(K,D,_).
maps_clean:-foreach(maps_iterate(D),map_clear(D)),init_gensym('$map').
maps_gc:-foreach(maps_iterate(D),map_gc(D)).
maps_show:-foreach(maps_iterate(D),map_show(D)).

map_put(D,K,V):-val(D,K,_),!,bb_set(D,K,V).
map_put(D,K,V):-bb_def(D,K,V),addq('$keys',D,K).

map_get(D,K,V):-map_key(D,K),bb_val(D,K,V).

map_rm(D,K):-map_key(D,K),rm(D,K).

map_gc(D):-membq('$keys',D,K),\+val(D,K,_),cdelq('$keys',D,K,_),fail.
map_gc(_).

map_clear(D):-membq('$keys',D,K),val(D,K,_),rm(D,K),fail.
map_clear(D):-val('$keys',D,_),rm('$keys',D),fail.
map_clear(_).

map_key(D,K):-nonvar(K),!,val(D,K,_).
map_key(D,K):-cmembq('$keys',D,K).

map_show(D):-map_get(D,K,V),println(D:K=>V),fail;nl.

map_show_(D):-cmembq('$keys',D,K0),copy_term(K0,K),(bb_val(D,K,V)->true;V='??'),println(D:K=>V),fail;nl.
mmap_show_(D):-cmembq('$mkeys',D,K0),copy_term(K0,K),(val(D,K,_)->cmembq(D,K,V);V='??'),println(D:K=>V),fail;nl.

/*
md:-
  mmap_new(D),
  mmap_put(D,a(X),f(X,X)),
  mmap_put(D,b,g(a)),
  mmap_put(D,c,h(X,f(X))),
  mmap_put(D,c,99),
  println(mmap_rm),
  foreach(mmap_rm(D,c,X),println(c=>X)),
  mmap_show_(D),
  mmap_rm_one(D,b,X),
  println(removed=(b:X)),
  mmap_show_(D),
  mmap_gc(D),
  println(after_gc),
  mmap_show_(D),
  mmap_clear(D),
  mmap_show(D).
  
 dd:-
  map_new(D),
  map_put(D,a(X),f(X,X)),
  map_put(D,b,g(a)),
  map_put(D,b,99),
  map_put(D,c,h(X,f(X))),
  map_rm(D,b),
  map_show_(D),
  map_gc(D),
  println(after_gc),
  map_show_(D),
  map_clear(D),
  map_show_(D).
 
*/