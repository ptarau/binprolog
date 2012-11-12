% generalized indexing on ground term key
% actually the term is only inspected for groundnes
% at this point up to its args top functor - except for
% list keys which are explored to the end
      
% adds one

set_hash_max(M):-let('$hash_max',default,M).

hash_max(M):-val('$hash_max',default,X),!,M=X.
hash_max(1999).
% hash_max(7). %4001 8009 12007 16001 32003 64007 96001

hash_of(T,N):-hash_max(M),deep_hash(T,64,M,X),!,N is -(1+X).
hash_of(T,_N):-errmes(hash_key_should_be_ground,T).

hash_key(K):-is_compiled('$hx'(_,_,_)),!,'$hx'(K,_,_).
hash_key(K):-hash_key0(K),once(mmap_get(K,_,_)).

hash_key0(K):-hash_max(M),for(I,1,M),K is -abs(I).

hash_push(GroundKey,Term):-
  hash_check_not_compiled(hash_push,GroundKey,Term),
  hash_of(GroundKey,K),
  mmap_push(K,GroundKey,Term).
  
hash_put(GroundKey,Term):-
  hash_check_not_compiled(hash_put,GroundKey,Term),
  hash_of(GroundKey,K),
  ##hash_trace(mmap_put(K,GroundKey,Term),TT),TT.

hash_check_not_compiled(Op,GroundKey,Term):-is_compiled('$hx'(_,_,_)),!,
  errmes(
    unable_to_update_compiled_hash_maps_for(Op),
    on(GroundKey,Term)
  ).
hash_check_not_compiled(_Op,_GroundKey,_Term).

% gets each

hash_get(Key,Term):-var(Key),!,hash_key(K),hash_get0(K,Key,Term).
hash_get(GroundKey,Term):-hash_of(GroundKey,K),hash_get0(K,GroundKey,Term).

hash_get0(K,Key,Term):-is_compiled('$hx'(_,_,_)),!,'$hx'(K,Key,Term).
hash_get0(K,GroundKey,Term):- ##hash_trace(mmap_get(K,GroundKey,Term),TT),TT.

% removes first
hash_rm_one(GroundKey,Term):-
  hash_check_not_compiled(hash_rm_one,GroundKey,Term),
  hash_of(GroundKey,K),
   ##hash_trace(mmap_rm_one(K,GroundKey,Term),TT),TT.

hash_rm(GroundKey,Term):-
  hash_check_not_compiled(hash_rm,GroundKey,Term),
  hash_of(GroundKey,K),
  ##hash_trace(mmap_rm(K,GroundKey,Term),TT),TT.
  
% removes all for a key
% hash_clear(Key):-var(Key),!,hash_clear.
hash_clear(GroundKey):-
  hash_check_not_compiled(hash_clear/1,GroundKey,'?'),
  ##hash_trace(hash_clear(GroundKey,_Term),TT),TT.

hash_clear(GroundKey,Term):-
  hash_of(GroundKey,K),
  mmap_rm(K,GroundKey,Term),
  fail.
hash_clear(GroundKey,Term):-
  hash_check_not_compiled(hash_clear/2,GroundKey,Term).
  
% removes all from all keys
hash_clear:-hash_key0(K),mmap_clear(K),fail.
hash_clear.

hash_gc:-hash_key0(K),mmap_gc(K),fail.
hash_gc.

hash_save(File):-hash_save(File,'$h').

hash_save(File,P):-
  tell(File),
  functor(Rel,P,2),
  arg(1,Rel,K),
  arg(2,Rel,Cs),
  hash_key(K),
    findall(Key-Term,hash_get0(K,Key,Term),KsTs),
    findall(Key-Term,
      keygroup(KsTs,Key,Term),
    Cs),
    pp_clause(Rel),
  fail.
hash_save(_,_):-
  told.

hash_load(File):-
   hash_clear,
   term_of(File,T),
   arg(2,T,Xs),
   foreach(
      and(
        member(K-Vs,Xs),
        member(V,Vs)
      ),
      hash_put(K,V)     
   ).

hash_compile:-
  ctime(T1),
  hash_compile('$d','$hx'),
  ctime(T2),
  T is T2-T1,
  ttyprint(['hash maps compiled, time':T,'warning: only hash_get operations allowed!']).
 
hash_compile(Db,P):-
  Rel=(H:-B),
  functor(H,P,3),
  arg(1,H,K),
  arg(2,H,Key),
  arg(3,H,C),
  B=(member(Key-Cs,Css),member(C,Cs)),
  hash_key(K),
    findall(Key-Term,hash_get0(K,Key,Term),KsTs),
    findall(Key-Term,
      keygroup(KsTs,Key,Term),
    Css),
    db_assert(Db,Rel),
  fail.
hash_compile(Db,P):-
  functor(H,P,3),
  dyn2stat(Db,H),
  hash_clear,
  db_clean(Db).
  
% like findall, but only keeps unique answers

hash_find_unique(X,G,Xs):-
  gensym('$find_unique',S),
  generate_find_unique(S,X,G),
  collect_find_unique(S,Xs).

generate_find_unique(S,X,G):-  
  G,
  ( hash_get(X,S)->true
  ; hash_put(X,S)->true
  ; errmes('should_be_ground_in_find_unique'(G),X)
  ),
  fail.
generate_find_unique(_S,_X,_G).  

collect_find_unique(S,Xs):-
  findall(X,hash_get(X,S),Xs),
  ( hash_key(K),
    mmap_rm(K,_,S),
    mmap_gc(K),
    fail
  ; true
  ).

test_find_unique(X,G):-
  findall(X,G,All),
  hash_find_unique(X,G,Xs),
  println(all=All),
  println(unique=Xs).

test_find_unique:-  
  G=(for(X,1,5);for(X,2,7)), 
  test_find_unique(X,G),
  fail 
; G=member(X,[f(a,b),f(b,a),f(a,b),c,f(b,a)]),  
  test_find_unique(X,G),
  fail
; G=member(X,[c,f(a,B),f(B,a)]), % error!!! 
  test_find_unique(X,G),
  fail  
; true.


