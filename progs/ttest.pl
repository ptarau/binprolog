nobug:-
   tstest(attr_ow(pax(1),type),attr_ow(pax(1),age),X),
   println(result=X).
   
go:-
  tstest(key(k1,k2),value(a,b,c),R),println(result=R),
  N=1000000,
  T=f(0,a,X,g(X),h(1,314)),
  new_term(T,I),
  println(handle:T=>I),
  instance_of(I,T1),
  println(instance:I=>T1),
  instance_of(I,T2),
  println(instance:I=>T2),
  free_term(I),
  println(T=T1),
  (T=T1->println(equal);println(not_equal)),
  ctime(S1),
  ( for(_,1,N),
      new_term(T,Handle),
      instance_of(Handle,_),
      free_term(Handle),
    fail
  ; true
  ),
  ctime(S2),
  S0 is S2-S1,
  S is S0/3000.00001,
  OT is S/N,
  ST is N/S,
  println([create_free(N), total=S0, total_ms_per_op=S,one=OT, per_sec=ST]).

go2:-
  K=f(a,b),
  push_term(K,g(c,d)),
  T0=h(f(123,b,U),g(m,3.14,U),n),
  put_term(K,T0),
  count_terms(K,N),
  new_iterator(K,Iter),
  println(t0=T0),
  println([count=N,iter=Iter]),
  ( for(I,1,N),
    get_next_term(Iter,T),
    println(K:I=>T),
    fail
  ;  close_iterator(Iter)
  ),
   delete_all_terms(K),
  (has_terms(K)->println(has_terms);println(no_terms_left)).

   
go1:-
  T0=f(a,X,[1,2],g(314,X),77),
  put_term(111,T0),
  new_iterator(a,I),
  get_next_term(I,T),
  println(T0=T),
  T=T0,
  println(equal).

