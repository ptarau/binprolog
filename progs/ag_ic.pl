:-op(200,xf,(?)).

ic([a,b,c]).
ic([d,e]).

violates(Abduced,IC):-
  select(X,Abduced,More),
  select(Y,More,_),
  member(X,IC),
  member(Y,IC).

inconsistent(Abduced):-ic(IC),violates(Abduced,IC).

select(X,[X|Xs],Xs).
select(X,[Y|Xs],[Y|Ys]):-select(X,Xs,Ys).

explain(G,Explanation):-
  G,
  \+ (
    -Abduced,
    inconsistent(Abduced)
  ),
  findall(Abduced,-Abduced,As),
  sort(As,Explanation).

/*
 to add an instance A of an abducible do:
 
 for all instances B of the same abducible
   take out B
   if B is more general than A, then succeed
   else
     remove B, 
     if A does not violate any constraints
     then add A
     else fail
*/

pick_subsumed(A,Bs):-
  functor(A,F,N),
  functor(B,F,N),
  -B,
  ( subsumes_check(A,B)->Bs=[B|MoreBs]
  ; Bs=MoreBs
  ),
  !,
  pick_subsumed(A,MoreBs).
pick_subsumed(_,[]).

add_abduced(A):-
  - abduced(B),
  ( -B, subsumes_check(B,A) -> +B
  ; +A
  ).

'?'(X):-add_abduced(X).
  
go:-explain(g,Explanation),write(Explanation),nl,fail.
go.

g:- a?,h?,i.
h:- i,b?,c? .
h:- i,d?,c? .
i:- f? .
