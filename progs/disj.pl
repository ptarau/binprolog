:-module(prolog).

disj(A,B,NewAB):-
        avoid_replacing_cut((A;B)),!,
        r_disj0(A,B,NewAB).
disj(A,B,Head):-
	make_new_head(or(A,B),Head),
	r_disj((A;B),Ns,[]),
        delay_disj(Head,Ns).


delay_disj(Head,Ns):-
        member(N,Ns),
          compile_later(Head,N),
        fail.
delay_disj(_,_).

r_disj0(If,C,if(NewA,NewB,NewC)):-nonvar(If),If=(A->B),!,
        repl_body(A,NewA),
        repl_body(B,NewB),
        repl_body(C,NewC).
r_disj0(A,B,or(NewA,NewB)):-
        repl_body(A,NewA),
        repl_body(B,NewB).


r_disj((A;B))-->!,
  r_disj(A),
  r_disj(B).
r_disj((A->B))-->!,
  { repl_body(A,NA),
    repl_macro('!',CUT),
    repl_body(B,NB)
  },
  [(NA,CUT,NB)].
r_disj(A)-->
  {repl_body(A,NA)},
  [NA].



:-module(user).

go:-X=(m;(n1;n2);p,q),'prolog:disj'(X,(a->b;c,X;d),R),
  pp_clause(R),nl,
  listing.
