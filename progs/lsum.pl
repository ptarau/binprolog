/*

Hi Veronica and Jamie,

Here are some thoughts following the linear logic paper and a
previous message.

Jamie writes:

>I have been thinking also about some
>possible source-to-source translation for clauses that would
>allow for more efficient compilation.  Lygon looks interesting,
>I didn't realize it was based on BinProlog.

An interesting way to enforce a resource-driven execution to
Prolog programs is the following (# is BinProlog's Hidden Argument
Grammar equivalent of DCGs 'C'/3):
*/

n(0) :- #t.
n(s(N)):- #t, n(N).

sum(0,N,N) :- #t.
sum(s(N),M,s(K)):- #t, sum(N,M,K).

prod(0,_,0) :-  #t.
prod(s(N),M,P):- #t, prod(N,M,P1), sum(P1,M,P).


make_tokens(N,Ts):-findall(t,for(_,1,N),Ts).

go(N,X*Y=Z):-
   make_tokens(N,Ts),
   dcg_def(Ts),
     prod(X,Y,Z),
   dcg_val([]).

go:-go(10,R),write(R),nl,fail.

/*
?- go.

s(s(0))*s(s(s(s(s(0))))) = s(s(s(s(s(s(s(s(s(s(0))))))))))
s(s(s(0)))*s(0) = s(s(s(0)))

If each use of a linear  clause has it's `own' #t resource guard (say
#t_n_1_1, #t_n_1_2, #t_sum_3_1, #t_sum_3_2, etc.) and the grammar is
initialised with exactly 1 token for each of them, then a reasonably
fast implementation is obtained at source level. (Heavy operations
like clause selection and unification are still compiled. Using
exactly once each clause in a proof is ensured by the fact that
the grammar consumes the `usage' flag of the clause which will fail
afterwards).

In principle real DCGs can be used instead of HAGs so that this
translates to a definite clause program.  The effect of # tokens is
simulated at WAM-level with a flag subject to value-trailing, so
linearity of statically known predicates can be made a very cheap
operation (probably within +-5% of ordinary Prolog execution time).

The more general idea beyond the example is the use of grammars to
specify/limit use of computational ressources. Linearity is an instance.
Iterative deepening can also be emulated by giving progressively
larger sets of tokens.

More complex constraints like "using exactly one among a set of
clauses" can be expressed by shared tokens. If a frequently occurring
subset of the constructs supported by Lygon or Lolli fits in this scheme
a speed-up of at least one order of magnitude can be expected.

Cheers,

Paul

P.S. I am wondering if grammars with restrictions on the number of times
a rule can be used have been investiagted. Maybe Veronica has some
hints about this. Clearly counters in parametric grammars can be used
to obtain this effect too.
*/
