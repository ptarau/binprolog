:-write('computes primes using bboard operations for fast tabultation'),nl.

max_prime(5000).

go:-max_prime(Max),go(Max).

cputime(T):-statistics(runtime,[_,T]).

go(N):-cputime(_),primes_to(N),cputime(T),write(time=T),nl,statistics.

primes_to(N):-
	init_primes,
	prime(2,N,Prime),
      Prime>N-100, 
	write(Prime),nl,
	fail
; true.

init_primes:-def(prime,limit,2),!,push(primes,2).
init_primes.

prime(Min,Max,P) :- 
	range(P,Min,Max),
		sqr(P,Lim),
		\+(divisible(P,Lim)).

divisible(P,Lim):-
	memo_prime(Lim,I),
	0 is P mod I.

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
	Min<Max,
	Min1 is Min+1,
	range(I,Min1,Max).

sqr(N,S):-N1 is N//2+2,range(S1,1,N1),S1*S1>N,!,S is S1-1.

memo_prime(Lim,I):-Lim>2,
	val(prime,limit,Old),Old>=Lim,!,
	stack(primes,Ps),
	member(I,Ps).
memo_prime(Lim,I):-Lim>2,
	val(prime,limit,Old),Old<Lim,
	prime(Old,Lim,I),
	push(primes,I),
	set(prime,limit,I),
	fail.
memo_prime(Lim,I):-
	set(prime,limit,Lim),
	stack(primes,Ps),
	member(I,Ps).
