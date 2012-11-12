/*
> Using the NU-Prolog library predicate mapList:
>
>	head(H._, H).
>	convert(LL, L) :- mapList(head, LL, L).
>


Is mapList efficient enough in NU-Prolog so that you suggest its use?

I like mapList-style constructs myself but I know they
are may be quite inefficient unless something nice is
done by the implementation.

The best thing that can happen to mapList(Closure,I,O) is to be
macro-expanded by the preprocessor to its first-order equivalent.

However if Closure is not known at compile-time, a general
implementation is needed. Even if Closure is just
the name of a functor this will be still slower then
macro-expansion `by-hand' or `by-preprocessor'.

Unless maplist is written in C,
one of the following 2 implementations (maplist0 or maplist)
or their variants are likely to be used:
*/

% --------------------- CUT HERE --------------------------------

go:-go(10000).

% straightforward maplist
maplist0(FXs,Is,Os):-
  maplist1(Is,FXs,Os).

maplist1([],_,[]).
maplist1([I|Is],Closure,[O|Os]):-
	% suppose ground(Closure), otherwise copy_term is needed
	Closure=..L1,
	concat(L1,[I,O],L2),
	P=..L2,
	P,
	maplist1(Is,Closure,Os).

% maplist with findall
maplist(Closure,Is,Os):-
	Closure=..L1,
	concat(L1,[I,O],L2),
	P=..L2,
	findall(O,map1(P,I,Is),Os).

map1(P,I,Is):-memb(I,Is),P.

concat([],Ys,Ys).
concat([X|Xs],Ys,[X|Zs]):-concat(Xs,Ys,Zs).

memb(X,[X|_]).
memb(X,[_|Xs]):-memb(X,Xs).

% benchmarking stuff
make_ints([],I,I):-!.
make_ints([I0|L],I0,I):-I0<I,I1 is I0+1,make_ints(L,I1,I).

% data

plus(A,B,C):-C is A+B.

% test

time(T):-statistics(runtime,[_,T]).

test(R1=R2):-
	maplist0(plus(1),[10,20,30],R1),
	maplist(plus(1),[10,20,30],R2).

go(Max):-test(Ok),write(Ok),nl,
	make_ints(Is,1,Max),
	time(_),maplist0(plus(1),Is,_),time(T0),
	maplist(plus(1),Is,_),time(T),
	write([straightforward=T0,with_findall=T]),nl.

/* 
Both, but especially maplist0 can get some help from
call/N predicates if available but this do not change
their relative timings too much.

The interesting thing I found out is that, for this benchmark at least,
in BinProlog 2.10, the findall based implementation is 7 times
faster than the straightforward one, while being faster in absolute
terms than any of them on other (even native code!) Prologs.
This actually takes advantage of the efficiency of BinProlog's
`copy_once' findall but it also shows that obvious implementations
are not so obvious after all.

Here are the timings for BinProlog, Quintus and Sicstus
on a Sparcstation 1.

------------ BinProlog 2.10 -----(bp -h4000)-----------
[straightforward = 4217,with_findall = 650] 
------------ Quintus 3.0 ------------------------------
[straightforward=5334,with_findall=8416]
------------- Sicstus 2.1_6 native --------------------
[straightforward=1909,with_findall=3370]
------------- Sicstus 2.1_6 emulated ------------------
[straightforward=2429,with_findall=3740]
-------------------------------------------------------
I am curious what's happening in other Prologs ...

I always found mapList-style predicates very convenient
but I was reluctant to use their `library versions'
because of lack of efficiency.

The main point is that if findall has (very) little overhead
then it can be used for all the family of map-predicates
in a similar way.

Paul Tarau
*/

