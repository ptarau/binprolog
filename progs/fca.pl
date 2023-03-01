go:-
	findall(C,concept(C),Cs),
	set_member(C,Cs),
	  write(C),nl,
	fail.
go.

% planets & attributes: must be sorted

planet(earth,[moon,near,small]).
planet(jupiter,[far,large,moon]).
planet(mars,[moon,near,small]).
planet(mercury,[near,single,small]).
planet(neptune,[far,medium,moon]).
planet(pluto,[far,moon,small]).
planet(saturn,[far,large,moon]).
planet(uranus,[far,medium,moon]).
planet(venus,[near,single,small]).

lcontext(L,Rs):-planet(L,Rs).

/*
% usage

?- go.
[jupiter,saturn]-[far,large,moon]
[neptune,uranus]-[far,medium,moon]
[jupiter,neptune,pluto,saturn,uranus]-[far,moon]
[pluto]-[far,moon,small]
[earth,jupiter,mars,neptune,pluto,saturn,uranus]-[moon]
[earth,mars]-[moon,near,small]
[earth,mars,pluto]-[moon,small]
[mercury,venus]-[near,single,small]
[earth,mars,mercury,venus]-[near,small]
[earth,mars,mercury,pluto,venus]-[small]
true.

*/


% Finds the lattice of concepts defined on on context
% as defined at pp. 221-236 in Davey & Priestley,
% Introduction to Lattices and Order (Cambridge Univ. Press, 1990)
% the theory of Formal Concept Analysis is due to R. WILLE

concept(Es-Is):-
	intent(Is),
	intent2extent(Is,Es).

gen_all:-
	findall(X,gen_one(X),Is),
	fill_instr(Is,0,_),
	pp_is(Is).

gen_one(wam(_,L)):-
	set_member(X,[deep,top]),
	set_member(Y,[head,body]),
	set_member(Z,[constant,structure,value,variable]),
	sort([X,Y,Z],L).

fill_instr([],N,N).
fill_instr([wam(N1,_)|Is],N1,N2):-
	N is N1+1,
	fill_instr(Is,N,N2).
	
pp_is(Is):-set_member(I,Is),write(I),write('.'),nl,fail.
pp_is(_).

context(L,R):-lcontext(L,Rs),set_member(R,Rs).

rcontext(R,Ls):-setof(L,context(L,R),Ls).

lset(Ls):-findall(L,lcontext(L,_),Ls).
rset(Rs):-findall(R,rcontext(R,_),Rs).

an_intent([I|Is]):-
	lset(Ls),
	subset(Ls,Xs),
	extent2intent(Xs,[I|Is]).

intent(Is):-
	findall(Is,an_intent(Is),Unsorted),
	sort(Unsorted,Iss),
	set_member(Is,Iss).

  
% tools

extent2intent([X|Xs], Ans) :- lcontext(X,L),intersect_extents(Xs, L, Ans).

intent2extent([X|Xs], Ans) :- rcontext(X,R),intersect_intents(Xs, R, Ans).

intersect_extents([], Ans, Ans).
intersect_extents([X|Xs], Ans0, Ans) :-
	lcontext(X,L),
	ord_intersect(Ans0, L, Ans1),
	intersect_extents(Xs, Ans1, Ans).

intersect_intents([], Ans, Ans).
intersect_intents([X|Xs], Ans0, Ans) :-
	rcontext(X,R),
	ord_intersect(Ans0, R, Ans1),
	intersect_intents(Xs, Ans1, Ans).

ord_intersect(_, [], []) :- !.
ord_intersect([], _, []) :- !.
ord_intersect([Head1|Tail1], [Head2|Tail2], Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2, Intersection).
 
ord_intersect(=, Head,  Tail1, _,     Tail2, [Head|Intersection]) :-
	ord_intersect(Tail1, Tail2, Intersection).
ord_intersect(<, _,     Tail1, Head2, Tail2, Intersection) :-
	ord_intersect(Tail1, [Head2|Tail2], Intersection).
ord_intersect(>, Head1, Tail1, _,     Tail2, Intersection) :-
	ord_intersect([Head1|Tail1], Tail2, Intersection).
 
subset([],[]).
subset([_|Xs],Ys):-subset(Xs,Ys).
subset([X|Xs],[X|Ys]):-subset(Xs,Ys).

set_member(X,[X|_]).
set_member(X,[_|Xs]):-
	set_member(X,Xs).
  