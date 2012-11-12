% bp -q0 -t2000
go:-go(100000).

go(N):-go(N,L),println(L).

go(N,L):-
  gen_cs(N,Xs),
  #>Xs,
  get_cs(Cs),
  #<[],
  length(Cs,L).

get_cs([C|Cs]):- #C,!,get_cs(Cs).
get_cs([]).

gen_cs(0,[]):-!.
gen_cs(I,[C|Cs]):-
  I>0,
  I1 is I-1,
  C is 0'a + I mod 26,
  gen_cs(I1,Cs).
