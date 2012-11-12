app(nil,Ys,Ys).
app(A^Xs,Ys,A^Zs):-
  app(Xs,Ys,Zs).

nrev(nil,nil).
nrev(X^Xs,R):-
  nrev(Xs,T),
  app(T,X^nil,R).

full_range(It,L):- range(_,1,It),nrev(L,_), fail.
full_range(_,_).

dummy(_,_).

empty_range(It,L):-range(_,1,It),dummy(L,_),fail.
empty_range(_,_).

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).

integers(nil,I,I):-!.
integers(I0^L,I0,I):-I0<I,I1 is I0+1,integers(L,I1,I).

bm(It,Len,Time,Lips):-
	integers(L,0,Len),
	timer(T0),
	empty_range(It,L),
	timer(T1),
	full_range(It,L),
	timer(T2),
	Time is (T2-T1)-(T1-T0),
	L1 is Len+1,
	L2 is Len+2,
	LI is (L1*L2)/2,
	LIs is It*LI,
	Lips is (LIs/Time)/1000.0.

htest(N,H,T,S):-
        integers(Is,0,N),
        statistics(global_stack,[H1,_]),
        statistics(trail,[T1,_]),
        statistics(local_stack,[S1,_]),
        nrev(Is,_),
        statistics(global_stack,[H2,_]),
        statistics(trail,[T2,_]),
        statistics(local_stack,[S2,_]),
        H is H2-H1,T is T2-T1,S is S2-S1.

go(Mes,Len,It):-
	bm(It,Len,T,L),
        nl,write(Mes=[klips=L]),nl,
	write([time=T,length=Len,iterations=It]),nl,
	htest(Len,H,Tr,S),
	nl,write(Mes=[heap=H,trail=Tr,stack=S]),nl.

gg:-go(50,100).

g0:-go(100,200).

g1:-go(100,800).

g2:-go(200,200).

g3:-write('use bp -h20000'),nl,go(2000,1).

go(L,I):-go(test,L,I).

go:-go('BMARK_brev:').

go(Mes):-
	Len=300,
	It=100,
	go(Mes,Len,It).

timer(T):-statistics(runtime,[T0,_]),T is T0/1000.0.
%timer(T):-statistics(cputime,T).
%timer(T):-T is cputime.

p:-[brev].
