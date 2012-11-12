% permutation with linear assumptions
% to be accelerated with a compiled implementation of linear assumptions

max(7).

t1(Mes):-test(Mes,lin_perm,g1).

t2(Mes):-test(Mes,lperm,g2).

t3(Mes):-test(Mes,lin_perm_with_findall,g3).

go(Mes):-
  write('execute with -h20000 option'),nl,
  t1(Mes),fail;t2(Mes),fail;t3(Mes),fail.

go:-go('BMARK_linperms').

nats(Max,Max):-!,assumel(n(Max)).
nats(Curr,Max):-
	Curr<Max,
        assumel(n(Curr)),
	Curr1 is Curr+1,
	nats(Curr1,Max).

perm([]):- \+ assumed(n(_)).
perm([X|Xs]):-
  assumed(n(X)),
  perm(Xs).

g1(N,P):-nats(1,N),perm(P),fail.
g1(_,_).

g2(N,P):-nats(1,N),lperm(P),fail.
g2(_,_).

g3(N,Ps):-nats(1,N),findall(P,perm(P),Ps).

test(Mes,LocalMes,F):-
        max(N),X=..[F,N,_],
	statistics(global_stack,[H1,_]),
	statistics(runtime,_),
	X,
	statistics(runtime,[_,T]),
	statistics(global_stack,[H2,_]),H is H2-H1,
	write([Mes,LocalMes]=[time=T,heap=H]),nl.

n(1,n1).
n(2,n2).
n(3,n3).
n(4,n4).
n(5,n5).
n(6,n6).
n(7,n7).
%n(X,n8).

n1.
n2.
n3.
n4.
n5.
n6.
n7.
%n8(8).

no:-fail.

lin_n(X):-n(X,G),G,override(G,no).


lperm([P|Ps]):-lin_n(P),lperm(Ps).
lperm([]):- \+ lin_n(_).
