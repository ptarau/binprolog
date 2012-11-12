% ap4.pl

all_permutations([],[[]]).
all_permutations([X|Xs],Ps2):-
       	all_permutations(Xs,Ps1),
        extend_permutations(Ps1,X,Ps2).

extend_permutations([],_,[]).
extend_permutations([P|Ps1],X,[[X|P]|Ps3]):-
	extend_permutations(Ps1,X,Ps2),
	insert_item(P,[],X,Ps2,Ps3).

insert_item([],_,_,Ps,Ps).
insert_item([Y|Ys],Ys0,X,Ps1,[Zs|Ps2]):-
   reverse_and_append(Ys0,Ys,X,Ps1,Ps2,[Y|Ys0],Zs,[Y,X|Ys]).

reverse_and_append([],Ys,X,Ps1,Ps2,Ys1,Zs,Zs):-
   insert_item(Ys, Ys1, X,Ps1,Ps2).
reverse_and_append([X1|Xs],Ys,X,Ps1,Ps2, Ys1,Zs,Zs1):-
   reverse_and_append(Xs,Ys,X,Ps1,Ps2,Ys1, Zs,[X1|Zs1]).

nats(Max,Max,[Max]):-!.
nats(Curr,Max,[Curr|Ns]):-
	Curr<Max,
	Curr1 is Curr+1,
	nats(Curr1,Max,Ns).

perm([],[]).
perm([X|Xs],Zs):-
	perm(Xs,Ys),
	insert(X,Ys,Zs).

insert(X,Ys,[X|Ys]).
insert(X,[Y|Ys],[Y|Zs]):-
	insert(X,Ys,Zs).

g0(N):-nats(1,N,Ns),perm(Ns,_),fail.
g0(_).

g1(N,Ps):-nats(1,N,Ns),all_permutations(Ns,Ps).
g2(N,Ps):-nats(1,N,Ns),findall(P,perm(Ns,P),Ps).

test(Mes,LocalMes,X):-
	statistics(global_stack,[H1,_]),
	statistics(runtime,_),
	X,
	statistics(runtime,[_,T]),
	statistics(global_stack,[H2,_]),H is H2-H1,
	write([Mes,LocalMes]=[time=T,heap=H]),nl.

t0(Mes):-test(Mes,nondet,g0(8)).

t1(Mes):-test(Mes,determ,g1(8,_)).

t2(Mes):-test(Mes,with_findall,g2(8,_)).

go(Mes):-
  write('execute with -h20000 option'),nl,
  t0(Mes),fail;t1(Mes),fail;t2(Mes),fail.

go:-go('BMARK_allperms').

p:-[allperms].
