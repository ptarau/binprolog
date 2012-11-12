fast_findall(X,G,Xs):-findall(X,G,Xs).

all_permutations([],[[]]).
all_permutations([X|Xs],Perms2):-
       	all_permutations(Xs,Perms1),
        extend_permutations(Perms1,X,Perms2).

extend_permutations([],_,[]).
extend_permutations([Perm|Perms1],X,[[X|Perm]|Perms3]):-
	extend_permutations(Perms1,X,Perms2),
	insert_item(Perm,X,[],Perms2,Perms3).

insert_item([],_,_,Perms,Perms).
insert_item([Y|Ys],X,Acc,Perms1,[Zs|Perms2]):-
       	reverse_and_append(Acc,[Y,X|Ys],Zs),
        insert_item(Ys,X,[Y|Acc],Perms1,Perms2).

reverse_and_append([],Acc,Acc).
reverse_and_append([X|Xs],Acc,Zs):-
       reverse_and_append(Xs,[X|Acc],Zs).

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
g2(N,Ps):-nats(1,N,Ns),fast_findall(P,perm(Ns,P),Ps).
g3(N,Ps):-nats(1,N,Ns),findall(P,perm(Ns,P),Ps).
g4(N,Ps):-nats(1,N,Ns),all_permutations_with_engine(Ns,Ps).

test(Mes,LocalMes,X):-
	statistics(global_stack,[H1,_]),
	statistics(runtime,[T1,_]),
	X,
	statistics(runtime,[T2,_]),T is T2-T1,
	statistics(global_stack,[H2,_]),H is H2-H1,
	write([Mes,LocalMes]=[time=T,heap=H]),nl.


test(Mes):-test(Mes,nondet,g0(8)).

test(Mes):-test(Mes,determ,g1(8,_)).

test(Mes):-test(Mes,with_fast_findall,g2(8,_)).

test(Mes):-test(Mes,with_findall,g3(8,_)).

test(Mes):-test(Mes,with_engine,g4(8,_)).

all_permutations_with_engine(Xs,Ps):-
   create_engine(E),
   %ctime(T1),
   load_engine(E,perm(Xs,P),P),
   grab_all(E,Ps),
   %ctime(T2),T is T2-T1,write('time if engines were reused'=T),nl,
   % slower, but still faster than findall based solution 
   % in all prologs around, SICStus 2.1_9 being closer at:
   % [BMARK_allperms,with_findall]=[time=3800,heap=2904124]
   destroy_engine(E).

grab_all(E,[P|Ps]):-
   ask_engine(E,P),!,
   grab_all(E,Ps).
grab_all(_,[]).

go(Mes):-
  write('execute with -h20000 option'),nl,
  test(Mes),
  fail
; true.

go:-go('BMARK_allperms').

go0:-
 Mes='BMARK_allperms, nondet and det',
 test(Mes,nondet,g0(11)),
 test(Mes,determ,g1(7,_)).
