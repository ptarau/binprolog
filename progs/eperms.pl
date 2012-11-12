go:-
   all_permutations_with_engine([1,2,3],Ps),
   write(Ps),nl.

% nondeterninistic permutation generator
perm([],[]).
perm([X|Xs],Zs):-
	perm(Xs,Ys),
	insert(X,Ys,Zs).

insert(X,Ys,[X|Ys]).
insert(X,[Y|Ys],[Y|Zs]):-
	insert(X,Ys,Zs).

% list of all permutations generator
all_permutations_with_engine(Xs,Ps):-
   create_engine(E), % create new engine
   load_engine(E,perm(Xs,P),P), % load engine with new goal
   grab_all(E,Ps), % process answers
   destroy_engine(E). % release engine

% permutation processor
grab_all(E,[P|Ps]):-
   ask_engine(E,P),!,
   grab_all(E,Ps).
grab_all(_,[]).
