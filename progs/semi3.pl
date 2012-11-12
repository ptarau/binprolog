go:-go('BMARK_semigroup:').

go(Mes):-go(T,N),nl,write(Mes=[time(T),n(N)]),nl.


/*-----------------------------------------------------------------------------
Program:  Semigroup (hash 2--3 tree version)
Author:   R. Overbeek
Modified: E. Tick
Date:     January 12 1989

Notes:
1. To run:
    ?- go(T,N).
where T is time and N should be output 313.

2. This version includes the generators in the answer (KL1 version doesn't)

3. This version has tuple length hardwired: BE CAREFUL!

4. Here we use a psuedo-hash 2--3 tree: each tuple has a hash key as its first
element, thus we do not need to implement buckets: the 2--3 tree code takes
care of synonyms by continuing to check the rest of the tuple!  Note that
the hash function is very good: only five synonyms generated out of 313 keys.
-----------------------------------------------------------------------------
:- sequential.
:- parallel member/2, umember/2.
*/

go(T,N) :-
    init_sos(Kernel,Sub),
    time(_),
    loop(Kernel, Sub, Kernel, Hbg, Kernel),
    time(T),
    count(Hbg, N).

init_sos(Sos,Sub) :-
    sos(Sos),
    extend_tree(Sos,nil,Sub).

% Sos = list of tuples that need to be processed
% Sub = tree corresponding to the (partial) semigroup Hbg
% Hbg = partial semigroup tuples (initially [])
% 
loop([], _, Hbg, Hbg, _) :- !.
loop(Sos, Sub, Hbg, F, Kernel) :-
    findall(Tuple, newtup(Sos, Kernel, Sub, Tuple), L),
    filter(L, Sub, NewSub, [], NewSos, Hbg, NewHbg),
    loop(NewSos, NewSub, NewHbg, F, Kernel).

% all the parallelism comes from here...
% the order of the umember goals does not seem to matter much
% the first umember can be changed to member with no slow-down
newtup(L,K,Sub,New) :-
    umember(E1,L),     % new candidates
    umember(E2,K),     % kernel (four elements here)
    bigm(E2,E1,New),
    \+ acc23(Sub,New).

filter(   [], Sub,  Sub, Sos,  Sos, Hbg,  Hbg).
filter([H|T], Sub, SubF, Sos, SosF, Hbg, HbgF) :-
    (add23(Sub,H,Sub1) ->
        Sos1=[H|Sos], 
        Hbg1=[H|Hbg]
    ;
        Sub1=Sub, 
        Sos1=Sos,
        Hbg1=Hbg
    ),
    filter(T, Sub1, SubF, Sos1, SosF, Hbg1, HbgF).

bigm(W1,W2,P) :- 
    functor(P,tuple,41),           % one extra element for hash key...
    mtab(Table),
    bigm(2,W1,W2,P,Table,0).

bigm(42,_,_,P,_,Key) :- !,         % insert hash key as first element!
    arg(1,P,Key).
bigm(I,W0,W1,P,Table,Key) :- I<42,
    arg(I,W0,X),
    arg(I,W1,Y),
    arg(X,Table,Row), 
    arg(Y,Row,Z),
    arg(I,P,Z),
    J is I+1,
    NewKey is Z+Key*3,             % hash function overflows, but gets only
    bigm(J,W0,W1,P,Table,NewKey).  % five synonyms in 313 tuples!

% utilities...
lmember(H,[H|_]). 
lmember(H,[_|T]) :- lmember(H,T).

% unrolling pass this point doesn't improve things...

umember(H,[H|_]).
umember(H,[_,H|T]).
umember(H,[_,_,H|T]).
umember(H,[_,_,_,H|T]).
umember(H,[_,_,_,_|T]) :- umember(H,T).

extend_tree([],S,S).
extend_tree([E|T],S,S1) :-
    add23(S,E,S2),
    extend_tree(T,S2,S1).

count(L,N) :- count(L,0,N).
count([X|Xs],M,N) :- M1 is M+1, count(Xs,M1,N).
count([],N,N).

time(T) :- statistics(runtime,[_,T]).

% 2-3 Trees: code from I. Bratko, "Prolog Programming for AI"
acc23(l(X),           X) :-          !.
acc23(n2(T1,M,_),     X) :-  M @> X, !, acc23(T1,X).
acc23(n2(_,_,T2),     X) :-          !, acc23(T2,X).
acc23(n3(T1,M2,_,_,_),X) :- M2 @> X, !, acc23(T1,X).
acc23(n3(_,_,T2,M3,_),X) :- M3 @> X, !, acc23(T2,X).
acc23(n3(_,_,_,_,T3), X) :-             acc23(T3,X).

add23(Tree,X,Tree1) :- 
    ins(Tree,X,Tree1).
add23(Tree,X,n2(T1,M2,T2)) :- 
    ins(Tree,X,T1,M2,T2).

ins(nil,X,l(X)) :- !.
ins(n2(T1,M,T2),X,n2(NT1,M,T2)) :- M @> X,
    ins(T1,X,NT1).
ins(n2(T1,M,T2),X,n3(NT1a,Mb,NT1b,M,T2)) :- M @> X, !,
    ins(T1,X,NT1a,Mb,NT1b).
ins(n2(T1,M,T2),X,n2(T1,M,NT2)) :- X @> M, 
    ins(T2,X,NT2).
ins(n2(T1,M,T2),X,n3(T1,M,NT2a,Mb,NT2b)) :- X @> M, !,
    ins(T2,X,NT2a,Mb,NT2b).
ins(n3(T1,M2,T2,M3,T3),X,n3(NT1,M2,T2,M3,T3)) :- M2 @> X, !,
    ins(T1,X,NT1).
ins(n3(T1,M2,T2,M3,T3),X,n3(T1,M2,NT2,M3,T3)) :- X @> M2, M3 @> X, !,
    ins(T2,X,NT2).
ins(n3(T1,M2,T2,M3,T3),X,n3(T1,M2,T2,M3,NT3)) :- X @> M3,
    ins(T3,X,NT3).

ins(l(A),X,l(A),X,l(X)) :- X @> A, !.
ins(l(A),X,l(X),A,l(A)) :- A @> X, !.
ins(n3(T1,M2,T2,M3,T3),X,n2(NT1a,Mb,NT1b),M2,n2(T2,M3,T3)) :- M2 @> X, !,
    ins(T1,X,NT1a,Mb,NT1b).
ins(n3(T1,M2,T2,M3,T3),X,n2(T1,M2,NT2a),Mb,n2(NT2b,M3,T3)) :- 
    X @> M2, M3 @> X, !,
    ins(T2,X,NT2a,Mb,NT2b).
ins(n3(T1,M2,T2,M3,T3),X,n2(T1,M2,T2),M3,n2(NT3a,Mb,NT3b)) :- X @> M3,
    ins(T3,X,NT3a,Mb,NT3b).

/*
% show tree...
show(T) :- show(T,0).

show(nil, _).
show(l(A),H) :- tab(H), write(A), nl.
show(n2(T1,M,T2),H) :-
    H1 is H+5,
    show(T2,H1),
    tab(H), write('--'), nl,
    tab(H), write(M), nl,
    tab(H), write('--'), nl,
    show(T1,H1).
show(n3(T1,M2,T2,M3,T3),H) :-
    H1 is H+5,
    show(T3,H1),
    tab(H), write('--'), nl,
    tab(H), write(M3), nl,
    show(T2,H1),
    tab(H), write(M2), nl,
    tab(H), write('--'), nl,
    show(T1,H1).
*/

/* 
% original problem (Melbourne paper by Overbeek & Lusk)
sos([tuple(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5),
     tuple(1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5),
     tuple(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,5,5,5,5,5,4,4,4,4,4),
     tuple(1,2,3,5,4,1,2,3,5,4,1,2,3,5,4,1,2,3,5,4,1,2,3,5,4)]).
*/

% 309+4 solutions: 
% to boost execution time by 20% switch elements 28 & 29 in last row!
sos([tuple(1000, 1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4, 
           5,5,5,5,5,3,3,3,3,3,5,5,5,5,5,4,4,4,4,4),
     tuple(2000, 1,2,3,4,5,1,2,3,4,5,1,2,3,4,5,1,2,3,4,5, 
           1,2,3,4,5,1,2,3,4,5,1,3,2,4,5,1,2,3,4,5),
     tuple(3000, 1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,5,5,5,5,5,
           4,4,4,4,4,2,2,2,2,2,4,4,4,4,4,3,3,3,3,3),
     tuple(4000, 1,2,3,5,4,1,2,3,5,4,1,2,3,5,4,1,2,3,5,4, 
           1,2,3,5,4,1,2,3,4,5,1,2,3,5,4,1,2,3,5,4)]).

mtab(table(row(1,1,1,1,1),
           row(1,2,1,4,1),
           row(1,1,3,1,5),
           row(1,1,4,1,2),
           row(1,5,1,3,1))).

