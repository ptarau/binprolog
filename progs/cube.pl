/*----------------------------------------------------------------------------
Program:  Instant Insanity (fused gen & test)
Author:   E. Tick
Date:     August 26 1989

Notes:
1. To run:
    ?- go(7,N,T,S).
where output S is the list of solutions, N=48 the number of solutions
(for 7-cubes), and T is the execution time.
--------------------------------------------------------------------
:- sequential.
:- parallel set/2, rotate/3.
*/
p:-[cube].

go:-go('BMARK_cube:').

go(Mes):-go(7,N,T,_S),write(Mes=[sols=N,time=T]),nl.

go(C,N,T,S) :- time(_),
	findall(X,sol(C,X),S), count(S,N), 
	time(T).

sol(C,X) :- cubes(C,Q), sol(Q,[],X).

sol([],A,A).
sol([Q|Qs],A,F) :-
    set_cube(Q,P),
    check(A,P),
    sol(Qs,[P|A],F).

check([],_).
check([q(A1,B1,C1,D1)|As],P) :-
    P = q(A2,B2,C2,D2),
    A1 =\= A2, B1 =\= B2, C1 =\= C2, D1 =\= D2, 
    check(As,P).
 
set_cube(q(P1,P2,P3),P) :- rotate(P1,P2,P).
set_cube(q(P1,P2,P3),P) :- rotate(P2,P1,P).
set_cube(q(P1,P2,P3),P) :- rotate(P1,P3,P).
set_cube(q(P1,P2,P3),P) :- rotate(P3,P1,P).
set_cube(q(P1,P2,P3),P) :- rotate(P2,P3,P).
set_cube(q(P1,P2,P3),P) :- rotate(P3,P2,P).

rotate(p(C1,C2),p(C3,C4),q(C1,C2,C3,C4)).
rotate(p(C1,C2),p(C3,C4),q(C1,C2,C4,C3)).
rotate(p(C1,C2),p(C3,C4),q(C2,C1,C3,C4)).
rotate(p(C1,C2),p(C3,C4),q(C2,C1,C4,C3)).

cubes(4,[q(p(0,1),p(2,0),p(1,3)),
         q(p(3,3),p(2,0),p(1,2)),
         q(p(0,3),p(3,1),p(1,2)),
         q(p(0,0),p(3,0),p(1,2))]).
cubes(5,[q(p(2,1),p(1,4),p(3,1)),
         q(p(3,2),p(2,0),p(3,4)),
         q(p(1,4),p(3,1),p(0,4)),
         q(p(1,0),p(2,2),p(0,4)),
         q(p(4,2),p(4,3),p(0,3))]).
cubes(6,[q(p(0,5),p(1,5),p(3,1)),
         q(p(2,1),p(3,4),p(4,0)),
         q(p(3,0),p(4,5),p(2,4)),
         q(p(1,3),p(5,1),p(0,1)),
         q(p(0,2),p(0,2),p(5,2)),
         q(p(4,4),p(2,3),p(4,5))]).
cubes(7,[q(p(5,1),p(0,5),p(3,1)),
         q(p(2,3),p(1,4),p(4,0)),
         q(p(3,6),p(0,0),p(2,4)),
         q(p(6,4),p(6,1),p(0,1)),
         q(p(1,5),p(3,2),p(5,2)),
         q(p(5,0),p(2,3),p(4,5)),
         q(p(4,2),p(2,6),p(0,3))]).

time(T) :- statistics(runtime,[_,T]).

count(L,N) :- count(L,0,N).
count([],N,N).
count([X|Xs],M,N) :- M1 is M+1, count(Xs,M1,N).



