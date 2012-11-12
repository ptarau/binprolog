:-dynamic digit/1,init/3.

% Program: linear implication based FD constraint solving
% Author: Paul Tarau, 1995

% simple cryptarithmetic puzzle solver
% a kind of "constraint solving with linear implication"

example1(
   [s,e,n,d,m,o,r,e,y]=[S,E,N,D,M,O,R,E,Y],
   [S,E,N,D]+
   [M,O,R,E]=
 [M,O,N,E,Y],
   [s,e,n,d]+
   [m,o,r,e]=
[ m,o,n,e,y]
).

% example from wamcc(clp)  - adapted by 
% Daniel Diaz - INRIA France, Original Source: P. Van Hentenryck's book                                       
example2(
   [a,b,c,d,e,f,g,h,i,j]=[A,B,C,D,E,F,G,H,I,J],
   [B, A, I, J, J, A, J, I, I, A, H, F, C, F, E, B, B, J, E, A]+
   [D, H, F, G, A, B, C, D, I, D, B, I, F, F, A, G, F, E, J, E]=
   [G, J, E, G, A, C, D, D, H, F, A, F, J, B, F, I, H, E, E, F],

   [b, a, i, j, j, a, j, i, i, a, h, f, c, f, e, b, b, j, e, a]+
   [d, h, f, g, a, b, c, d, i, d, b, i, f, f, a, g, f, e, j, e]=
   [g, j, e, g, a, c, d, d, h, f, a, f, j, b, f, i, h, e, e, f]
).

% Addition of two numbers
sum(As, Bs, Cs) :- sum(As, Bs, 0, Cs).

sum([A|As], [B|Bs], Carry, [C|Cs]) :- !,
	add2digits(A,B,Carry,C,NewCarry),
        sum(As, Bs, NewCarry, Cs).
sum([], Bs, 0, Bs) :- !.
sum(As, [], 0, As) :- !.
sum([], [B|Bs], Carry, [C|Cs]) :- !,
	add1digit(B,Carry,C,NewCarry),
        sum([], Bs, NewCarry, Cs).
sum([A|As], [], Carry, [C|Cs]) :- !,
	add1digit(A,Carry,C,NewCarry),
        sum([], As, NewCarry, Cs).
sum([], [], Carry, [Carry]).

add2digits(A,B,Carry,Result,NewCarry):-
  bind(A),bind(B),
  add_with_carry(10,A,B,Carry,Result,NewCarry).

add1digit(D,Carry,Result,NewCarry):-
  bind(D),
  add_with_carry(10,D,0,Carry,Result,NewCarry).

add_with_carry(Base,A,B,Carry,Result,NewCarry):-
  S is A+B+Carry,
  Result is S mod Base,
  NewCarry is S // Base,
  new_digit(Result).

reverse(Xs,Zs):-rev(Xs,[],Zs).

rev([],Ys,Ys).
rev([X|Xs],Ys,Zs):-rev(Xs,[X|Ys],Zs).

bind(A):-var(A),!,digit(A).
bind(_).

new_digit(A):-digit(A),!.
new_digit(_).

solve(As,Bs,Cs,Z):-
  digit(0)-:digit(1)-:digit(2)-:digit(3)-:digit(4)-:digit(5)-:
  digit(6)-:digit(7)-:digit(8)-:digit(9)-:
  ( sum(As,Bs,Cs),
    Z>0
  ).

show_answer(Vars,Ns+Vs=Rs,As+Bs=Cs):-
  write(Vars),nl,nl,
  write(As),write(+),nl,
  write(Bs),write(=),nl,
  write(Cs),nl,nl,
  write(Ns),write(+),nl,
  write(Vs),write(=),nl,
  write(Rs),nl,nl.

puzzle:-
    init(Vars,Puzzle,Names),
    Puzzle=(Xs+Ys=Zs),Zs=[Z|_],
    reverse(Xs,As),
    reverse(Ys,Bs),
    reverse(Zs,Cs),
      solve(As,Bs,Cs,Z),
      show_answer(Vars,Names,Puzzle),
    fail.
puzzle:-
    write('no (more) answers'),nl,nl.

time(Goal):-
  ctime(T1),
  (Goal->true;true),
  ctime(T2),
  T is T2-T1,
  write(time=T),nl,nl.
  
go:-
  (init(X,Y,Z):-example1(X,Y,Z))-:
  time(puzzle),
  fail.
go:-
  (init(X,Y,Z):-example2(X,Y,Z))-:
  time(puzzle).
