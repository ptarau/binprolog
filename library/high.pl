
go:-foldl1(+,0,[1,2,3],R),write(R),nl.

% fold,foldl based on safe failure driven destructive change_arg
foldl1(Closure,Null,List,Final):-fold(Closure,Null,X^member(X,List),Final).

fold(Closure,Null,I^Generator,Final):-
  fold0(s(Null),I,Generator,Closure,Final).

fold0(Acc,I,Generator,Closure,_):-
  term_append(Closure,args(SoFar,I,O),Selector),
  Generator,
    arg(1,Acc,SoFar),
    Selector,
    change_arg(1,Acc,O),
  fail.
fold0(Acc,_,_,_,Final):-
  arg(1,Acc,Final).

% new version of bestof
best_of(X^Generator,TotalOrder,Bottom,Result):-
	term_append(TotalOrder,args(X,Y),Test),
	fold(compare_closure(Y,Test),Bottom,X^Generator,Result).

% map with updates on place
map2(Closure,Xs):-
  term_append(Closure,args(I,O),Goal),
  update_on_place(Xs,I,O,Goal),
  fail.
map2(_,_).

update_on_place(Xs,I,O,Goal):-
   Xs=[I|_],
   Goal,
   change_arg(1,Xs,O).
update_on_place([_|Xs],I,O,Goal):-
   update_on_place(Xs,I,O,Goal).

    
% gives the illusion of a parallel thread
% works only with goals generating a finite stream of solutions

test_thread:-
  open_thread(X,(X=1;X=2;X=3),E),
  query_thread(E,A),
  query_thread(E,B),
  close_thread(E),
  query_thread(E,C),
  query_thread(E,D),
  write([A,B,C,D]),nl.
  
open_thread(X,G,'$answers'(Gs)):-findall(X,G,Gs).

query_thread('$answers'([]),X):-!,X='$empty'.
query_thread(E,'$answer'(X)):-E='$answers'([X|Xs]),setarg(1,E,Xs).

close_thread(Engine):-setarg(1,Engine,[]).


/************************* maplist/3 ****************************/
% maps a Closure to a list and collects the results
%
% ex: ?-maplist(+(1),[10,20,30],Xs).
%

maplist(Closure,Is,Os):-maplist(Closure,Is,Os,[]).

maplist(Closure,Is,Os,End):-
  term_append(Closure,args(I,O),Test),
  findall(O,member_test(Test,I,Is),Os,End).

member_test(Test,I,[I|_]):-Test.
member_test(Test,I,[_|Is]):-member_test(Test,I,Is).

/************************* find/4 ****************************/
% combines 2 by 2 using Closure the selected answers I of Generator
% accumulating in Final the overall result
%
% ex: ?-find(member(X,[10,20,30]),+,X,Sum).
%

find(Generator,Closure,I,Final):-
  term_append(Closure,args(SoFar,I,O),Selector),
  find0(SoFar,I,O,Generator,Selector,Final).

find0(SoFar,I,O,Generator,Selector,_):-
  inc_level(find,Level),
  Generator,
  select_or_init(Selector,Level,SoFar,I,O),
  fail.
find0(_,_,_,_,_,Final):-
  dec_level(find,Level),
  bb_val(find,Level,Final),
  rm(find,Level).

select_or_init(Selector,Level,SoFar,_,O):-
  bb_val(find,Level,SoFar),!,
  Selector,
  bb_set(find,Level,O).
select_or_init(_,Level,_,I,_):-
  bb_def(find,Level,I).

% ensure correct implementation of embedded calls to find/4

inc_level(Obj,X1):-bb_val(Obj,Obj,X),!,X1 is X+1,bb_set(Obj,Obj,X1).
inc_level(Obj,1):-bb_def(Obj,Obj,1).

dec_level(Obj,X):-bb_val(Obj,Obj,X),X>0,X1 is X-1,bb_set(Obj,Obj,X1).

/************************* scan/3 ****************************/
% Scans a list accumulating the results of applyng Closure on
% the elements of the list
% 
% ex: ?-scan(+,[10,20,30],Sum).
%

scan(Closure,List,Result):-find(member(X,List),Closure,X,Result).

% X is the best answer of G with respect to TotalOrder (a closure)
% 
% ex: ?-bestof(X,>,member(X,[3,2,9,1,5,4]).
%

bestof(X,TotalOrder,Generator):-
	term_append(TotalOrder,args(X,Y),Test),
	find(Generator,compare_closure(Y,Test),X,X).

compare_closure(Y,Test,Y,X,R):-Test,!,R=X.
compare_closure(_,_,Y,_,Y).

:-op(602,xfx,(..)).
:-op(602,xf,(...)).
:-op(700,xfx,(<-)).
:-op(750,xfy,(#=)).
:-op(750,xfy,(#<)).
:-op(750,xfy,(#>)).

% TOWARDS AN ALGEBRA OF (FINITE AND INFINITE) STREAMS

% basic infinite streams

% natural numbers starting with N: i,o
nat(N,N).
nat(N,R):-N1 is N+1,nat(N1,R).

% arithmetic progression: i,i,o
arith_prog(X0,Ratio,X):-nat(0,I),X is Ratio*I+X0.

% constant stream = N: i,o
const(N,N).
const(N,R):-const(N,R).

% finite infix selectors
interval(Min,Max,X):-N is Max-Min,take_at_most(N,nat(Min,X)).

finite_arith_prog(Min,Ratio,Max,X):-while(X=<Max,arith_prog(Min,Ratio,X)).

% infinite stream transformers
drop_first(N,G):-drop_at_least(N,G).

skip_interval(N1,N2,G,X):-skip_when((X>N1,X<N2),G).

G #= N :- nth_answer(N,G).

% syntactic interface
X <- Xs :- member(Y,Xs),reduce_int(Y,X).

reduce_int(Min/Next..Max,X):-!,Ratio is Next-Min,
  finite_arith_prog(Min,Ratio,Max,X).
reduce_int(Min..Max,X):-!,interval(Min,Max,X).
reduce_int(Min/Next... ,X):-!,Ratio is Next-Min,
  arith_prog(Min,Ratio,X).
reduce_int(Min... ,X):-!,nat(Min,X).
reduce_int(X,X).

g1:-find_at_most(5,X,nat(10,X),Xs),write(Xs),nl,fail.
g2:-all_but_at_least(5,X,take_at_most(10,nat(20,X)),Xs),write(Xs),nl,fail.
g3:-take_at_most(20,skip_interval(2,15,X<-[1..3,10 ...],X)),write(X),nl,fail.


/*

NOTE: That's not syntactic sugar to transform functions to predicates
but an adaptation of an `emergent' ability of lazy functional languages
which is not noramally present in LP.

*/

