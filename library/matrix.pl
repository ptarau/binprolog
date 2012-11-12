% Program: basic matrix manipulation package for BinProlog 3.30
% FILE: library/matrix.pl Available from clement.info.umoncton.ca.
% 
% Author: Paul Tarau, 1995
%
% - argues for the vertues of an OR-intensive programming style
% - argues for the vertues of Haskell-style high order operators
%
% THE POINT IS THAT WE CAN AVOID EXPLICIT ITERATION mainly because
% an OR-intensive style is `compositional' in the sense that
% it allows reuse of existing finite domain generators (i.e. for/3)
% The alternative: endless functor+arg+ I1 is I+1 hacking.
%
% Things can be made much faster if this package is found useful.
% Absolute speed was not an issue. The issue was to have it all working
% in about 30 minutes :-). Comments and improvements are welcome.
% The funny thing is that the full package has only 2 cuts...

% makes a new vector of MaxI elements V, such that V[I]=VI,
% where VI is produced by generator Gen for each I
newv(Name,MaxI,Gen,I,VI,V):-
  findall(VI,(for(I,1,MaxI),Gen),VIs),
  V=..[Name|VIs].

% makes a 2-dim matrix M of MaxI X MaxJ elements, such that M[I,J]=MIJ, 
% where MIJ is produced by Gen for each I,J
newm(MaxI,MaxJ,Gen,I,J,MIJ,M):-
  newv(m,MaxI,
       newv(v,MaxJ,(for(J,1,MaxJ),Gen),J,MIJ,V),
       I,V,M).

% functor/arg based representation is avoidable in BinProlog
% by using hashing based blackboard primitives
% true iff M[I,J]=X
aref(M,I,J,X):-arg(I,M,V),arg(J,V,X).

% true iff M has I rows and J columns
dim(M,I,J):-
  functor(M,_,I),
  arg(1,M,V),
  functor(V,_,J).

% M=M1+M2
sum(M1,M2,M):-sum_like(+,M1,M2,M).

% M=M1-M2
dif(M1,M2,M):-sum_like(-,M1,M2,M).

sum_like(Op,M1,M2,M):-
  dim(M1,MaxI,MaxJ),
  dim(M2,MaxI,MaxJ),
  newm(MaxI,MaxJ,sumIJ(Op,M1,M2,I,J,X),I,J,X,M).

sumIJ(Op,M1,M2,I,J,X):-
  aref(M1,I,J,X1),
  aref(M2,I,J,X2),
  call(Op,X1,X2,X).

transposed(M,T):-
  dim(M,MaxI,MaxJ),
  newm(MaxJ,MaxI,aref(M,I,J,X),J,I,X,T).

% M = M1*M2
prod(M1,M2,M):-prod_like(+,*,M1,M2,M).

prod_like(SumOp,MultOp,M1,M2,M):-
  dim(M1,MaxI,MaxK),
  dim(M2,MaxK,MaxJ),
  newm(MaxI,MaxJ,
    fold(SumOp,P^prodIJ(MultOp,M1,M2,MaxK,I,J,P),X),
  I,J,X,M).

prodIJ(Op,M1,M2,MaxK,I,J,X):-
  for(K,1,MaxK),
    aref(M1,I,K,X1),
    aref(M2,K,J,X2),
    call(Op,X1,X2,X).

% M is the unit square matrix of dim N
id(N,M):-newm(N,N,(I=J->X=1;X=0),I,J,X,M).

% M is the 0 square matrix of dim N
zero(N,M):-newm(N,N,X=0,_,_,X,M).

% KM is K times M, where K is a scalar
times(K,M,KM):-
  dim(M,MaxI,MaxJ),
  newm(MaxI,MaxJ,(aref(M,I,J,X),KX is K*X),I,J,KX,KM).

print_matrix(M):-
  argn(_I,M,MI), % non-deterministic arg/3
  nl,argn(_J,MI,X),
  write(X),write(' '),
  fail
; nl.

list2vector(Name,Xs,V) :- V=..[Name|Xs].

list2matrix(Xss,M):-
  findall(V,(member(Xs,Xss),list2vector(v,Xs,V)),Vs),
  list2vector(m,Vs,M).

% boolean matrix operations with 1=true, 0=false

bool_or(M1,M2,R):-sum_like(max,M1,M2,R).
bool_and(M1,M2,R):-sum_like(min,M1,M2,R).

bool_prod(M1,M2,M):-prod_like(max,min,M1,M2,M).

% tools for boolean operations
/*
max(X,Y,Z):-compare(R,X,Y),order(R,X,Y,_,Z).

min(X,Y,Z):-compare(R,X,Y),order(R,X,Y,Z,_).
*/

order(<,X,Y,X,Y).
order(=,X,Y,X,Y).
order(>,X,Y,Y,X).

% generic tools

% combines 2 by 2 answers I of Generator, by applying Closure,
% and accumulating in Final the overall result 
% no 0 element is needed as in Haskell because we initialize
% with the first solution
% if the generator is `empty' i.e if it always fails
% then fold will simply fail - this make its behvior compositional 

fold(Closure,I^Generator,Final):-
  % we construct the Selector once and reuse it over and over
  term_append(Closure,args(SoFar,I,O),Selector),
  fold0(SoFar,I,O,Generator,Selector,Final).

fold0(SoFar,I,O,Generator,Selector,_):-
  inc_level(fold,Level),
  Generator,
  select_or_init(Selector,Level,SoFar,I,O),
  fail.
fold0(_,_,_,_,_,Final):-
  dec_level(fold,Level),
  bb_val(fold,Level,Final),
  rm(fold,Level).

select_or_init(Selector,Level,SoFar,_,O):-
  val(fold,Level,SoFar),!,
  Selector,
  bb_set(fold,Level,O).
select_or_init(_,Level,_,I,_):-
  bb_def(fold,Level,I).

% ensure correct implementation of embedded calls to fold/4

inc_level(Obj,X1):-val(Obj,Obj,X),!,X1 is X+1,set(Obj,Obj,X1).
inc_level(Obj,1):-def(Obj,Obj,1).

dec_level(Obj,X):-val(Obj,Obj,X),X>0,X1 is X-1,set(Obj,Obj,X1).

/*
% test data

rcons(Y,X,[X|Y]).

reverse(Xs,Ys):-fold(rcons,X^(X=[];member(X,Xs)),Ys).

test(1):- 
  newm(3,3,(X is (I+J)//2),I,J,X,M),print_matrix(M),
  sum(M,M,R),
  times(10,R,RR),
  print_matrix(R),
  print_matrix(RR).

test(2):-id(3,Id),newm(3,3,(X is I+J),I,J,X,M),
  prod(M,Id,R),prod(Id,M,RR),
  print_matrix(M),
  print_matrix(R),
  print_matrix(RR).

test(3):-
  Xss=[[1,2,3],
       [0,4,5],
       [0,0,6]],
     list2matrix(Xss,M),
     transposed(M,T),
     prod(M,T,R),
     print_matrix(M),
     print_matrix(T),
     print_matrix(R).

test(4):-
  Xss=[[0,1,0],
       [0,0,1],
       [1,0,0]],
     list2matrix(Xss,M),
     transposed(M,T),
     prod_like(max,min,M,M,R),
     print_matrix(M),
     print_matrix(T),
     print_matrix(R).

test(5):-

  list2matrix(
      [[7,6,4],
       [8,2,9],
       [2,5,3]],
  M1),
  list2matrix(
      [[1,2,3],
       [4,5,6],
       [7,8,9]],
  M2),
  transposed(M1,T1),transposed(M2,T2),
  prod(M1,M2,M12),
  prod(T2,T1,T21),
  transposed(T21,T12),
  print_matrix(M12),
  print_matrix(T12).

  
go:-test(_),fail.

ex(1,
        [[      0.500027,     0,      -0.866010,    0],
         [      0,      1,      0,      0],
         [      0.866010,     0,      0.500027,     0],
         [      0,      0,      0,        1]]).

ex(2,
        [[      1,      0,      0,      0],
         [      0,      0.7071,       0.7071,       0],
         [      0,      -0.7071,      0.7071,       0],
         [      0,      0,      0,       1]]).

exm(I,M):-ex(I,Xss),list2matrix(Xss,M).

go1:-exm(1,M1),exm(2,M2),prod(M1,M2,M12),print_matrix(M12),fail.
*/
