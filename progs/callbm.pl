xfoldl(F,Z,Xs,R):-xfoldl0(Xs,F,Z,R).
  
xfoldl0([],_,R,R).
xfoldl0([X|Xs],F,R1,R3):-call(F,R1,X,R2),xfoldl0(Xs,F,R2,R3).

xfoldr(F,Z,Xs,R):-xfoldr0(Xs,F,Z,R).
  
xfoldr0([],_,Z,Z).
xfoldr0([X|Xs],F,Z,R2):-xfoldr0(Xs,F,Z,R1),call(F,X,R1,R2).

lsum(Nul,Xs,R):-xfoldl(+,Nul,Xs,R).

rsum(Nul,Xs,R):-xfoldl(+,Nul,Xs,R).

ok:-xfoldr((-),0,[1,2,3],R),write(R),nl.

make_ints([],I,I):-!.
make_ints([I0|L],I0,I):-I0<I,I1 is I0+1,make_ints(L,I1,I).

time(T):-statistics(runtime,[T,_]).

% maplist with findall
maplist(Closure,Is,Os):-
  term_append(Closure,args(I,O),Goal),
%  findall_workhorse(O,maplist1(Goal,I,Is),'.',100000,[[]|Os]).
  findall(O,maplist1(Goal,I,Is),Os).

maplist1(Goal,I,Is):-member(I,Is),Goal.

% map with call/N
lmap(F,Xs,Ys):-map0(Xs,F,Ys).

map0([],_,[]).
map0([X|Xs],F,[Y|Ys]):-
   call(F,X,Y),
   map0(Xs,F,Ys).

test(F,Op,T):-
  iter(N),
  make_ints(Is,0,N),
  time(T1),
  call(F,Op,Is,_Os),
%  write(_Os),nl,
  time(T2),
  T is T2 - T1.

dtest(G,T):-
  iter(N),
  make_ints(Is,0,N),
  time(T1),
  call(G,Is,_Os),
%  write(G+_Os),nl,
  time(T2),
  T is T2 - T1.

iter(20000).

add(N,Xs,Ys):-add1(Xs,N,Ys).

add1([],_,[]).
add1([X|Xs],N,[Y|Ys]):-
   Y is X+N,
   add1(Xs,N,Ys).

go:-
   member(F-Op,[maplist - +(10),lmap - +(10),lsum-0,rsum-0]),
   test(F,Op,T),
   write('BMARK_call_builtin'(F)=T),nl,
   fail.
go:-
   F=add(10),
   dtest(F,T),
   write('BMARK_call_peval_time'(F)=T),nl,
   fail.

