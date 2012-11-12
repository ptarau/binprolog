:-[library(tree)].


vmax(4). % max vertical levels
hmax(5). % max siblings
dmax(3). % max distinct vars

max(100). % nb of clauses

alea(Max,N):-Max>0,random(X),N is X mod Max.

time(T):-statistics(runtime,[T,_]).

rantree(T):-vmax(V),hmax(H),dmax(N),N1 is N+1,
  N2 is N1+1,functor(D,d,N2),
  arg(1,D,0),
  arg(2,D,a),
  rantree(V,H,N1-D,T).

rantree(0,_,_,b).
rantree(V,H,D,T):-
  V>0,V1 is V-1,
  alea(H,N),
  length(Xs,N),
  T=..[t|Xs],
  ranforest(Xs,V1,H,D,T).

ranforest([],_,_,_,_).
ranforest([X|Xs],V,H,D,T):-
  (alea(2,0)->ranbind(D,X);rantree(V,H,D,X)),
  ranforest(Xs,V,H,D,T).

ranbind(N-D,X):-
  alea(N,I),J is I+1,arg(J,D,X).


show:-
 random_seed(17),
 nl,
 range(_,1,20),
   rantree(T),ppt(T),nl,
 fail.

fname('ranfile.pro').

ranfile:-max(N),ranfile(N).

ranfile(Max):-
  random_seed(17),
  fname(F),
  tell(F),
  pp_clause((:-dynamic n/2,i/2,ok/1)),nl,
  pp_clause(ok(yes)),
  range(I,1,Max),
    rantree(H),
    (alea(2,0)->rantree(B);B=yes),
    pp_clause((i(I,H):-ok(B))),
    pp_clause((n(H,I):-ok(B))),
  fail
; nl,told.
    

test(Verb,Fcom):-
/*
  abolish(i,2),
  abolish(n,2),
  abolish(ok,1),
  abolish(i1,2),
  abolish(n1,2),
*/
  functor(Consult,Fcom,1),
  fname(F),
  arg(1,Consult,F),
  Consult,
  time(T1),
  bm(Verb,n(J,_),J),
  time(T2),
  bm(Verb,i(J,_),J),
  time(T3),
  D1 is T2-T1,
  D2 is T3-T2,
  write(Verb=[indexed=D1,unindexed=D2]),
  nl.

bm(Verb,G,I):-
  functor(Test,Verb,1),arg(1,Test,G),
  max(N),
  range(J,1,N),
    I is 1+N-J,
    Test,
  fail.
bm(_,_,_).


range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).

  
clause1(X):-clause(X,_).

uclause1(X):-uclause(X,_).

nop(_).

tgo:-test(clause1,consult),test(call,nop),test(retract,nop).

ugo:-test(uclause1,uconsult),test(call,nop),test(uretract,nop).

go:-iobm(1000).

iobm(Max):-
  fname(F),
  write(clauses(Max,to,F)),nl,
  obm(Max,T),
  write(time(written,T)),nl,
  co(F).

obm(Max,T):-
 ctime(T1),
 obm0(Max),
 ctime(T2),
 T is T2-T1.

obm0(Max):-
  random_seed(17),
  fname(F),
  tell(F),
  qprint(ok(yes)),
  range(I,1,Max),
    rantree(H),
    (alea(2,0)->rantree(B);B=yes),
    qprint((i(I,H):-ok(B))),
    qprint((n(H,I):-ok(B))),
  fail
; nl,told.

%qprint(C):-cwrite(C),cwrite('.'),nl.

cread(C):-read_clause(C).

ibm(T):-
  ctime(T1),
  fname(F),
  see(F),
  repeat,
    read_clause(C),
    ( C=end_of_file->!
    ; assertz(C),fail
    ),
  seen,
  ctime(T2),
  T is T2-T1.

