go:-calibrate.

calibrate:-
  calibrate(T,Times),
  write(T=Times),nl.

calibrate(T):-calibrate(T,_).

calibrate(T,Details):-calibrate(30,T,Details).
  
calibrate(Multiplier,T,times(lips(LIPS),nrev(TN),perm(TP),assert(TA))):-
   Base is 5,
   NTimes is Base*Multiplier,
   PTimes is Base*Multiplier,
   ATimes is Base*Multiplier,
   Len is 200,
   getctime(TB),
   fortest(NTimes),
   getctime(T0),
   nrevtest(Len,NTimes),
   getctime(T1),
   permtest(7,PTimes),
   getctime(T2),
   asserttest(5,ATimes),
   getctime(T3),
   TF is T0-TB,
   TN is T1-T0,
   TP is T2-T1,
   TA is T3-T2,
   T is TN+TP+TA,
   LI is (NTimes/1.0001)*(((Len+1)*(Len+2))//2),
   TS is (TN-TF)/1000.0001,
   LIPS is integer(LI/TS).

fortest(N):-'$for'(_I,1,N),fail.
fortest(_).
   
nrevtest(N,Times):-
  findall(I,'$for'(I,1,N),Is),
  '$for'(I,1,Times),
  '$nrev'(Is,_),
  fail.
nrevtest(_N,_Times).

'$app'([],Ys,Ys).
'$app'([A|Xs],Ys,[A|Zs]):-
  '$app'(Xs,Ys,Zs).

'$nrev'([],[]).
'$nrev'([X|Xs],Zs):-
  '$nrev'(Xs,Ys),
  '$app'(Ys,[X],Zs).

permtest(N,Times):-
  '$for'(_I,1,Times),
  permtest(N),
  fail.
permtest(_N,_Times).
  
permtest(N):-
  findall(I,'$for'(I,1,N),Is),
  '$perm'(Is,_),
  fail.
permtest(_).

'$perm'([],[]).
'$perm'([X|Xs],Zs):-
	'$perm'(Xs,Ys),
	'$insert'(X,Ys,Zs).

'$insert'(X,Ys,[X|Ys]).
'$insert'(X,[Y|Ys],[Y|Zs]):-
	'$insert'(X,Ys,Zs).

asserttest(MX,MY):-
  initboard1(MX,MY),
  updateboard1(MX,MY),
  accessboard1(MX,MY),
  cleanboard1.

initboard1(MaxX,MaxY):-
	'$for'(X,1,MaxX),
	'$for'(Y,1,MaxY),
	assert('$board'(X,Y,0)),
	fail.
initboard1(_,_).

updateboard1(MaxX,MaxY):-
	'$for'(X,1,MaxX),
	'$for'(Y,1,MaxY),
        update1(X,Y),
	fail.
updateboard1(_,_).
	
update1(X,Y):-
  retract('$board'(X,Y,_)),
  !,
  assert('$board'(X,Y,1)).

accessboard1(MaxX,MaxY):-
	'$for'(X,1,MaxX),
	'$for'(Y,1,MaxY),
 	'$board'(X,Y,_),
	fail.
accessboard1(_,_).

cleanboard1:-
  abolish('$board'/3).

getctime(T):-statistics(runtime,[T,_]).

'$for'(Min,Min,Max):-Min=<Max.
'$for'(I,Min,Max):-
  Min<Max,
  Min1 is Min+1,
  '$for'(I,Min1,Max).
