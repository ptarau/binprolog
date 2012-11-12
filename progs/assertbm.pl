:-dynamic board/3.

% dim(200,200).
% dim(100,100).
% dim(50,50).
dim(64,64).

% dim(128,8).
% dim(64,16).
% dim(32,32).

% dim(16,32).
% dim(16 ,16).
% dim(8 ,8).
% dim(2,2).

go:-go3,go3.

go3:-go1,fail;go2.

go1:-go1a,go1b.

go1a:-
	time(_),nop,time(N),
	initboard1,time(I0),
	%hash_compile,U0=0,C0=0,
	updateboard1,time(U0),
	accessboard1,time(A0),
	cleanboard1,time(C0),
	I is I0-N,U is U0-N,A is A0-N,C is C0-N,
	T is I+U+A+C,
	R=[db_op_all,nop=N,init=I,update=U,access=A,clean(C),total=T],
	write(R),nl,
	dim(XM,YM),SM is 1.000001*XM*YM,
	NM is N/SM, IM is I/SM,UM is U/SM, AM is A/SM, TM is T/SM,CM is C/SM,
  RM=[db_op(1/SM),nop=NM,init=IM,update=UM,access=AM,clean=CM,total=TM],
	write(RM),nl.

go1b:-	
        findall(X,board(_,_,X),Xs),
        write('counting elements'),nl,
        length(Xs,Len),
        write('asserted elements'(Len)),nl.

go2:-
	time(_),nop,time(N),
	initboard2,time(I0),updateboard2,time(U0),accessboard2,time(A0),cleanboard2,time(C0),
	I is I0-N,U is U0-N,A is A0-N,C is C0-N,
	T is I+U+A+C,
	R=[bb_op_all,nop=N,init=I,update=U,access=A,clean=C,total=T],
	write(R),nl.

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        range(I,Min1,Max).

nop:-
	dim(MaxX,MaxY),
	range(_X,1,MaxX),
	range(_Y,1,MaxY),
	fail.
nop.
	
initboard1:-
	dim(MaxX,MaxY),
	range(X,1,MaxX),
	range(Y,1,MaxY),
	assert(board(X,Y,0)),
	fail.
initboard1.

updateboard1:-
	dim(MaxX,MaxY),
	range(X,1,MaxX),
	range(Y,1,MaxY),
        update1(X,Y),
	fail.
updateboard1.
	
update1(X,Y):-retract(board(X,Y,_)),!,assert(board(X,Y,1)).

clean1(X,Y):-retractall(board(X,Y,_)).

% access1(X,Y,V):-clause(board(X,Y,V),_),!.

access1(X,Y,V):-board(X,Y,V),!.

accessboard1:-
	dim(MaxX,MaxY),
	range(X,1,MaxX),
	range(Y,1,MaxY),
        access1(X,Y,_),
%	board(X,Y,_),
	fail.
accessboard1.

cleanboard1:-
	dim(MaxX,MaxY),
	range(X,1,MaxX),
	range(Y,1,MaxY),
        clean1(X,Y),
%	board(X,Y,_),
	fail.
cleanboard1:-
  retractall(board(_,_,_)).

initboard2:-
	dim(MaxX,MaxY),
	range(X,1,MaxX),
	range(Y,1,MaxY),
	def(X,Y,0),
	fail.
initboard2.

updateboard2:-
	dim(MaxX,MaxY),
	range(X,1,MaxX),
	range(Y,1,MaxY),
	set(X,Y,1),
	fail.
updateboard2.

accessboard2:-
	dim(MaxX,MaxY),
	range(X,1,MaxX),
	range(Y,1,MaxY),
	val(X,Y,_),
	fail.
accessboard2.

cleanboard2:-
	dim(MaxX,MaxY),
	range(X,1,MaxX),
	range(Y,1,MaxY),
	rm(X,Y),
	fail.
cleanboard2.

time(T):-statistics(runtime,[_,T]).
