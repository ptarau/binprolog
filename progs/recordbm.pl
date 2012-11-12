:-[library(record)].

:-dynamic board/3.

go:-go1,fail;go2.

go1:-
	time(_),nop,time(N),
	initboard1,time(I0),updateboard1,time(U0),accessboard1,time(A0),
	I is I0-N,U is U0-N,A is A0-N,
	T is I+U+A,
	R=[nop=N,init=I,update=U,access=A,total=T],
	write(R),nl,
        findall(X,recorded(board,board(_,_,X)),Xs),length(Xs,Len),
        write('recorded elements'(Len)),nl.

go2:-
	time(_),nop,time(N),
	initboard2,time(I0),updateboard2,time(U0),accessboard2,time(A0),
	I is I0-N,U is U0-N,A is A0-N,
	T is I+U+A,
	R=[nop=N,init=I,update=U,access=A,total=T],
	write(R),nl.

dim(16 ,16).
% dim(2,2).

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
	recordz(board,board(X,Y,0)),
	fail.
initboard1.

updateboard1:-
	dim(MaxX,MaxY),
	range(X,1,MaxX),
	range(Y,1,MaxY),
        update1(X,Y),
	fail.
updateboard1.
	
update1(X,Y):-
  erase(board,board(X,Y,_)),!,
  recordz(board,board(X,Y,1)).

accessboard1:-
	dim(MaxX,MaxY),
	range(X,1,MaxX),
	range(Y,1,MaxY),
        recorded(board,board(X,Y,_)),
	fail.
accessboard1.

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

time(T):-statistics(runtime,[_,T]).
