foldall(F,X^G,R):-
  new_engine('$found'(X),G,E),
  get(E,the(A)),
  check_return(A,R1),
  combine_with(E,F,R1,R2),
  !,
  R=R2.

combine_with(E,F,R1,R3):-
  get(E,the(A)),
  check_return(A,X),
  call(F,R1,X,R),
  !,
  R2=R,
  combine_with(E,F,R2,R3).
combine_with(_,_,R,R).

check_return(R,X):-nonvar(R),R='$found'(A),!,X=A.
check_return(Ret,_):-return(Ret),fail.

freverse(Xs,Ys):-foldall(rcons,X^(X=[];member(X,Xs)),Ys).  

rcons(Y,X,[X|Y]).

xdiv(X,Y,R):-(X=0;Y=0)->return(0);R is X//Y.

ydiv(X,Y,R):-(X=0;Y=0)->fail;R is X//Y.  

