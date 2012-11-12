go:-
  time(_),
  puzzle(Show,[0,1,2,3,4,5,6,7,8,9],_),
  time(T),
  Show,
  write(time(T)),nl.


puzzle(show(S,E,N,D,M,O,R,Y))-->
  digit(D),digit(E),{add_digit(D,E,Y,  0,R1)},digit(Y),
  digit(N),digit(R),{add_digit(N,R,E, R1,R2)},
  digit(O),         {add_digit(E,O,N, R2,R3)},
  digit(S),{S>0},
  digit(M),{M>0},   {add_digit(S,M,O, R3,
                                    M)}.

add_digit(C1,C2,Res,R1,R2):-
    S is C1+C2+R1,
    Res is S mod 10,
    R2 is S // 10.

digit(X,[X|Xs],Xs).
digit(X,[Y|Xs],[Y|Ys]):-digit(X,Xs,Ys).

show(S,E,N,D,M,O,R,Y):-
  write('  '),
  write([S,E,N,D]),
  write(+),nl,
  write('  '),
  write([M,O,R,E]),
  write(=),nl,
  write([M,O,N,E,Y]),nl,
  fail
; nl.

time(T):-statistics(runtime,[_,T]).
