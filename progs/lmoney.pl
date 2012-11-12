% quick SEND+MORE=MONEY with linear implication

:-dynamic digit/1.

go:-
  time(_),
  digit(0)-:digit(1)-:digit(2)-:digit(3)-:digit(4)-:
  digit(5)-:digit(6)-:digit(7)-:digit(8)-:digit(9)-: puzzle(Show),
  time(T),
  Show,
  write(time(T)),nl.


puzzle(show(S,E,N,D,M,O,R,Y)):-
  digit(D),digit(E),add_digit(D,E,Y,  0,C1),digit(Y),
  digit(N),digit(R),add_digit(N,R,E, C1,C2),
  digit(O),         add_digit(E,O,N, C2,C3),
  digit(S),S>0,
  digit(M),M>0,     add_digit(S,M,O, C3,
                                  M).

add_digit(D1,D2,Res,C1,C2):-
    S is D1+D2+C1,
    Res is S mod 10,
    C2 is S // 10.

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
