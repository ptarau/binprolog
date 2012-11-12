% quick SEND+MORE=MONEY with invisible grammars

go:-
  time(_),
  dcg_def([0,1,2,3,4,5,6,7,8,9]),
  puzzle(Show),
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

digit(D):-dcg_val(Xs),select(D,Xs,Ys),dcg_def(Ys).

select(X,[X|Xs],Xs).
select(X,[Y|Xs],[Y|Ys]):-select(X,Xs,Ys).

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

/*

% in BinProlog 3.30

?- go.
  [9,5,6,7]+
  [1,0,8,5]=
[1,0,6,5,2]

time(140) <= it seems faster than with the best FD constraint solvers!!!

*/
