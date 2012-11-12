%
%   tak
%
%   Evan Tick (from Lisp version by R. P. Gabriel)
%
%   (almost) Takeuchi function (recursive arithmetic)
%   arithmetics reordered for optimal inline execution - Paul Tarau 1994

:-write('In BinProlog use: -h1200 '),nl.
:-set_c_threshold(9).

go :- statistics(global_stack,[H1,_]),
      statistics(runtime,_), 
      tak,
      statistics(runtime,[_,T]),
      statistics(global_stack,[H2,_]),H is H2-H1,
      nl,write('BMARK_tak:' = [time=T,heap=H]), nl.


tak :- tak(18,12,6,R), write(tak(18,12,6)=R), nl.

tak(X,Y,Z,A):-
        X =< Y,!,
        A = Z.
tak(X,Y,Z,A):-
        X1 is X - 1,
        Y1 is Y - 1,
        Z1 is Z - 1,
        tak(X1,Y,Z,A1),
        tak(Y1,Z,X,A2),
        tak(Z1,X,Y,A3),
        tak(A1,A2,A3,A).

