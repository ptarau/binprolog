% DCG

axiom --> ex,[';'].

ex --> term,terms.

terms --> [].
terms --> ['+'],term,terms.
terms --> ['-'],term,terms.

term --> factor,factors.

factors --> [].
factors --> ['*'],factor,factors.
factors --> ['/'],factor,factors.

factor --> [id], subfactors.
factor --> ['(',ex,')'].

subfactors --> [].
subfactors --> ['('], ex, exs, [')'].

exs --> [].
exs --> [','],ex,exs.

% HAG
axiom :- ex, #(';').

ex :- term,terms.

terms.
terms :- #('+'),term,terms.
terms :- #('-'),term,terms.

term :- factor,factors.

factors.
factors :- #('*'),factor,factors.
factors :- #('/'),factor,factors.

factor :- #id, subfactors.
factor :- #('('), ex, #(')').

subfactors.
subfactors :- #('('), ex, exs, #(')').

exs.
exs :- #(',') ,ex,exs.


% common data

terminal(id).
terminal('+').
terminal('-').
terminal('*').
terminal('/').
terminal(',').
terminal('(').
terminal(')').
terminal(';').

% DCG vs. HAG comparison

generate(0,[]).
generate(N,[X|Xs]):-N>0,N1 is N-1,generate(N1,Xs),terminal(X).

do(N,Goal):-
  write(begin(N)),nl,
  generate(N,Xs),
  arg(1,Goal,Xs),
  Goal,
  writeln(Xs),
  fail.
do(N,_):-write(end(N)),nl.

test(N,Mes,Goal):-
  time(T0),
  do(N,dummy(_)),
  time(T1),
  do(N,Goal),
  time(T2),
  T is (T2-T1)-(T1-T0),
  write(Mes=time(T)),nl,nl.
  
writeln([]):-nl.
writeln([X|Xs]):-write(X),writeln(Xs).

time(T):-statistics(runtime,[T,_]).

dcgs(Xs):-axiom(Xs,[]).

hags(Xs):-dcg_def(Xs),axiom,dcg_val([]).

dummy(_):-fail.

go(N):-
  test(N,'DCG_bmark:',dcgs(_)),
  test(N,'HAG_bmark:',hags(_)).

go:-go(4).
