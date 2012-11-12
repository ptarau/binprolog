% comparision between HAGs and DCGs on arith. expr. grammar
% author: Paul Tarau
% timestamp: Sat Sep 30 13:45:39 ADT 1995

go:-go(4).

go(N):-
  test(N,'DCG_bmark:',dcgs(_)),
  test(N,'HAG_bmark:',hags(_)).

% DCG

axiom --> ex,[';'].

ex --> term,terms.

terms --> [].
terms --> aterm,terms.

aterm --> ['+'],term.
aterm --> ['-'],term.

term --> factor,factors.

factors --> [].
factors --> mfactor,factors.

mfactor -->  ['*'],factor.
mfactor -->  ['/'],factor.

factor --> [id], subfactors.
factor --> ['('], ex, [')'].

subfactors --> [].
subfactors --> ['('], ex, exs, [')'].

exs --> [].
exs --> [','],ex,exs.


% equivalent HAG

axiom :- ex, #(';').

ex :- term,terms.

terms.
terms :- aterm,terms.

aterm:- #('+'),term.
aterm:- #('-'),term.

term :- factor,factors.

factors.
factors :- mfactor,factors.

mfactor :- #('*'),factor.
mfactor :- #('/'),factor.

factor :- #id, subfactors.
factor :- #('('), ex, #(')').

subfactors.
subfactors :- #('('), ex, exs, #(')').

exs.
exs :- #(',') ,ex,exs.


% tools for DCG vs. HAG comparison

dcgs(Xs):-axiom(Xs,[]).

hags(Xs):-dcg_def(Xs),axiom,dcg_val([]).

dummy(_):-fail.

terminal(id).
terminal('+').
terminal('-').
terminal('*').
terminal('/').
terminal(',').
terminal('(').
terminal(')').
terminal(';').

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

writeln([]):-nl.
writeln([X|Xs]):-write(X),writeln(Xs).

time(T):-statistics(runtime,[T,_]).

test(N,Mes,Goal):-
  time(T0),
  do(N,dummy(_)),
  time(T1),
  do(N,Goal),
  time(T2),
  T is (T2-T1)-(T1-T0),
  write(Mes=time(T)),nl,nl.
