% EXAMPLE of override/2 and hiden argument grammar (HAG) synergy
% needs source license so you can recompile BinProlog
% with -DJUMP_COMPRESS=0 - it still might work in some cases without

% written by Paul Tarau - Thu Sep 14 21:51:52 ADT 1995

:-write('DO NOT FORGET TO DISABLE MACRO EXPANSION OF'),nl,
  write('get_code and friends through peval_io in co.pl'),nl.

go:-
  twice,
  t2s(f(a,b,A,A,_),S),write(S),nl,
  s2t(S,T),write(T),nl.

a(1).
a(2).

b(3).
b(4).
b(5).

c(5).
c(6).

twice:-override(a(_),b(_)),override(a(_),c(_)),a(X),write(over(X)),nl,fail.
twice:-dummy,a(X),write(normal(X)),nl,fail.
twice:-write('you can override twice and it is backtrackable!!!'),nl.

dummy.

d(X,X).

arity:-override(a(_),d(_,_)),a(X),write(booooo(X)),nl,fail.
arity:-write(evrika),nl.
  

% term to string

t2s(T,S):-findall(S,do_t2s(T,S),[S]).

do_t2s(T,S):-
  override(nl,my_nl),
  override(put_code(_),my_put_code(_)),
  override(fast_write(_),my_cwrite(_)),
  numbervars(T,0,_),
  dcg_def(S),  % opens current dcg stream
    generic_write(T,write),
  dcg_val([]). % closes current dcg stream

% the only way to properly emulate write is with side effects
% as write itself fails (to save heap space after doing its work)
% however, generic_write/2 is clean 
% of side effects so grammar emulation is ok

% string to term

s2t(S,T):-findall(T,do_s2t(S,T),[T]).

do_s2t(S,T):-
  override(get_code(_),my_get_code(_)),
  append(S,[46,10],NewS), % adds "." and end-of-line
  dcg_def(NewS), % opens current dcg stream
    read(T),
  dcg_val([]).   % closes current dcg stream


% tools using HAGs (hidden argument grammars - highly recommended !)

my_get_code(X):- #X.

my_nl:- #10.
my_put_code(X) :- #X.
my_cwrite(X):- name(X,Xs), my_write_name(Xs).

my_write_name([]).
my_write_name([X|Xs]):- #X, my_write_name(Xs).


% other stuff

abolished:-errmes('just passed away','whoami?').

abolish_compiled(F/N):-
  N1 is N+1,
  functor(P,F,N1),
  abolish_compiled0(P,Tombstone),
  bb_def(abolished,P,Tombstone).

restore_compiled(F/N):-
  N1 is N+1,
  functor(P,F,N1),
  bb_val(abolished,P,Tombstone),
  restore_compiled0(Tombstone).

abolish_compiled0(P,tombstone(Adr,I0,I1)):-
  tval(predmark,P,var(Adr)),
  array_get0(Adr,0,I0),
  array_get0(Adr,1,I1),
  tval(predmark,abolished(_),var(Killer)),
  array_get0(Killer,0,K0),
  array_get0(Killer,1,K1),
  array_set(Adr,0,K0),
  array_set(Adr,1,K1).

restore_compiled0(tombstone(Adr,I0,I1)):-
  integer(Adr),
  array_set(Adr,0,I0),
  array_set(Adr,1,I1).

m(1).
m(X):-X=2;X=3.

bug:-
  abolish_compiled(m/1),m(X),write(X),nl,fail
;
  fail,restore_compiled(m/1),m(X),write(X),nl,fail.

