% EXAMPLE of override/2 and hiden argument grammar (HAG) synergy
% needs source license so you can recompile BinProlog
% with -DJUMP_COMPRESS=0 - it still might work in some cases without

% written by Paul Tarau - Thu Sep 14 21:51:52 ADT 1995

go:-
  t2s(f(a,b,A,A,_),S),write(S),nl,
  s2t(S,T),write(T),nl.

% term to string

t2s(T,S):-findall(S,do_t2s(T,S),[S]).

do_t2s(T,S):-
  override(nl,my_nl),
  override(put_code(_),my_put_code(_)),
  override(fast_write(_),my_cwrite(_)),
  numbervars(T,0,_),
  dcg_def(S),
    generic_write(T,write),
  dcg_val([]).

% the only way to properly emulate write is with side effects
% as write itself fail to save heap space after doing its work
% however, generic_write/2 is clean 
% of side effects so grammar emulation is ok

% string to term

s2t(S,T):-findall(T,do_s2t(S,T),[T]).

do_s2t(S,T):-
  override(get_code(_),my_get_code(_)),
  append(S,[46,10],NewS), % adds "." and end-of-line
  dcg_def(NewS),
    read(T),
  dcg_val([]).


% tools using HAGs (hidden argument grammars - highly recommended !)

my_get_code(X):- #X.

my_nl:- #10.
my_put_code(X) :- #X.
my_cwrite(X):- name(X,Xs), my_write_name(Xs).

my_write_name([]).
my_write_name([X|Xs]):- #X, my_write_name(Xs).
