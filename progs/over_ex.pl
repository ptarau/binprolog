% needs source license so you can recompile BinProlog
% with -DJUMP_COMPRESS=0 - it still might work in some cases without

go:-g0,fail;g1,fail;g2,fail;g3,fail;g4,fail;g,fail.
    %bug,fail;
    %not_a_bug_anymore.

a(1).
a(2).

b(10).
b(20).

c(100).
c(200).

d(1000).
d(2000).
d(3000).
d(X):-X=4000.

g0:-override(a(_),b(_)),
    override(b(_),c(_)),
    (override(c(_),d(_)),a(X),write(X),nl,fail;
     dummy,c(X),write(c(X)),nl,fail).


forward(F,N,G,M):-
  functor(P,F,N),
  functor(Q,G,M),
  override(P,Q).

g:-forward(a,1,b,1),forward(b,1,c,1),g1.
g1:-dummy,a(X),write(X),nl,fail.

dummy.

g2:-override(a(_),d(_)),a(X),write(X),nl,fail. %NO

g3:-override(d(_),a(_)),d(X),write(X),nl,fail. %NO

g4:-override(d(_),a(_)),d(X),write(X),nl,fail. % YES


let_pred(F/N,G/M,InG):-
  forward(F,N,G,M),
  InG,
  forward(G,M,F,N). % this gives a loop


not_a_bug_anymore:-let_pred(b/1,c/1,b(X)),b(Y),write(X+Y),nl,fail.

%bug:-override(put(_),string_put(_)),put_string("gaga").

bug:-override(put(_),string_put(_)),put_string("gaga"),nl,fail.
bug:-override(my_put(_),string_put(_)),put_string("gaga"),nl,fail.
bug:-put_string("gaga"),nl.

put_string([]).
put_string([X|Xs]):-
%  put(X), -- impossible to override inline builtins -- trigers the bug
  my_put(X),
  put_string(Xs).

string_put(X):-cwrite(X),nl. % only for tracing purpose

my_put(X):-put(X).
