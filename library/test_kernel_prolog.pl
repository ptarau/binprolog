:-[kernel_prolog].

t1:-t1(_).

t1(NewEs):-
  assertK(
    (a(13):-true),
    [],
    Es
  ),
  assertK(
    (a(14):-true),
    Es,
    NewEs
  ),
  clauseK(NewEs,a(X),true),
  println(X),
  fail.

t2:-
  t1(Es),
  assertK((b(X):-a(X)),Es,NewEs),
  solveK(NewEs,b(R)),
  println(R),
  fail.


:-dynamic a/1.
:-dynamic b/1.

a(1).
a(2).

b(X):-a(X).
