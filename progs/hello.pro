/*
  do not consult directly: demo
  for creation of standalone executables

  requires full source BinProlog license
*/
:-op(200,fx,hello).

:-begin_module(prolog).
:-[wam].
:-end_module(prolog).

main:-write(hello world),nl.
