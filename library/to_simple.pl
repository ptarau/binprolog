to_simple(Files,File):-
  Db='$temp',
  db_clean(Db),
  (atomic(Files)->Fs=[Files];Fs=Files),
  foreach(member(F,Fs),portable_consult(F,Db)),
  db_save(Db,File).

portable_consult(File,Db):-
  ( is_prolog(binprolog)->consult(File,Db)
  ; db_consult(File,Db)
  ).

/*

Usually you make a file like:

project.pl containing:

:-[myfile1].
:-[myfile2].
...

and then call to_simple with it. Here is an example
with a single file - note that discontiguous predicates are handled
without problems. The same applies to multifile predicates.

?- to_simple('t1.pl','t2.pl').

% input: file: t1.pl

a(1).

b(X):-a(X).

a(2).


% output: file: t2.pl

':-'(a(1),true).
':-'(a(2),true).
':-'(b(_x2647),a(_x2647)).
 
*/
