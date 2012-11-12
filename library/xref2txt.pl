:-[library(xref)].

plain_xref(File):-
  plain_xref_checks(File),
  plain_list_call_graph(File).


plain_list_call_graph(File):-
  db_proc(File,FN),
  nl,write('*** PREDICATE ***: '),write(FN),
  nl,nl,
  write('CALLS: '),
  forall(calles_to(File,FN,Callee),(write(Callee),write(' '))),
  nl,nl,
  write('CALLED FROM: '),
  forall(called_from(File,FN,Caller),(write(Caller),write(' '))),
  nl,nl,nl,
  fail
; nl.


plain_xref_checks(File):-
  plain_xref_checks(File,
     write('UNDEFINED:'),
     write('UNUSED (statically unreachable from definitions)')
  ),
  !.
  

plain_undef_check(DB):-
   undefined(DB,F/N),
   write(F/N),nl,
  fail
; nl.


plain_check_unused(DB):-
  unused(DB,F/N),
  write(F/N),nl,
  fail
; nl.

plain_xref_checks(File,A1,A2):-
  init_xref(File),
  nl,A1,nl,nl,
  plain_undef_check(File),
  nl,A2,nl,nl,
  plain_check_unused(File).

