file2clause(F0,C):-
  seeing(S),
  find_file(F0,F),!,
  see(F),
  repeat,
    read(X),
    ( X=end_of_file,!,see(F),seen,see(S),fail
    ; C=X
    ).


file2byte(F0,C):-
  seeing(S),
  find_file(F0,F),!,
  see(F),
  repeat,
    get0(X),
    ( X =:= -1,!,see(F),seen,see(S),fail
    ; C=X
    ).

file2pred(F0,FNCs):-
  seeing(S),
  find_file(F0,F),!,
  see(F),
  repeat,
    get_a_predicate(mem,FN,Cs),
    ( FN==end_of_file/0,Cs=[end_of_file],!,
      see(F),seen,see(S),fail
    ; FNCs=FN-Cs
    ).

