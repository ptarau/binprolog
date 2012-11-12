go:-
  for(I,0,255),
    to_string(I,S),
    namecat('ping -n 1 -w 200 192.168.1.',S,'',Cmd),
    % quiet(10),
    (pcollect(Cmd,Rs)->true;Rs=[]),
    % quiet(2),
    ( is_sstring("100%",Rs)->write(I),write(' ')
    ; nl,println(Cmd)
    ),
    fail.
go.

is_sstring(Ss,Cs):-append(_,Xs,Cs),is_sstring(Ss,Xs,_),!.

is_sstring([])-->[].
is_sstring([X|Xs]) --> [X],is_sstring(Xs).
