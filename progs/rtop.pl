% REMOTE TOPLEVEL

go:-remote_top.

remote_top:-lval(top,remote,yes),rtoplevel.

local_top:-rtoplevel.

% generic top

generic_top(G):-bb_val(top,remote,yes),!,remote_top(G).
generic_top(G):-local_top(G).

rtoplevel:-
  generic_top(start(E,Prompt)),
  repeat,
    rtopstep(Prompt,E,YN),
  YN=stop(G),!,
  generic_top(stop(E,G)).

rtopstep(Prompt,E,YN):-
	ttyprin(Prompt),flush,
        ttyin(top_read_term(Body,Vs)),
	report_answers(Vs,Body,E,YN),
	ttyprint(YN).

report_answers([],Goal,E,YN):-
  generic_top(cmd(E,Goal,YN)).
report_answers([V|Vs],Goal,E,YN):-
  ask_answers(E,Goal,[V|Vs],YN).

ask_answers(E,Goal,Vs,YN):-
  generic_top(call(E,Goal,Vs,Answer)),
  (  Answer\==no -> 
     report_top_vars(Vs),
     another_sol(Ok),
     ( Ok=no,!,YN=yes
       ; fail
     )
  ; !,YN=no
  ).

report_top_vars([V|Vs]):-report_one_var(V),report_more_top_vars(Vs).

report_more_top_vars(Eqs):-
	member(Eq,Eqs),
	ttyprint(','),report_one_var(Eq),
	fail.
report_more_top_vars(_).

report_one_var(V=E):-
	ttyprin(V),ttyprin(=),ttyout(writeq(E)).

another_sol(Ok):-
	is_interactive,!,
	ttyin(get_code(A)),user_action(A,Ok),ttynl.
another_sol(yes):-ttyprint(';'),ttynl.

user_action(10,no):-!.
user_action(59,yes):-!,ttyin(get_code(10)).
user_action(_,Ok):-ttyprin(' ; for more, <return> otherwise '),
  ttyin(get_code(10)),ttyin(get_code(U)),user_action(U,Ok).

% local interface

local_top(start(0,'?- ')):-!.

local_top(stop(_,G)):-!,G->true;true.

local_top(call(_,G,Vs,Answer)):-!,G,Vs=Answer.

local_top(cmd(_,G,R)):-topcall(G)->R=yes;R=no.

% remote interface

remote_top(start(E,'??- ')):-!,
  ttyprint('
    BinProlog remote toplevel 0.1 (alpha stage)
    !!!this simple client/server does NOT redirect write commands!!!
    Avoid write/1 and friends, please use QUERY VARIABLES for answers.
  '),
  create_remote_engine(E),
  remote_top(cmd(E,set_load_method(oconsult),yes)).
remote_top(stop(E,G)):-!,
  stop_remote_engine(E),
  ( member(G,[halt])->remote_run(G)
  ; true
  ).
remote_top(call(E,Goal,Vs,Answer)):-!,
  default(max_answers(Max),Max=100),
  load_remote_engine(E,metacall(Goal),Vs),
  for(_,1,Max),
    ( ask_remote_engine(E,Answer)->Vs=Answer
    ; !, Answer=no
    ).
remote_top(cmd(E,G,R)):-!,rtop_cmd(E,G,R).

rtop_cmd(_,G,R):-member(G,
  [ interactive(_),
    set_password(_)
  ]),!, % if intended to be executed locally
  (G->R=yes;R=no).
rtop_cmd(_,G,R):-member(G,[halt,stop_server,end_of_file]),!,R=stop(G).
rtop_cmd(E,Goal,R):-
  load_remote_engine(E,metacall(Goal),yes),
  ( ask_remote_engine(E,R)->true
  ; R=no
  ).

