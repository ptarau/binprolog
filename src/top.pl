% TOPLEVEL

topcall(G):-metacall(G).

prolog_run(_):-(prolog_run0,fail;true).

prolog_run0:-
  exec_run_time_commands,
  fail.
prolog_run0:-
  string_op(2,0,QueryChars), % +,+,-
  !,
  call_prolog(QueryChars,AnswerChars),
  string_op(4,AnswerChars,_). % +,+,-
prolog_run0:- % default, calls toplevel/0, unless main/0 is defined
  call_ifdef(main,toplevel).
  
call_prolog(QueryChars,AnswerChars):-
   term_chars(QueryTerm,QueryChars),
   debugmes(enter_call_prolog_term(QueryTerm)),
   call_prolog_term(QueryTerm,AnswerTerm),
   debugmes(exit_call_prolog_term(AnswerTerm)),
   term_chars(AnswerTerm,AnswerChars).

call_prolog_term(QueryTerm,AnswerTerm):-
   nonvar(QueryTerm),
   ( QueryTerm=(Vars:-Goal)->Query=Goal,Answer=Vars
   ; QueryTerm=(Vars^Goal)->Query=Goal,Answer=Vars
   ; QueryTerm=the(Vars,Goal)->Query=the(Vars,Goal,Answer)
   ; QueryTerm=(K:Vars*Goal),nonvar(K)->Query=find_at_most(K,Vars,Goal,List),Answer=List
   ; QueryTerm=(Vars*Goal)->Query=findall(Vars,Goal,List),Answer=List
   ; Query=the(QueryTerm,QueryTerm,Answer)   % NEW from 7.36 !
   ),
   ( topcall(Query)->AnswerTerm=Answer
   ; AnswerTerm=no
   ).

the(X,G,R):-copy_term(the(X,G),the(NewX,NewG)),NewG,!,R=the(NewX).
the(_,_,no).

the(X,G):-the(X,G,the(X)).
the(G):-the(G,G).

% LOCAL OR REMOTE TOPLEVEL

toplevel:-
  % quiet(2),
  repeat,
    topstep('?- '),
  fail.

topstep(Prompt):-
	ttyprin(Prompt),flush,
        ttyin(top_read_term(Body,Vs)),
	report_answers(Vs,Body,YN),
	ttyprint(YN).

report_answers([],Goal,YN):-
  topcall(Goal)->YN=yes
  ; YN=no.
report_answers([V|Vs],Goal,YN):-
  ask_answers(Goal,[V|Vs],YN).

ask_answers(Goal,[V|Vs],YN):-
  topcall(Goal),
     report_one_var(V),report_top_vars(Vs),
     another_sol(Ok),
     ( Ok=no,!,YN=yes
       ; fail
     )
   ; YN=no,!.

report_top_vars(Eqs):-
	member(Eq,Eqs),
	ttyprint(','),report_one_var(Eq),
	fail.
report_top_vars(_).

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

