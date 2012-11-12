% deprecated operators

:-op(500,yfx,(#)).
:-op(500,fx,(#)).

:-op(1150,fx,(delphi)).
:-op(1150,fx,(memo)).
:-op(600,xfx,(:=:)).
:-op(950,xfy,(-:)).
:-op(950,xfx,(=:)).
:-op(950,xfy,(-::)).
:-op(950,xfy,(=::)).
:-op(950,xfy,(=>>)).

:-op(700,xfx,(:=)).

:-op(450,fx,(::)).
:-op(880,xfx,(extends)).
:-op(890,xfx,(with)).
:-op(890,xfy,(::)).

% for smooth http reading :-)
:-op(420, xfx, (://)).
:-op(400, yfx, (/~)).

:-op(1150,fx,(type)).

% deprecated predicates

remote_top:-toplevel.

% adds a (assumed ground) object unless it is there
memoq(Key,Name,X):-
  cmembq(Key,Name,X),!.
memoq(Key,Name,X):-
  addq(Key,Name,X).

% Basic Jinni compatibility: see more in jlib.pl

chars_to_words(Cs,Ws):-
   chars_to_words(Cs,Ws,Vs),
   equate_vars(Vs).

chars_to_words(Cs,Ws,Vs):-
   read_tokens_from_chars(Cs,Ts,Vs),
   map(extract_token,Ts,Ws).

equate_vars([]).
equate_vars([var(X,X,_)|Es]):-equate_vars(Es).

% reads words from a constant line
words_from(Line,Ws):-
   name(Line,Cs),
   chars_to_words(Cs,Ws).

% reads a list of space separated words, each possibly a term containing vars 
old_read_words(Ws):-
  read_tokens(Ts,_),
  map(extract_token,Ts,Ws).

extract_token(T,R):-var(T),!,R=T.
extract_token(T,R):-number(T),!,R=T.
extract_token(string(Cs),R):-!,R=string(Cs).
extract_token(T,R):-compound(T),arg(1,T,X),!,extract_token(X,R).
extract_token('((',R):-!,R='('.
extract_token('))',R):-!,R=')'.
extract_token(T,T).


% MOBILE THREAD HELPERS

% wraps continuation of current thread to be taken
% by inner move_thread goal to be executed remotely 
wrap_thread(Goal):-
  capture_cont_for(Goal).

% wraps continuation and retruns LeftOver when execution hits return/0
wrap_thread(Goal,LeftOver):-left_over_cont(LeftOver)-::capture_cont_for(Goal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% AG stuff to be deprecated

% INVISIBLE DCG connect operation: normally macro-expanded
% renamed as Assumption Grammars

'#'(Word):-dcg_connect(Word).
'#<'(Xs):-dcg_def(Xs).
'#>'(Xs):-dcg_val(Xs).
'##<'(N):-dcg_tell(N).
'##>'(N):-dcg_telling(N).

% example: dcg_phrase((#a,#b,#c),X).
dcg_phrase(Axiom,Phrase):-dcg_phrase(1,Axiom,Phrase).

dcg_phrase(DcgStream,Axiom,Phrase):-
  '##>'(X),'##<'(DcgStream),
    '#<'(Phrase),
      Axiom,
    '#>'([]),
  '##<'(X).

dcg_apply(Method):-
  term_append(Method,args(I,O),Goal),
  '#>'(I),
  Goal,
  '#<'(O).


% AG tools
% Matches a list of chars - some may be given as variables

% setarg based AGs: a DCG implementation without prog transformation

% ag basics

ag_connect(X):-
  ag_store(S,N),
  arg(N,S,[X|Xs]),
  setarg(N,S,Xs).

ag_def(Xs):-
  ag_store(S,N),
  setarg(N,S,Xs).

ag_val(Xs):-
  ag_store(S,N),
  arg(N,S,Xs).

ag_tell(N):-
  ag_store(S,_),
  setarg(33,S,N).

ag_telling(N):-
  ag_store(_,N).

ag_store(S,N):-
  current_engine_id(I),
  lval(ag_store,I,S),
  init_ag_store(S,N).
  
init_ag_store(S,N):-var(S),!,N=1,functor(S,ag_store,33),arg(33,S,N).
init_ag_store(S,N):-arg(33,S,N). % already done


% ag tools

match_word([C|Cs]):- #C,!,match_word(Cs).
match_word([]).

% Matches chars until a Stop char is found

match_before(Stops,[],Stop):- #Stop,member(Stop,Stops),!.
match_before(Stops,[C|Cs],Stop):- #C,match_before(Stops,Cs,Stop).

% NL tokenizer - converts NL strings of chars to lists of words

bad_codes_words(Cs,Ws):-'#<'([32|Cs]),dcg_words(Ws),!,'#>'([]).

dcg_words(Ws):-dcg_star(dcg_word,Ws),dcg_space.

dcg_word(W):-dcg_space,(dcg_plus(dcg_is_letter,Xs);dcg_one(dcg_is_punct,Xs)),!,name(W,Xs).

dcg_space:-dcg_star(dcg_is_space,_).

dcg_is_space(X):- #X, member(X,[32,7,9,10,13]).

dcg_is_letter(X):- #X, is_an(X).

dcg_is_punct(X):- #X, \+is_an(X). %, (is_spec(X);member(X,"!,;`""'()[]{}*")).


% regexp tools with  AGs + high order

dcg_one(F,[X]):- call(F,X).

% added nonvar - deals with possible GC bug - which unbinds DCG stream
dcg_star(F,XXs):-'#>'(Current),nonvar(Current),dcg_star0(F,XXs).

dcg_star0(F,[X|Xs]):- call(F,X),!,dcg_star0(F,Xs).
dcg_star0(_,[]).

dcg_plus(F,[X|Xs]):- call(F,X),dcg_star(F,Xs).


% asserts a program from a stream of clauses
% (usually coming from a URL connection or a file)
  
assume_from_chars(Cs):-
  findall(Expanded,
    ( 
      read_terms_from_chars(Cs,Clause),
      expand_term(Clause,Expanded)
    ),
    Clauses
  ),
  map(assumei,Clauses).  

% string to tokens

read_tokens_from_chars(S,Ts,Vs):-
  ( nonvar(S),
    safe_override_call(
      get_code(_),
      gg_get_code(_),
      do_s2tokens(S,TsVs)
    ) -> 
    TsVs=Ts-Vs
  ; errmes(read_tokens_from_chars,'arg 1 should be nonvar')
  ).

do_s2tokens(S,Ts-Vs):-
  End=[46,10], % "." + end-of-line
  append(S,End,NewS),
  'gg_def'(NewS), % opens current dcg stream
    read_tokens(Ts,Vs),
  'gg_val'([]).   % closes current dcg stream

% from net.pl - assumption based


  
  % ************CHANGE FOR NEW BinProlog VERSIONS ****************



  default_well_known_master(H,P):-
    default_well_known_host(H),  
    default_well_known_port(P1),P is P1-1.

  default_master_server(H,P):-
    default(master_server(H,P),default_well_known_master(H,P)).

  set_master_server(Host,Port):-
    set_default(master_server(Host,Port)).

  default_proxy_server(H,P):-
    default( proxy_server(H,P),
             ( fail % DO NOT ACTVATE THIS UNLESS IT ACTUALLY EXISTS
               %default_well_known_host(H),
               %default_well_known_port(P)
             )
          ).


  set_proxy_server(Host,Port):-
    set_default(proxy_server(Host,Port)).



  default_code(X):-default(code(X),current_user_file(X)).

  set_code(RemoteFile):-
    set_default(code(RemoteFile)).

  default_client_action(X):-
    default(client_action(X),X=term_client_action).

  set_client_action(R):-
    set_default(client_action(R)).

  default_server_action(X):-
    default(server_action(X),X=term_server_action).

  set_server_action(W):-
    set_default(server_action(W)).

  default_server_interactor(X):-
    default(server_interactor(X),X=term_server_interactor).

  set_server_interactor(W):-
    set_default(server_interactor(W)).

  get_id(X):-bb_val(id,no,X),!.
  get_id(0).

  set_id(X):-bb_let(id,no,X).

  xdefault_port(X):-default(port(X),default_well_known_port(X)).

  default_well_known_host('129.120.36.15'). % go, Univ of North Texas
  default_well_known_port(7001).
  

  random_password:-
    random(N),
    symcat(pw,N,P),
    set_password(P).

  random_password(P):-
    default(
      password(P),
      (random_password,get_password(P))
    ).
    
    
% interface

  run_bserver:-run_bserver(_). % runs server on the first free port

  run_default_server:-default_this_port(Port),  run_server(Port).

  fork_server:-fork_server(_).

  fork_server(Pid):-default_this_port(Port),  fork_server(Port,Pid).

  stop_server:-default_password(P),stop_server(P).
 
% generic client/server predicates

fork_server(Port,Pid):-
    (free_port(Port)->true;errmes('port busy',Port)),
    unix_fork(Pid),
    ( Pid=:=0->
      quietmes(1,'forking with pid'(Pid)),
      quiet(Q),quiet(10),
                run_server(Port),               % child: a daemon
      quiet(Q),
      quietmes(1,'forked server done, pid'(Pid)),
      halt
    ; sleep(1)                                  % parent
    ).

free_port(Port):-new_server(Port,NewServer),close_socket(NewServer).

new_server_on_free_port(Port,NewServer):-
  ( nonvar(Port)->new_server(Port,NewServer)
  ; get_free_port(new_server(Port,NewServer),Port)
  ).
get_free_port(FreePort):-get_free_port(free_port(FreePort),FreePort).

get_free_port(Test,FreePort):-get_free_port(64,Test,FreePort).

get_free_port(MaxTry,Test,FreePort):-
  ( bb_val(last,this_port,P)->true
  ; default_this_port(P),bb_def(last,this_port,P)
  ),
  Max is MaxTry-1,
  quietmes(1,default_this_port(P)),
  ( FreePort=P
  ; for(_,1,Max),random(I),FreePort is P+1+(I mod 998)
  ),
  Test,
  bb_set(last,this_port,FreePort),
  quietmes(1,got_free_port(FreePort)),
  !.
get_free_port(MaxTry,_,_):-
  errmes('unable to find free port for server', tried(MaxTry,times)).

% if nonvar(Port) sticks to it, if var(Port) tries to find a free one
run_bserver(Port):-
  Fuel is 1<<28,
  vset(err_id,int(77)),
  new_server_on_free_port(Port,NewServer),
  (assumed(register)->register_server(Port);true),
  default_password(Pwd),
  default_this_host(ServerHost),
  quietmes(2, running_server(this_host(ServerHost), this_port(Port), 
              server(NewServer), password(Pwd))),
    this_port(Port)=>> % needed to set the port??$$
     server(NewServer)=>>
       server_loop(NewServer,Fuel),
  close_socket(NewServer),
  quietmes(2, server_done(port(Port), server(NewServer), password(Pwd))),
  vset(err_id,int(0)).

server_loop(Server,Fuel):-
  default_password(W),
  for(I,1,Fuel),
    quietmes(1,'serving socket, fuel'(Server,I)),
    interact(Server),   
    assumed(server_done(W)),
  !,
  clean_up_servants,
  quietmes(1,server_done(server=Server,fuel=I)).


interact(Server):-
  default_server_action(Action),
  apply_server_action(Server,Action),
  !.
interact(Server):-
  \+ errmes(bad_interaction,server(Server)).

apply_server_action(Server,Action):-
    % blocks here until one of a max of 64 waiting clients connects
    new_service(Server,Service),
    apply_service_action(Service,Action)
  ->true
  ; \+ errmes(failed_interaction(Server),Action).

new_service(Server,Service):-
  default_timeout(T),
  new_service(Server,T,Service).
  
apply_service_action(Service,Action):-
   has_threads,
   assumed(server(S)),\+ assumed(force_fg(S)),!,
   %listing,
   default_password(Pwd),
   default_server_interactor(SI),
   default_server_action(SA),
   default_this_port(P),
   G=bg(run_service_action(Pwd,SI,SA,S,P,Service,Action),Thread),
   G,
   debugmes(service_action(G,thread(Thread))).
apply_service_action(Service,Action):-
   run_service_action(Service,Action),
   debugmes(service_action(Service,Action)).

run_service_action(Service,Action):-
   socket(Service) =>> 
      call(Action,Service),
   do_postponed,
   close_socket(Service). % only the client should close?

run_service_action(Pwd,SI,SA,S,P,Service,Action):-
   password(Pwd)=>>
    server_interactor(SI)=>>
     server_action(SA)=>>
      server(S)=>>
         this_port(P)=>>
           socket(Service) =>> 
             call(Action,Service),
   do_postponed,
   close_socket(Service). % only the client should close?

do_postponed:-
  assumed(todo(G)),!,
  debugmes(entering_do_postponed(G)),
  (G->true;true),
  debugmes(exiting_do_postponed(G)),
  do_postponed.
do_postponed:-
  debugmes(finished(do_postponed)).

new_client(Connection):-
  default_host(Host),
  default_port(Port),
  new_client(Host,Port,Connection).

ask_server(Question,Answer):-
  quietmes(1,ask_server(Question)),
  assumed(to_all(ServerPattern)),!,
  all_servers(ServerPattern,Xs),
  ask_all_servers(Xs,Question),
  Answer=yes.
ask_server(Question,Answer):-
  ask_a_server(Question,Answer).

ask_all_servers(ListOfServers,Question):-
  quietmes(1,ask_all_servers(ListOfServers,Question)),
  member(server(_,H,P),ListOfServers),
  host(H)=>>port(P)=>>ask_a_server(Question,_),
  fail.
ask_all_servers(_,_).


ask_a_server(Question,Answer):-
  default_host(Host),
  default_port(Port),
  default_client_action(Interactor),
  ask_a_server(Interactor,Host,Port,Question,Answer).

ask_a_server(Host,Port,Question,Answer):-
  default_client_action(Interactor),
  ask_a_server(Interactor,Host,Port,Question,Answer).

ask_a_server(Interactor,Host,Port,Question,Answer):-
  default_proxy_server(ProxyH,ProxyP),!,
  (ProxyH==Host,ProxyP==Port->
    \+ errmes(bad_proxy_same_as_host(ProxyP,ProxyP),asking(Question)),
    R=no
  ; directly_ask_a_server(
      Interactor,ProxyH,ProxyP,proxy(Host,Port,Question),R
    )
  ),
  Answer=R.
ask_a_server(Interactor,Host,Port,Question,Answer):-
  directly_ask_a_server(Interactor,Host,Port,Question,Answer).

directly_ask_a_server(Interactor,Host,Port,Question,Answer):-
  call(Interactor,Host,Port,Question,Result),
  !,
  Answer=Result.
directly_ask_a_server(_,_,_,stop(_),yes):-!.
directly_ask_a_server(_,_,_,halt(_),yes):-!.
directly_ask_a_server(Interactor,H,P,Q,no):-
  \+ errmes(bad_interaction_with(Interactor,H,P),ask_a_server(Q)),
  ( not_master_server, assumed(to_all(_)) ->
    quietmes(1,asking_master_to_check(H,P)),
    refresh_servers(server_id(_,H,P))
  ; true
  ).

not_master_server:-
  \+ assumed(server_interactor(master_server_interactor)).

% helpers for Linda server operations

serve_cin(X,R):-current_db(Db),serve_cin(Db,X,R).

serve_cin(Db,X,R):-nonvar(X),db_retract1(Db,X),!,R=X.
serve_cin(Db,X,R):-var(X),db_clause(Db,X,true),db_retract1(Db,X),!,R=X.
serve_cin(_,_,no).

serve_rd(X,R):-current_db(Db),serve_rd(Db,X,R).

serve_rd(Db,X,R):-db_clause(Db,X,true),!,R=X.
serve_rd(_,_,no).

serve_out(X,R):-current_db(Db),serve_out(Db,X,R).

serve_out(_,X,no):-var(X),!.
serve_out(_,no,no):-!.
serve_out(_,yes,no):-!.
serve_out(Db,X,R):-serve_delayed_in(Db,X),R=yes.


serve_cout(X,R):-current_db(Db),serve_cout(Db,X,R).

serve_cout(Db,X,R):-serve_rd(Db,X,no)->serve_out(Db,X,R)
               ; R=no.

%serve_collect(X,G,Xs):-nonvar(G),!,findall(X,G,Xs).
%serve_collect(X,G,Xs):-serve_facts(X,G,Xs).

serve_facts(X,G,Xs):-current_db(Db),serve_facts(Db,X,G,Xs).

serve_facts(Db,X,G,Xs):-findall(X,db_clause(Db,G,true),Xs).

serve_stop(W):-check_password(W),assumel(server_done(W)).

serve_all(G,R):-serve_facts(G,G,R).

serve_halt(W,yes):-
  check_password(W),
  assumed(socket(S)),
  sock_write(S,"yes"),!,
  halt.
serve_halt(_,no).

this_id(R):-
  default_this_host(H),default_this_port(P),
  this_id(H,P,R).

this_id(H,P,server_id(InfoList,H,P)):-
   get_id(No),
   default_login(U),
   % default_password(W),
   unix_pid(Pid),
   % default_server_interactor(I),default_server_action(A),
   InfoList=[server(No),login(U),pid(Pid),
   % server_interactor(I),action(A),
   language(bp)
   ].

% pretty secure, Linda, proxy and chat-only server


term_server_interactor(cin(X),R):-serve_cin(X,R).
term_server_interactor(out(X),R):-serve_out(X,R).
term_server_interactor(cout(X),R):-serve_cout(X,R).
term_server_interactor(rd(X),R):-serve_rd(X,R).
term_server_interactor(all(X),Xs):-serve_facts(X,X,Xs).
term_server_interactor(all(X,G),Xs):-serve_facts(X,G,Xs).
term_server_interactor(id(R),R):-this_id(R).
term_server_interactor(ping(P),T):-default_password(P)->rtime(T);T=no.
term_server_interactor(stop(W),yes):-serve_stop(W).
term_server_interactor(halt(X),R):-serve_halt(X,R).
term_server_interactor(add_servant(X),R):-serve_add_servant(X,R).
term_server_interactor(refresh_servants(W),R):-
  serve_refresh_servants(W),R=yes.
term_server_interactor(mes(From,Cs),Answer):-
  show_mes(From,Cs,Answer),
  forward_to_servants(mes(From,Cs)).
term_server_interactor(proxy(H,P,Q),R):-
  ask_a_server(H,P,Q,A)->R=A;R=no.
% term_server_interactor(out0(X),R):-serve_out0(X,R). % $$ redundant
% to be over ridden !!! \/
term_server_interactor(the(X,G,Pwd),R):-
  serve_secure_the(Pwd,X,G,R).
term_server_interactor(call_back_in(X,H,P,W),R):-
   serve_call_back_in(X,H,P,W,R).

serve_call_back_in(X,H,P,W,R):-
  current_db(Db),
  (  serve_cin(Db,X,R),R\==no->true
  ;  delay_in(Db,X,H,P,W),R=no
  ).

proxy(Host,Port,Question,Answer):-
  ask_a_server(proxy(Host,Port,Question),Answer).

show_mes(From,Cs,yes):-quiet(Q),Q<5,!,
  write(From),write('> '),write_chars(Cs),nl.
show_mes(_,_,no).

clean_up_servants:-
  this_id(Id),
  forward_to_servants(stop(Id)),
  abolish(servant_id/1).

remove_dead_servants:-
  default_timeout(T), % sec
  ( T=:=0->Age=60
  ; Age is T
  ),
  abstime(Now),
  Servant=servant(_,Stamp),
  asserted(servant_id(Servant)),
  \+asserted(query(Servant,_,_)), % if there let's wait
  Stamp+Age<Now,                  % too old for interactive chat
  retract1(servant_id(Servant)),
  retractall(query(Servant,_)),
  fail.
remove_dead_servants.
  
clean_up_servant(Id):-
  retractall(servant_id(Id)),     
  retractall(query(Id,_)).

% has to be a forward loop - because of +todo/1 actions!

forward_to_servants(Query):-
  findall(Id,clause(servant_id(Id),true),Ids),
  forward_to_each_servant(Ids,Query),
  debugmes(forward_to_each_servant(Ids,Query)).

forward_to_each_servant([],_).
forward_to_each_servant([Id|Ids],Query):-
   serve_out(query(Id,Query),_),
   forward_to_each_servant(Ids,Query).


% used in interact/1
term_server_action(ServiceSocket):-
  socket(ServiceSocket)=>>term_server_step(ServiceSocket).

term_server_step(Socket):-
  server_try(Socket,sock_read(Socket,QuestionChars)),
  question2answer(QuestionChars,AnswerChars),
  server_try(Socket,sock_write(Socket,AnswerChars)).

question2answer(QuestionChars,AnswerChars):-
  term_chars(Question,QuestionChars),
  default_this_port(P),
  quietmes(1,this_port(P)+question(Question)),
  default_server_interactor(Filter),
  call(Filter,Question,Answer),!,
  term_chars(Answer,AnswerChars),
  quietmes(1,port(P)+answer(Answer)),
  !.
question2answer(QuestionChars,"no"):-
  (term_chars(Question,QuestionChars)->true;Question=bad_question),
  default_server_action(A),
  default_server_interactor(I),
  \+ errmes(failed_or_unauthorized_server_operation(action=A,interactor=I),
            Question
  ).

%server_try(_,Goal):-synchronize_on(1,Goal),!.
server_try(_,Goal):-Goal,!.
server_try(Socket,Goal):-
  close_socket(Socket),
  default_server_action(Action),
  errmes(server_socket_error_for(Action),Goal).

qa(Q):-qa(localhost,7001,Q).

qa(H,P,Q):-qa(H,P,Q,Q,none,the(Q)).

qa(H,P,X,G,W,A):- term_client_action(H,P,the(X,G,W),R),A=R.


% BinProlog Linda client operations
term_client_action(Host,Port,Question,Answer):-
  new_client(Host,Port,Socket),
  socket(Socket)=>>term_client_step(Socket,Question,Answer),!,
  close_socket(Socket).

term_client_step(Socket,Question,Answer):-
  term_chars(Question,QuestionChars),
  client_try(Socket,sock_write(Socket,QuestionChars)),
  client_try(Socket,sock_read(Socket,AnswerChars)),
  term_chars(Answer,AnswerChars),
  quietmes(1,term_client_step(Socket,Question,Answer)),
  !.
term_client_step(_,_,no).

%client_try(_,Goal):-synchronize_on(1,Goal),!.
client_try(_,Goal):-Goal,!.
client_try(Socket,Goal):-
  close_socket(Socket),
  default_client_action(Action),
  errmes(client_socket_error_for(Action),Goal).

% Linda clients for BinProlog and Java Linda servers
% -- to be merged once both servers will provide similar functions

% Linda operations common to Java and BinProlog servers

xout(X):-ask_server(out(X),yes).
xcout(X):-ask_server(cout(X),_).
xall(X,Xs):-ask_server(all(X),Xs).
xrd(X):-ask_server(rd(X),R),!,R\==no,X=R.
xcin(X):-ask_server(cin(X),R),!,R\==no,X=R.

that_id(X):-ask_server(id(X),R),R\==no,X=R.

in0(X):-cin(X)->true;wait_for_in0(100,X). % $$ legacy!

wait_for_in0(Max,X):-for(I,1,Max),J is 1+I mod 3,sleep(J),cin(X),!.

xin(X,R):-in(X)->R=yes;R=no.
xin(X):-
  get_socket_for_in(Port,ServerSocket), % socket reuse mechanism ?
  force_fg(ServerSocket)-::
     launch_in(X,Port,ServerSocket).

launch_in(X,Port,Server):-
  (
    send_in(X,Port,Server,R)->
    ( R\==no
      ->X=R
    ; debugmes(entering_server_in(Port, in(X) )),
      server_interactor(wait_in)=>>interact(Server),
      assumed(got_in(X))
      ->
      % rm_socket_for_in(Server), % let's not close it, we can reuse it
      debugmes(exiting_server_in(Port,Server,in(X) ))
    ; rm_socket_for_in(Server),
      errmes(unexpected_failure_on_port_sever(Port,Server),in(X))
    )
    ; rm_socket_for_in(Server),fail
  ).

% interactor for waiting in/1 server
wait_in(receive_in(X,W),R):-!,
  debugmes(entering-receive_in(X,W)),
  ( check_password(W)->
    R=yes,
    %serve_stop(W), $$
    assumel(got_in(X)) % output X
  ; R=no
  ),
  debugmes(exiting-receive_in(X,W)).
wait_in(X,R):-term_server_interactor(X,R). % mostly for debugging

rm_socket_for_in(Server):-
  close_socket(Server),bb_rm(socket_for,in)->true
; true.

get_socket_for_in(Port,Server):-
  bb_val(socket_for,in,data(Port,Server)),!.
get_socket_for_in(Port,Server):-
  get_free_port(new_server(Port,Server),Port),
  bb_def(socket_for,in,data(Port,Server)),!.

% in/out waiting policy with in/1 becoming server

send_in(X,P,S,R):-
  debugmes(entering-send_in(X,P,S,R)),
  default_this_host(H),
  default_password(W),
  default_host(SH),
  default_port(SP),
  (
    SH==H,SP==P->
    errmes('bad in/1 would create local loop',no_server_at(SH,SP))
  ; ask_server(call_back_in(X,H,P,W),R),
    debugmes(exiting-send_in(X,P,S,R)-password(W))
  ).

db_add_waiting(Db,X0,WaitInfo,X):-
  ( nonvar(X0)->X=X0
  ; X='$any'(X0)
  ),
  termcat(X,WaitInfo,T),
  db_assert(Db,T).

db_remove_waiting(Db,X0,WaitInfo,X,R0):-
    ( X=X0
    ; X='$any'(X0)
    ),
    termcat(X,WaitInfo,T),
    serve_cin(Db,T,R0).

delay_in(Db,X0,H,P,W):-
  db_add_waiting(Db,X0,waiting_in(H,P,W),X),
  debugmes(delay_in(X,addr(H),port(P),password(W))).

serve_delayed_in(Db,X0):-
  debugmes(entering-serve_out-serve_delayed_in(Db,X0)),
  (  db_remove_waiting(Db,X0,waiting_in(H,P,W),X,R0),R0\==no->
     +todo(delayed_in_worker(H,P,X0,W))
     %delayed_in_worker(H,P,X0,W)
   ; X=X0,db_assert(Db,X),
     debugmes(asserting-serve_out-serve_delayed_in(Db,X))
  ),
  !,
  debugmes(exiting-serve_out-serve_delayed_in(Db,X)).

delayed_in_worker(H,P,X0,W):-
  debugmes(entering(delayed_in_worker(H,P,matching=X0,password=W))),
  host(H)=>>port(P)=>>ask_server(receive_in(X0,W),Ok),
  ( Ok==no->debugmes(discared_lost_waiting_in(H,P,X0,W))
  ; debugmes(served(delayed_in_worker(H,P,matching=X0,password=W)))
  ).


% BinProlog specific

xall(X,G,Xs):-ask_server(all(X,G),Xs).

ping:-
   quiet(Q), quiet(10), %$$
     ping(R),
   quiet(Q),
   R\==no.

%ping(R):-has_threads,!,tping(R).
ping(R):-default_password(P),ask_a_server(ping(P),R).

run(G):-run(G,G).

run(X,G):-default_password(P),run(P,X,G).

run(P,X,G):-run(P,X,G,the(X)).

run(P,X,G,R):-assumed(where(W)),run(W,P,X,G,R).

run(here,Pwd,X,G,R):-!,serve_run(Pwd,X,G,R).
% run(there,P,X,G,R):-bremote_run(P,X,G,R).
run(there,Pwd,X,G,R):-
  default_host(H),
  default_port(P),
  remote_run(H,P,X,G,Pwd,R).

% currently only on BinProlog servers

bremote_run(G):-bremote_run(G,G).

bremote_run(X,G):-default_password(P),bremote_run(P,X,G).

bremote_run(P,X,G):-bremote_run(P,X,G,R0),yes2the(R0,R),R=the(X).

bremote_run(P,X,G,R):-
  ( nonvar(P),nonvar(G),ask_server(run(P,X,G),Answer)->true
  ; Answer=no( ask_server_failed(password(P)))
  ),
  check_remote_answer(Answer,G,R).

check_remote_answer(T,_,R):-functor(T,F,_),member(F,[yes,the,no]),!,R=T.
check_remote_answer(Bad,G,_):-errmes(bad_answer(Bad),bremote_run(G)).

remote_mes(From,Mes,Answer):-
  listify(Mes,Chars),
  ask_server(mes(From,Chars),Answer).

say(Mes):-
  default_login(From),
  remote_mes(From,Mes,Answer),
  Answer==yes,
  !.
say(Mes):-
 chat_err(Mes).

chat_err(Mes):-
  listify(Mes,Cs),
  term_chars(T,Cs),
  errmes(lost_message,T).

yell(Mes):-to_all(_)=>>say(Mes).

stop_server(X):-ask_server(stop(X),R),R==yes.

halt_server:-default_password(Pwd),halt_server(Pwd).

halt_server(Pwd):-ask_server(halt(Pwd),R),R==yes.



rsh(Cmd):-rsh(Cmd,Xs),(member(X,Xs),put(X),fail;true).

rsh(Cmd,Chars):-default_password(P),rsh(P,Cmd,Chars).

rsh(P,Cmd,Chars):-bremote_run(P,Chars,pcollect(Cmd,Chars)).

rexec(Cmd):-rexec(Cmd,Xs),(member(X,Xs),put(X),fail;true).

rexec(Cmd0,Cs):-default_password(P),rexec(P,Cmd0,Cs).

rexec(P,Cmd0,Cs):-
   Temp='cmd.tmp',
   make_cmd([Cmd0,'>',Temp],Cmd),
   bremote_run(P,_,system(Cmd)),
   bremote_run(P,Cs,file2chars(Temp,Cs)).

fetch_pred_for(H):- 
   \+ \+ fetch_pred_from_remote_code(H).

fetch_pred_from_remote_code(H):-is_dynamic(H),!.
fetch_pred_from_remote_code(H):-
  call_ifdef(code(_),is_dynamic(code(_))),
  default_code(File),
  quietmes(1,fetch_pred_from_remote_code(H)),
  functor(H,F,N),functor(H0,F,N),
  fetch_file(File,File,H0,user),
  is_asserted(H0).

fetch_file(File):-fetch_file(File,File,_,user).

% loads to LocalDB all clauses matching H from default remote server 
% which will mirror in RemoteDB on the server the RemoteFile
% however if RemoteFile is a unknown it will try to pick them from
% RemoteDB where RemoteFile is expected to be preloaded
fetch_file(RemoteFile,RemoteDB,H,LocalDB):-
   quietmes(1,entering-fetch_file(RemoteFile,RemoteDB,H,LocalDB)),
   ( var(RemoteFile)->true
   ; bremote_run(RemoteDB,db_mirror(RemoteFile,RemoteDB))
   ),
   % fetch_remote_operators,
   start_remote_engine(db_clause(RemoteDB,H,B),(H:-B),E),
   repeat,
     ( ask_remote_engine(E,Clause)->
       db_assert(LocalDB,Clause),fail
     ; true
     ),
   !,
   stop_remote_engine(E),
   quietmes(1,finished(file=RemoteFile,engine=E)).

fetch_remote_operators:-   
   bremote_run(Ops,findall(op(X,Y,Z),current_op(X,Y,Z),Ops)),
   foreach(member(Op,Ops),Op).

db_mirror(File,DB):- 
   ( asserted(mirroring(File,DB))->true
   ; db_clean(DB),
     assert(mirroring(File,DB)),
     consult(File,DB)
   ).

% remote file get/put operations

rget(File):-rget(File,File).

rget(RemoteFile,LocalFile):-rget(256,RemoteFile,LocalFile).

rget(BufSize,RemoteFile,LocalFile):-
   quietmes(1,entering_rget(RemoteFile,LocalFile)),
   bremote_run(RS,fopen(RemoteFile,'rb',RS)),
   fopen(LocalFile,'wb',LS),
   repeat,
     bremote_run(NotRead-Cs,fget_chars(RS,BufSize,NotRead,Cs)),
     %write(got(NotRead,Cs)),nl,
     fput_chars(LS,Cs),
     NotRead>0,
   !,
   bremote_run(fclose(RS)),
   fclose(LS),
   quietmes(1,finished_rget(remote=RemoteFile,local=LocalFile,rs=RS)).

rput(File):-rput(File,File).

rput(LocalFile,RemoteFile):-rput(256,LocalFile,RemoteFile).

rput(BufSize,LocalFile,RemoteFile):-
   quietmes(1,entering_rput(LocalFile,RemoteFile)),
   bremote_run(RS,fopen(RemoteFile,'wb',RS)),
   fopen(LocalFile,'rb',LS),
   repeat,
     fget_chars(LS,BufSize,NotRead,Cs),
     %write(sent(NotRead,Cs)),nl,
     bremote_run(fput_chars(RS,Cs)),
     NotRead>0,
   !,
   bremote_run(fclose(RS)),
   fclose(LS),
   quietmes(1,finished_rput(local=LocalFile,remote=RemoteFile,rs=RS)).

fput_chars(File,Cs):-nonvar(Cs),!,fput_chars0(File,Cs).
fput_chars(File,Cs):-errmes(bad_list,fput_chars(File,Cs)).

fput_chars0(File,Cs):-
  member(C,Cs),
  fputc(File,C),
  fail.
fput_chars0(_,_).

fget_chars(File,BufSize,PlacesNotFilled,Chars):-
   fget_chars0(BufSize,PlacesNotFilled,File,Chars).

fget_chars0(N,N2,F,[C|Cs]):-
  N>0,N1 is N-1,fgetc(F,C),C>=0,!,
  fget_chars0(N1,N2,F,Cs).
fget_chars0(N,N,_,[]).

% direct C-based fast file transfer

fget(File):-fget(File,File).

fget(RemoteFile,LocalFile):-
   quietmes(1,entering_fget(RemoteFile,LocalFile)),
   bremote_run(RF,fopen(RemoteFile,'rb',RF)),
   fopen(LocalFile,'wb',LF),
   term_chars(to_sock(RF),Cmd),
   new_client(Socket),
     sock_write(Socket,Cmd),
     socket(Socket)=>>from_sock(LF,_),
     fclose(LF),
     sock_read(Socket,_),
   close_socket(Socket),
   bremote_run(fclose(RF)),
   quietmes(1,finished_fget(remote=RemoteFile,local=LocalFile)).


fput(File):-fput(File,File).

fput(LocalFile,RemoteFile):-
   quietmes(1,entering_fput(LocalFile,RemoteFile)),
   bremote_run(RF,fopen(RemoteFile,'wb',RF)),
   fopen(LocalFile,'rb',LF),
   term_chars(from_sock(RF),Cmd),
   new_client(Socket),
     sock_write(Socket,Cmd),
     socket(Socket)=>>to_sock(LF,_),
     fclose(LF),
     sock_read(Socket,_),
   close_socket(Socket),
   bremote_run(fclose(RF)),
   quietmes(1,finished_fget(local=LocalFile,remote=RemoteFile)).

to_sock(F,yes):-
   assumed(socket(S)),
   file2sock(F,S),!,
   quietmes(1,to_sock(file=F,socket=S)).
to_sock(F,no):-
   quietmes(1,failing(to_sock(F))).

from_sock(F,yes):-
   assumed(socket(S)),
   sock2file(S,F),!,
   quietmes(1,to_sock(file=F,socket=S)).
from_sock(F,no):-
   quietmes(1,failing(from_sock(F))).

cache_file(F,Suf,CachedF):-
  CacheDir='cache',
  (exists_file(CacheDir)->true
  ; namecat(mkdir,' ',CacheDir,Cmd),
    system(Cmd,X),
    ( X==0->true
    ; errmes(unable_to_do,Cmd)
    )
  ),
  namecat(F,'.',Suf,File),
  ( bremote_run(Size,file_size(File,Size))->
    namecat(CacheDir,'/',File,CachedF),
    ( file_size(File,Size)->true % this is not 100% reliable but is simple
    ; fget(File,CachedF)
    )
  ; errmes(remote_file_not_found,File)
  ).

% remote code operations

rconsult(F):- %$
  cache_file(F,'pl',CF),
  reconsult(CF).

rcompile(F):-
  cache_file(F,'pl',CF),
  compile(CF).

rload(F):-
  cache_file(F,'wam',CF),
  load(CF).


% runs a local server based on a local master trusting its clients

btrust:-run_unrestricted_server.

local_master(ThisH,P):-
  default_master_server(_,P),
  default_this_host(ThisH).

% internet server
run_master_server:-run_master_server(run_default_server).

fork_master_server:-run_master_server(fork_server).

% master server: hub keeping track of various BinProlog servers 
% alive on the net

run_master_server(ServerGoal):-
  local_master(ThisH,P),
  ( run_master(ThisH,P,ServerGoal)->true
  ; errmes(unable_to_run_master_server, on(host=ThisH,port=P))
  ).

run_master(H,P,ServerGoal):-
   this_port(P)=>>master_server(H,P)=>>
     server_interactor(master_server_interactor)=>>
        ServerGoal.

master_server_interactor(refresh(X),yes):-
  serve_refresh_servers(X).
master_server_interactor(halt(N),R):-
  kill_all_servers(N,_),
  term_server_interactor(halt(N),R).
master_server_interactor(add(X),R):-register_on_master(X,R).
master_server_interactor(all(X),Xs):-serve_facts(X,X,Xs).
master_server_interactor(all(X,G),Xs):-serve_facts(X,G,Xs).
master_server_interactor(ping(T),R):-
  term_server_interactor(ping(T),R).

register_on_master(Id,R):-
  Id=server_id([server(_)|Xs],H,P),
  abstime(T),
  ( T=:=0 ->gensym_no(client_no,N)
  ; N is T
  ),
  NewId=server_id([server(N)|Xs],H,P),
  retractall(server_id(_,H,P)),
  serve_out(NewId,_),
  !,
  R=N.
register_on_master(_,no).

kill_all_servers(Pword,X):-
  serve_facts(X,X,Xs),
  member(server_id(_,H,P),Xs),
  ask_a_server(H,P,halt(Pword),_),
  fail.
kill_all_servers(_,_).

serve_refresh_servers(X):-
  serve_facts(X,X,Xs),
  +todo(cleanup_dead_servers(Xs)).

cleanup_dead_servers(Xs):-
   member(X,Xs),
   remove_dead(X),
   fail.
cleanup_dead_servers(_).

remove_dead(X):-debugmes(entering_server_alive(X)).
remove_dead(X):-
   X=server_id(_,H,P),
   host(H)=>>port(P)=>>ping,!,     % just check if alive
   debugmes(server_alive(X)).
remove_dead(X):-
   debugmes(before_remove_dead(X)),
   retract1(X),                       % remove if not
   debugmes(after_remove_dead(X)).

 
register_server(Port):-
  default_this_host(Host),
  quietmes(1,entering_register_server(Host,Port)),
  ( register_server(Host,Port)->true
  ; quietmes(1,
               unable_to_register_server_on_master_server-
               server(Host,Port)
    )
  ).

register_server(Host,Port):-local_master(Host,Port),!.
register_server(Host,Port):-!,
   this_id(Host,Port,Id), % i,i,o
   default_master_server(H,P),
   quietmes(1,
     registering_at(default_master(H,P),this_host(Host),this_port(Port))),
   quiet(Q), quiet(5),
   ( ask_master_server(add(Id),R),R\==no -> set_id(R)
   ; true
   ),quiet(Q) %$$
   ,R\==no.

is_localhost('127.0.0.1'):-!.
is_localhost(localhost).

% master server interface:

ask_master_server(Query,Answer):-
   default_master_server(H,P),
   default_client_action(A),
   directly_ask_a_server(A,H,P,Query,Answer).

all_servers(H,Xs):-
   ask_master_server(all(server(N,H,P),server_id([server(N)|_],H,P)),Xs).

all_servers(Xs):-all_servers(_,Xs).

all_servers:-all_servers(Xs),foreach(member(X,Xs),quietmes(5,X)).

refresh_servers:-refresh_servers(_),!.
refresh_servers:-
  default_this_host(ThisH),
  default_master_server(H,P),
  errmes( unable_to_refresh_servers_from_this_ip(ThisH),
          master_server(H,P)
  ).

refresh_servers(X):-ask_master_server(refresh(X),yes).

% on the client, use as in: password(pw_6708)=>>bremote_run(X,X=1).

% password protected fairly secure RPC server

secure_server:-random_password(Pwd),secure_server(Pwd).

secure_server(Pwd):-password(Pwd)=>>run_unrestricted_server.

% unrestricted unsecure server: use only on trusted Intranet
run_unrestricted_server:-run_unrestricted_server(_).

run_unrestricted_server(Port):-
  server_interactor(unrestricted_server_interactor)=>>
  run_server(Port).

unrestricted_server_interactor(to_sock(F),Ok):-!,to_sock(F,Ok).
unrestricted_server_interactor(from_sock(F),Ok):-!,from_sock(F,Ok).
unrestricted_server_interactor(run(Pwd,G),R):-!,
  serve_run(Pwd,G,G,R).
unrestricted_server_interactor(run(Pwd,X,G),R):-!,
  serve_run(Pwd,X,G,R).
unrestricted_server_interactor(the(X,G,Pwd),R):-!,
  serve_the(Pwd,X,G,R). % for Java based stuff
unrestricted_server_interactor(Q,A):-term_server_interactor(Q,A).


serve_secure_the(Pwd,X,G,R):-
  functor(G,F,N),
  ( member(F/N,[serve_all/2,serve_out/2,serve_cout/2,
      serve_cin/2,serve_rd/2,mes/2])->
    serve_the(Pwd,X,G,R)
  ; errmes(unauthorized_in(serve_secure_the),G)
  ),
  !.
serve_secure_the(_,_,_,no).

serve_the(Pwd,X,G,R):-serve_run(Pwd,X,G,R0),yes2the(R0,R).


% works only as a callback mechanism
% when BinProlog embedded in Jinni as a native method

ask_jinni(Query):-ask_jinni(Query,Answer),Answer=the(Query).

ask_jinni(Pattern,Goal,Answer):-
  ask_jinni(the(Pattern,Goal),R),
  ( R=the(Y),Y=the(X,_)->Answer=the(X)
  ; Answer=no
  ).

ask_jinni(Query,Answer):-
  term_chars(Query,Qs),
  ask_jinni_string(Qs,As),
  term_chars(Answer,As).
    
ask_jinni_string(Qs,As):-
  vget(callback,int(F)),
  call_external(F,Qs,As),
  !.
ask_jinni_string(_,"no").


% for Jinni compatibility

first_solution(X,G,R):-copy_term(the(X,G),the(NewX,NewG)),NewG,!,R=the(NewX).
first_solution(_,_,no).

% the(X,G,R) returning a copy of the(X) if G succeeds or 'no' otherwises
xthe(X,G,R):-assumed(where(there))->remote_the(X,G,R);first_solution(X,G,R).

xthe(X,G):-xthe(X,G,the(X)).
xthe(G):-xthe(G,G).

there:-assumeai(where(there)).
here:-assumeai(where(here)).

remote_the(X,G,R):-
  default_password(P),
  ask_server(run(P,X,G),R0),
  % write(got=R0),nl,
  yes2the(R0,R).

yes2the(no,no).
yes2the(the(X),the(X)).
yes2the(yes(X),the(X)).

serve_run(Password,Answer,Goal,Reply):-
  check_password(Password)->
    ( nonvar(Goal),Goal->Reply=yes(Answer)
    ; Reply=no
    )
  ; 
    Reply=remote_error(bad_password(Password))
  .

% executes queries obeying to sever at default host, port

register_servant(R,ThatId):-
  select_server,
  that_id(ThatId),
  get_id(Id),
  ask_server(add_servant(Id),A),
  A\==no,
  !,
  R=A.
register_servant(_,_):-
  errmes(unable_to_register,servant).

run_servant:-
  vset(err_id,int(78)), % makes this aboort on ^C
  default_server_interactor(Interactor),
  ( register_servant(No,ThatId)->
    show_servants,
    servant_loop(No,ThatId,Interactor),
    Ok=true
  ; Ok=fail
  ),
  vset(err_id,int(0)),
  !,
  Ok.

run_a_servant:-
  default_host(DH),default_port(DP),
(
  run_servant->true
; ( 
    all_servers(Xs),
    member(server_id(_,H,P),Xs),H\==DH,P\==DP,
    host(H)=>>port(P)=>>
     ( ping,
       run_servant
     ),
    !
  ; fail
  )
).

servant_loop(No,ThatId,Interactor):-
  debugmes(entering-servant_loop(No,Interactor)),
  repeat,
    in(query(No,Query),Ok),
    ( Ok==no->!, \+ errmes(servant_failure,query(No,Query))
    ; true
    ),
    functor(Query,F,_),
    ( F=stop,Query==stop(ThatId)->Stop=true %$$
    ; call(Interactor,Query,Reply)->Answer=Reply
    ; Answer=no
    ),
    ( 
      F=mes->true
    ; F=stop->true
    ; out(answer(No,Answer))
    ),
    quietmes(1,servant(Query,Answer)),
    Stop==true,
  !,
  quietmes(5,'SERVER STOPPED'(ThatId)).

serve_add_servant(N,Id):-
   serve_add_servant0(N,Id),
   default_password(W),
   serve_refresh_servants(W).

serve_refresh_servants(_):- % ignore Passwords for now
   default_password(_),
   +todo(remove_dead_servants).

refresh_servants(ThatId):-
   default_password(W),
   that_id(ThatId),
   ask_server(refresh_servants(W),_).

serve_add_servant0(N,Id):-N=:=0,
   get_id(ServerId),
   abstime(T), % uses absolute time for unique identity
   ( T=:=0->gensym_no(servant_no,ServantNo)
   ; ServantNo is T
   ),
   Id=servant(ServerId,ServantNo),
   serve_out(servant_id(Id),_),
   !.
serve_add_servant0(Id,Id).

ask_servant(ID,Query,Answer):-
   out(query(ID,Query)),
   in(answer(ID,R),Ok),
   (Ok==no->Answer=no
   ; Answer=R
   ).

show_servants:-
  QLevel=5,
  refresh_servants(Id),
  quietmes(QLevel,'SERVER'=Id),
  quietmes(QLevel,'CURRENTLY ATTACHED SERVANTS:'),
  (quiet(Q),Q<QLevel->
     all_servants
  ; true
  ),!.
show_servants.

all_servants:-all_servants(Xs),foreach(member(X,Xs),quietmes(5,X)).

all_servants(Xs):-all(X,servant_id(X),R),R=Xs.

% chat listener: tries first to run as a servant then as a server
% servants are happy behind a firewall, servers are usually not:-)
% servants pull Linda commands from a server and execute them
%   as such they exhibit some form of server functionality
% servers listen on a socket and try to register to a master serve
% so that they interact with others over the net

listen:-
  fix_local_host(_),
  (select_master_server->true;true),
  ( ( run_servant
    ; quietmes(2,'trying to work as a server'),
      run_server
    )->true
  ; quietmes(2,'something wrong with network or defaults:'),
    show_defaults
  ).


fix_local_host(H):-
  default_this_host(H0),
  ( is_localhost(H0)->
    write(detected-this_host(H0)),nl,
    write('this local host address is unusable to others'),nl,
    repeat,
    write('enter your real IP address if known, as xx.yy.zz.uu'),nl,
      write('> '),read_chars(Cs),
      ( member(C,Cs), \+ ((is_num(C);[C]=".")) -> fail
      ; name(H,Cs)
      ),
    !,
    set_host(H)
  ; H=H0
  ).

set_that_host(Server):-set_host(Server).
set_that_port(Port):-set_port(Port).

select_master_server:-select_master_server(_,_).

select_master_server(H,P):-
  default_master_server(MH,P),
  default_well_known_master(KH,KP),
  default_this_host(ThisH),default_this_port(ThisP),
  default_host(ThatH),
  quietmes(2,entering_select_master_server_known_to_be_at(KH,KP)),
  ( ThisP\==P->
    ( MH==KH, H=KH
    ; MH\==KH, (H=KH;H=MH)
    ; MH\==ThisH,ThisH==ThatH, H=ThisH
    ; MH\==ThisH,ThisH\==ThatH, (H=ThatH;H=ThisH)
    ),
    quietmes(2,'trying_master_server'(H,P)),
    host(H)=>>port(P)=>>ping,
    ( (H==KH;P==KP)->true
    ; quietmes(2,assuming_master_server_on(H,P)-
                 instead_of(KH,KP)-where_expected_by_other_users
               )
    ),
    % set_master_server(H,P), % persistent
    assumei(master_server(H,P)), % backtrackable
    quietmes(2,found_master_server(host=H,port=P))
   ;ThisP==P,
    default_server_interactor(F),
    quietmes(2,'This seems to be a master server'(interactor=F))
  ),
  !.
select_master_server(_,_):-
  errmes(warning,unable_to_detect_master_server).

select_server:-select_server(_,_).

select_server(H,P):-
  default_this_host(ThisH),default_this_port(ThisP),
  default_host(ThatH),default_port(ThatP),
  default_master_server(MH,_),
  default_well_known_host(KH),default_well_known_port(KP),
  ( ThisH==ThatH,ThisP==ThatP, H=ThisH,P=ThisP
  ; (ThisH\==ThatH;ThisP\==ThatP), (H=ThatH,P=ThatP;H=ThisH,P=ThisP)
  ; (MH\==ThisH,MH\==ThatH),  H=MH,P=KP
  ; KH\==MH,KH\==ThisH,KH\==ThatH,ThisP\==KP,ThatP\=KP,  H=KH,P=KP
  ),
  quietmes(2,'trying_server'(H,P)),
  host(H)=>>port(P)=>>ping,
  assume_server(H,P),
  !,
  quietmes(2,found_and_set_server(host=H,port=P)).
select_server(_,_):-
  errmes(warning,unable_to_detect_server).

%set_server(H,P):-set_this_host(H),set_this_port(P).
assume_server(H,P):- 
     assumeai(this_host(H)),
     assumeai(this_port(P)). % backtrackable

show_servers:-
  refresh_servers, 
  quietmes(5,'TARGET SERVERS ALIVE ON THE NET:'),
  (quiet(Q),Q<5->all_servers;true).

% chat client - broadcasts chat input to registered servers

chat:-chat(_).

chat(Target):-  
  (
    select_master_server-> (
      run_chat(Target)->true
      ; select_server->talk
      ; fail
    )
  ; show_defaults,
    \+ errmes(something_wrong_with_defaults,failed_chat(Target)),
    quietmes(2,'trying local server based talk'),
    talk
  ).

run_chat(Target):- 
  show_servers,
  show_servants,
  quietmes(5,'multi-server chat, ^D on Unix or ^Z on PCs to end'),
  repeat,    
    ( write('>'),read_chars(Cs)->to_all(Target)=>>say(Cs),fail
    ; true
    ),
  !.

% talks to a given server and its servants

talk:-  
  show_servants,
  quietmes(5,'^D on Unix or ^Z on PCs to end'),
  repeat,    
    ( write('>'),read_chars(Cs)->say(Cs),fail
    ; true
    ),
  !.

% requires unrestricted server on target

rshell:-
  quietmes(5,'^D on Unix or ^Z on PCs to end'),
  default_host(H),default_port(P),
  repeat,    
    ( write(H),write((:)),write(P),write('>'),
       read_chars(Cs),term_chars(Cmd,Cs)->rexec(Cmd),fail
    ; true
    ),
  !.

% NEW PACKAGE

move:-
  % assumes wrap_thread(...) has been executed before
  call_with_cont(run_and_return).

return:-
  call_with_cont(collect_left_over),
  true. % should be here: simplifies cont. cutting algo

run_and_return(Gs):-
  %println(Gs),
  the(
    from_to(Gs,Cont),
    wrap_thread(Gs,Cont), % this send left over
    Result
  ),
  %println(Result),
  eq(the(from_to(Gs,NewGs)),Result),
  ( var(NewGs)->true
  ; NewGs
  ).

collect_left_over(Gs):- assumed(left_over_cont(Gs)).
  

% OLD PACKAGE

% picks up wrapped continuation,
% jumps to default remote site and runs it there
move_thread:-
  call_with_cont(move_with_cont).
  
% moves to remote site goals Gs in current continuation
move_with_cont(Gs):-
  % gets info about this host
  default_this_host(BackHost),
  get_free_port(BackPort),
  default_password(BackPasswd),
  default_code(BackCode),
  % runs delayed remote command
  bremote_run(
     +todo(
       host(BackHost)=>>port(BackPort)=>>code(BackCode)=>>(
         sleep(10), % waits until server on BackPort is up
         % runs foals Gs picked up from current continuation 
         (Gs->true;true), % ignores failure
         % stopps server back on site of origin
         stop_server(BackPasswd)
       )
     )
  ),
  % becomes data and code server for mobile code until is
  % stopped by mobile code posessing password
  this_port(BackPort)=>>run_unrestricted_server.

% BACKTRACKABLE GLOBAL VARS

lval(A,X):-current_engine(E),lval(E,A,X).

% GLOBAL LOGICAL VARIABLES

':=:'(K1#K2,X):-nonvar(K1),nonvar(K2),!,lval(K1,K2,X).
':=:'(K,X):-nonvar(K),!,lval(K,X).
':=:'(K,X):-errmes('unbound keys not allowed in :=:/2',':=:'(K,X)).
% Linear and intuitionistic implication and assumption

% intuitionistic implication - stack (asserta) ordering
C=>>G :- 
  assume(push(intuitionistic(Scope)),C),  % reusable
  metacall(G),
  Scope='$closed'.

% intuitionistic implication - assertz ordering
C=>G :- 
  assume(intuitionistic(Scope),C),  % reusable
  metacall(G),
  Scope='$closed'.

% linear implication - stack (asserta) ordering
(C-::G) :- 
  assume(push(linear(Scope)),C), % usable_once
  metacall(G),
  Scope='$closed'. % weakening: unused is OK

% linear implication - assertz ordering
(C-:G) :- 
  assume(linear(Scope),C), % usable_once
  metacall(G),
  Scope='$closed'. % weakening: unused is OK

% linear implication - asserta ordering, quite close to "-o" in Lolli
(C=::G) :-
  assume(push(linear(Scope)),C), % usable_once
  metacall(G),
  Scope=='$closed'. % no weakening: should be consumed!

% linear implication - assertz ordering
(C=:G) :- 
  assume(linear(Scope),C), % usable_once
  metacall(G),
  Scope=='$closed'. % no weakening: should be consumed!


% semantics: Clause implies the current continuation, i.e.
%            it will be true until the end (success or failure)
%            of the current resolution branch.

% Depending on the type of implication get 2 versions of assume:
%   intuitionistic: ~~ 
%        assume_ll(intuitionistic(_),Clause,Cont)::-Clause=>Cont.
%   linear: ~~
%        assume_ll(linear(_),Clause,Cont)::- Clause-:Cont.

% defining

+(X):-assumel(X),add_assumed(X).
*(X):-assumei(X),add_assumed(X).
-(X):-check_assumed(X),assumed(X).

add_assumed(C):-
  add_true(C,(H:-_)),
  functor(H,F,N),
  ( assumed(has_assumptions(F,N))->true
  ; assumei(has_assumptions(F,N))
  ).

check_assumed(X):-nonvar(X),!.
check_assumed(X):-assumed(has_assumptions(F,N)),functor(X,F,N).

assumel(X):-assume(linear(_),X).
assumei(X):-assume(intuitionistic(_),X).

assumeal(X):-assume(push(linear(_)),X).
assumeai(X):-assume(push(intuitionistic(_)),X).

assume(_,C):-var(C),!,errmes(error_in_assume,bad_clause(C)).
assume(Hint,C):-assume1(Hint,C).

% stack ordering makes no sense for files
assume1(Hint,[F]):-!,assume_file(Hint,F).
assume1(Hint,C):-assume_ll(Hint,C).

assume_ll(Hint,C):-
  add_true_ll(C,H,NewC),
  lval(H,PXsYs),
  assume_with_hint_ll(Hint,PXsYs,NewC).

assume_with_hint_ll(push(Hint),PXsYs,NewC):-!,
  assume_with_hint_ll0(PXsYs,cls(Hint,NewC)).
assume_with_hint_ll(Hint,PXsYs,NewC):-
  assume_with_hint_ll1(PXsYs,cls(Hint,NewC)).

assume_with_hint_ll0(PXsYs,NewC):-
  nonvar(PXsYs),PXsYs=pred(Xs-Ys),!,
  % $$$ might trigger GC bug
  setarg(1,PXsYs,[NewC|Xs]-Ys).
assume_with_hint_ll0(pred([NewC|Cs]-Cs),NewC).

assume_with_hint_ll1(PXsYs,NewC):-
  nonvar(PXsYs),PXsYs=pred(Xs-[NewC|Ys]),!,
  setarg(1,PXsYs,Xs-Ys).
assume_with_hint_ll1(pred([NewC|Cs]-Cs),NewC).

add_true_ll((H:-B),H1,C):-!,H1=H,C=(H:-B).
add_true_ll(H, H,(H:-true)).


% using

assumed(H):-assumed_clause(H,B),call_body(B).

assumed_clause(H,B):-assumed_clause(H,B,_).

assumed_clause(H,B,Hint):-
  assumed_prop(H,pred(Cs-_)),
  get_assumed_clause(H,Cs,B+Hint).

get_assumed_clause(G,[cls(Hint,C)|_],NewG+Hint):-
  copy_or_delete_ll(Hint,C,G,NewG).
get_assumed_clause(G,[_|Ys],NewGHint):-
  nonvar(Ys),
  get_assumed_clause(G,Ys,NewGHint).

copy_or_delete_ll(linear(Scope),C,G,NewG):-
  var(Scope),Scope='$closed',C=(G:-NewG).
copy_or_delete_ll(intuitionistic(Scope),C,G,NewG):-
  var(Scope),
  copy_term(C,(G:-NewG)).

% linear and intuitionistic file operations
assume_file(Hint,F):-
  find_file(F,InFile),
  seeing(F0),see(InFile),
  assume_all(Hint),
  seen,see(F0).

assume_all(Hint):-
  gc_read_clause(C),
  assume_all0(C,Hint).

assume_all0(end_of_file,_):-!.
assume_all0(C,Hint):-
  assume_one(C,Hint),
  gc_read_clause(NewC),
  assume_all0(NewC,Hint).

assume_one(':-'([F]),Hint):-!,assume_file(Hint,F).
assume_one(':-'(G),_):-!,(consult_cmd(G)->true;true).
assume_one(C,Hint):-assume_ll(Hint,C).

% gets info without touching
% although in a predicate different types of clauses can be assumed
% this will only return the type of the first clause


is_assumed(H):-is_assumed(H,_).

is_assumed(H,Info):-call_ifdef(assumed_info(H,Info),fail).

get_assumed_prop(Goal,Prop):-call_ifdef(assumed_prop(Goal,Prop),fail).

was_assumed(H):-assumed_prop(H,X),nonvar(X).

assumed_prop(H,PCs):-val(H,X),nonvar(X),PCs=X.

assumed_info(H,Info):-
   Flag='$not_closed',
   member(Hint,[intuitionistic(Flag),linear(Flag)]),
   assumed_info0(H,Hint),
   functor(Hint,Info,_).

assumed_info0(H,Hint):-
    assumed_prop(H,pred(Cs-_)),
    member_scan(cls(Hint,(H:-_)),Cs,_).

% Linear implication (-:: in `asserta' order) based
% exception handling constructs (see LOPSTR'94 paper by Tarau & Dahl 
% for a backtrackable version of catch/throw)

% end_cont(_) is used to mark mobile threads $$$ - bug in timed_call !!!


% topcall(G):-mobile_call(G).
% topcall(G):-wrap_thread(metacall(G)).

mobile_call(G):-wrap_thread(catch(metacall(G),E,handle_uncaught(E))).

end_cont(C):-debugmes(unexpected(end_cont(C))).

handle_uncaught('$commit'):-!,topcall(true).
handle_uncaught(E):-errmes(uncaught_exception,E).


commit :-
  assumed(catchmarker('$commit',Do,Choice,_)),
  cut_to(Choice), Do.

/*
catch(Goal,Ball,Do,Cont) ::- catch0(Goal,Ball,Do,Cont,Cont) .
catch(_,_,_) :- fail .

catch0(Goal,Ball,Do,Cont) :- get_neck_cut(Choice) ,
		(catchmarker(Ball,Do,Choice,Cont) -:: Goal) .

throw(Term) :-	copy_term(Term,Copied) ,
		assumed(catchmarker(Ball1,Do1,Choice,Cont1)),!,
		Ball1 = Ball ,
		Do1 = Do,
		Cont1 = Cont ,
		untrail_to(Choice) ,
		(Ball = Copied -> Do,call_cont(Cont) ; throw(Term)) .

% CONTINUATIONS ARE FIRST ORDER OBJECTS: some tools based on this

% calls Goal with current continuation available to its inner calls
capture_cont_for(Goal):-
  assumeal(cont_marker(End)),
    Goal,
  end_cont(End).

% passes Closure to be called on accumulated continuation
call_with_cont(Closure):-
  assumed(cont_marker(End)),
  consume_cont(Closure,End).
  
% gathers in conjunction goals from the current continuation
% until Marker is reached when it calls Closure ont it
consume_cont(Closure,Marker):-
  get_cont(Cont),
  consume_cont1(Marker,(_,_,_,Cs),Cont,NewCont), % first _
  call(Closure,Cs),                              % second _
  % sets current continuation to leftover NewCont    
  call_cont(NewCont).                            % third _

% gathers goals in Gs until Marker is hit in continuation Cont
% when leftover LastCont continuation (stripped of Gs) is returned
consume_cont1(Marker,Gs,Cont,LastCont):-
   strip_cont(Cont,Goal,NextCont),
   ( NextCont==true-> !,errmes(in_consume_cont,expected_marker(Marker))
   ; arg(1,NextCont,X),Marker==X->
     Gs=Goal,arg(2,NextCont,LastCont)
   ; Gs=(Goal,OtherGs),
     consume_cont1(Marker,OtherGs,NextCont,LastCont)
   ).

% this `binarized clause' gets the current continuation
get_cont(Cont,Cont)::-true(Cont).

% setes calls NewCont as continuation to be called next
call_cont(NewCont,_) ::- true(NewCont).

% sets NewCont as continuation to be called next 
% instead of OldCont which is returned in arg 2
swap_cont(NewCont,OldCont,OldCont) ::- true(NewCont).

*/

% A special instance of Delphi Lemmas:
% is ON THE USER to ensure that ProbAndArgs = Prob / Arglist
% contains a Prob in 0..100 (memoing probability) and a list of
% argument position which will be ground when F/N is called


delphi(F/N):-!,delphi0(F/N-10).
delphi(FND):-delphi0(FND).

delphi0(F/N-D):-
  ( vget0(bbhi,int(0))->nogc,nobbgc % disables garbage collection
  ; !,errmes(high_blackboard,memoing_disabled)
  ),
  functor(P,F,N),
  ( integer(D)->ProbAndArgs=D
    ; D=Prob/_,integer(Prob)->ProbAndArgs=D
  ),!,
  bb_let(P,'$delphi',ProbAndArgs)
; errmes('bad memo declaration',F/N-D).

% moved to lib.pl
%is_delphi(P,DP):-val(P,'$delphi',DP).

make_delphi_call(G,100/Args,memo_call(G,Args)):-!.
make_delphi_call(G,Prob/Args,delphi_call(G,Prob,Args)):-!.
make_delphi_call(G,100,memo_call_1(G,X)):-!,arg(1,G,X).
make_delphi_call(G,Prob,delphi_call_1(G,Prob,X)):-arg(1,G,X).

memo(F/N):-delphi0(F/N-100).
memo(F/N-Args):-delphi0(F/N-100/Args).

delphi_call(P,_,Args):-delphi_key(Args,P,K),membq(K,P,P),!.
delphi_call(P,Delphi,Args):-
  random(R),Luck is R mod 100,Delphi>Luck,!,
  P,!,
  delphi_key(Args,P,K),addq(K,P,P).
delphi_call(P,_,_):-P,!.

delphi_call_1(P,_,X):-membq(X,P,P),!.
delphi_call_1(P,Delphi,X):-
  random(R),Luck is R mod 100,Delphi>Luck,!,
  P,!,
  addq(X,P,P).
delphi_call_1(P,_,_):-P,!.

term_hash(Term,IndexArgList,Key):-
   hkey(Term,K),
   delphi_key1(IndexArgList,Term,K,Key).

delphi_key(Args,P,K):-delphi_key1(Args,P,0,K).

delphi_key1([],_,R,R).
delphi_key1([I|Is],P,R1,R3):-
  arg(I,P,X),
  hkey(X,K),
  R2 is (R1<<4) + K,
  delphi_key1(Is,P,R2,R3). 

memo_call(P,Args):-delphi_key(Args,P,K),membq(K,P,P),!.
memo_call(P,Args):-P,!,delphi_key(Args,P,K),addq(K,P,P).

memo_call_1(P,X):-membq(X,P,P),!.
memo_call_1(P,X):-P,!,addq(X,P,P).

% code from headers.pl - these are not builtins anymore !!!

/*
p0(words_from,2,'reads from a constant, a list of words'
  -[x('I bought 2 (hot) dogs for 1.50$',_)]).
p0(chars_to_words,2,'reads from list of chars, a list of words'
  -[x("It was 35.50$ for two",_)]).
p0(chars_to_words,3,'reads from list of chars, a list of words and a variable list'
  -[x("f(X)+g(X,Y)+h(Y)",_,_)]).
p0(read_words,1,'reads words from stdin into a list').

p0(start_remote_engine,3,'starts remote engine, args: Answer,Goal,Engine').
p0(create_remote_engine,1,'creates remote engine, returns its handle').
p0(load_remote_engine,3,'loades remote engine, args: Engine,Goal,Answer').

p0(ask_remote_engine,2,'askes remote engine, args: Engine,Answer').
p0(stop_remote_engine,1,'stops and frees resources of remote engine').

p0(db_mirror,2,'mirrors a Prolog file (arg 1) as a database of clauses (arg 2) - useful if serving multiple code clients').

p0(rget,1,'downloads remote (binary) file from Linda server - see fget/1 for faster operation').
p0(rget,2,'copies remote (binary) file arg1 to local file arg 2 - see fget/2 for faster operation').

p0(rput,1,'uploads (binary) file to Linda server - use equivalent fput/1 which is much faster').
p0(rput,2,'copies local (binary) file arg1 to remote file arg 2 - use equivalent fput/2 which is much faster').

p0(fput_chars,2,'puts to CStream list of chars in arg 2').
p0(fget_chars,4,'gets from CStream, BufSize, PlacesNotFilled, Chars').

p0(fget,1,'downloads remote (binary) file from Linda server').
p0(fget,2,'copies remote (binary) file arg1 to local file arg 2').

p0(fput,1,'uploads (binary) file to Linda server (fast)').
p0(fput,2,'copies local (binary) file arg1 to remote file arg 2 (fast)').

p0(rconsult,1,'downloads to cache/file.pl and reconsults remote file provided by BinProlog server').
p0(rcompile,1,'downloads to cache/file.pl and compiles remote file provided by BinProlog server').
p0(rload,1,'downloads to cache/file.bp and loads bytecode file provided by remote BinProlog server').

p0(move_thread,0,
  'deprecated - see move/0').
p0(move_with_cont,1,
  'deprecated - see move/0').

p0(wrap_thread,1,
  'wraps Goal with current continuation for mobile threads').
p0(wrap_thread,2,
  'wraps continuation and returns LeftOver when execution hits return/0').
p0(end_cont,1,
  'marks the dynamic end of the mobile continuation').

p0(move,0,
  'first key Mobile Code operation: picks up wrapped continuation and moves to remote site is there/0 has been set, or runs it locally if here/0 has been set').
p0(return,0,
  'second key Mobile Code operation: forces the moved continuation to come back and run left over goals').
p0(run_and_return,1,
  'part of Mobile Code implementation: runs a goal on moved thread, returns and executes the left over continuation').
p0(collect_left_over,1,
  'part of Mobile code implamenetation: picks up left over continuation').

p0(ask_jinni,1,'(Query): when BinProlog works as Jinni Accelerator, calls back Jinni from BinProlog and applies returned bindings').
p0(ask_jinni,3,
 '(Pattern,Goal,Answer): calls back Jinni from BinProlog, with Answer an instance of the(Pattern) on success, no on failure').
p0(ask_jinni,2,'(QueryTerm,AnswerTerm): calls back Jinni from embedded BinProlog acclerator with the(...) returned on success, no on failure').
p0(ask_jinni_string,2,'calls back Jinni from embedded BinProlog accelearator with Query and Answer lists of chars').

% sandbox security
p0(sandbox,0,'disables unsecure system calls').
p0(sandbox,1,'disables a list of unsecure system calls given as functor/arity').
p0(in_sandbox,1,'true if arg 1 has been target of overriding as a sandboxed predicate').

p0(extract_token,2,'gets a plain word from token term').

p0(fetch_pred_for,1,'fetches remote code for arg 1, without calling it').
p0(fetch_file,1,'fetches code from remote file (arg 1) on server').
p0(fetch_file,4,'fetches remote code, args:RemoteF,RemoteDB,Head,LocalDB').
p0(fetch_remote_operators,0,'fetches operators in use on remote server').

p0(run,1,'runs Goal here or there on server').
p0(run,2,'runs arg 2 on here or on server and unifies arg 1 with result').
p0(run,3,'runs with (Password,Answer,Goal) here or on server').
p0(run,4,'runs with (Password,Answer,Goal,Reply) here or remote server').

p0(remote_mes,3,'sends From, Mes and returns yes if succesfully displayed on server').
p0(say,1,'sends Mes and succeeds if displayed on server').
p0(yell,1,'broadcasts Mes to all servers registered on the default master server').

p0(proxy,4,'args: Host,Port,Query,Answer, asks proxy to forward Query to Host').

p0(chat,0,'sends what you type in to all BinProlog users registered at default master server').
p0(chat,1,'sends what you type in to selected BinProlog users registered at default master server').
p0(run_chat,1,'runs a chat after proper initialisation').

p0(talk,0,'sends what you type in, to another BinProlog user').
p0(listen,0,'does its best to listen to messages of chat users - it tries out possible servers and master servers to find them').

p0(ask_servant,3,'sends query and waits until servant (Linda client pulling from a server) answers').
p0(run_servant,0,'executes queries pulled from sever at default host, port').
p0(run_a_servant,0,'executes queries pulled from first detected server').

p0(refresh_servants,1,'removes inactive servants (Linda clients pulling commands from default server)').
p0(remove_dead_servants,0,'removes inactive servants').
p0(serve_refresh_servants,1,'if arg 1 password is ok, removes inactive servants').
p0(all_servants,0,'lists all servants on current server').
p0(all_servants,1,'returns all servants on current server').

p0(rsh,3,'rsh(Password,Command,ResultChars) runs a remote shell command').
p0(rsh,2,'runs arg 1 as a remote shell command and collects the result').
p0(rsh,1,'runs arg 1 as a remote shell command and prints result').
p0(rexec,3,'rexec(Password,Command,ResultChars) runs a remote PC command').
p0(rexec,2,'runs arg 1 as a remote PC command and collects the result').
p0(rexec,1,'runs arg 1 as a remote PC command and prints the result').
p0(rshell,0,'runs commands on remote PC and prints the result').
p0(remote_top,0,'starts a shell executing pure queries on a remote BinProlog server').

p0(run_server,2,'Port,Password: runs password protected server - good for remote_run services for Jinni clients').
p0(run_server,5,'Port,Password,Heap,Stack,Trail: runs server using engines of given size - good for services for Jinni clients').

p0(handle_service,2,'handles a Jinni service S with password P - always succedes').
p0(answer_one_query,2,'handles a Jinni service S with password P').

p0(run_default_server,0,'runs foreground Linda server on localhost, port 9001 by default').

p0(fork_server,0,'runs background Linda server on localhost, port 9001 by default').
p0(fork_server,1,'forks Linda server and returns its Pid').
%p0(fork_server,2,'forks Linda server on Port and returns its Pid').

p0(stop_server,0,'stops server and forces it back to intractive mode').
p0(stop_server,1,'stops server using given password').

p0(halt_server,0,'halts Linda server').
p0(halt_server,1,'halts Linda server using given password').

p0(ask_server,2,'sends a query and gets an answer from server(s)').
p0(ask_a_server,2,'sends a query and gets an answer from a Linda server').
p0(ask_a_server,5,'args: Interactor,Host,Port,Question,Answer').
p0(ask_all_servers,2,'broadcast query to all server given by master').

p0(server_try,2,'args: Socket, Goal').
p0(client_try,2,'args: Socket, Goal').

p0(server_loop,2,'answers client queries on server').

p0(term_server_action,1,'opens service socket and answers query').
p0(term_server_interactor,2,'filters secure commands known to the server').
p0(term_server_step,1,'answers query on service socket').
p0(term_client_action,4,'opens client socket and sends query').
p0(term_client_step,3,'sends query on client socket').

p0(interact,1,'handles interaction step on a server').
p0(run_service_action,7,'runs fg or bg service when requested on a socket').

p0(run_line_server,0,'runs a server reading and answering CR/LF ended lines').
p0(line_server_action,1,'opens service socket and answers query line').
p0(line_server_step,1,'answers line query on service socket').
p0(line_client_action,4,'opens client socket and sends query').
p0(line_client_step,3,'sends line query on client socket').

p0(ask_line_server,2,'exchanges a list of chars with a line server').

p0(serve_cin,2,'executes serve_cin/3 on current_db').
p0(serve_cin,3,'server side worker for conditional non-suspending cin/1').
p0(serve_out,2,'executes serve_out/3 on current_db').
p0(serve_out,3,'server side worker for out/1').
p0(serve_cout,2,'executes serve_out/3 on current_db').
p0(serve_rd,2,'server side worker for rd/1').
p0(serve_all,2,'server side worker for all/2').
p0(serve_all,3,'server side worker for all/3').

p0(wait_in,2,'specialized interactor for in/1').

p0(delayed_in_worker,4,'').
p0(db_add_waiting,4,'').
p0(db_remove_waiting,5,'').


p0(unrestricted_server_interactor,2,
  'allows unrestricted commands run1/ run/2 on the server').


p0(secure_server,0,'runs secure_server/1 with random password').

p0(secure_server,1,'runs password protected Linda and remote_run server').

p0(run_unrestricted_server,1,'runs Linda and unsecure remote_run server on given port').

p0(trust,0,'the same as run_unrestricted_server').

p0(run_unrestricted_server,0,'runs Linda and remote_run server on a free port').

p0(master_server_interactor,2,'filters commands available on master server').

p0(ask_master_server,2,'asks a question the master server').

p0(new_service,2,
  'creates new service socket on server socket and returns it').

p0(get_free_port,1,'obtains a random free port').

p0(free_port,1,'asserts that arg 1 is a free port').

p0(new_server_on_free_port,2,
  'runs new server on first random free port').

p0(new_client,1, 'creates new client socket and returns it').

p0(from_sock,2,'copies from a socket to a file (fast, all in C)').
p0(to_sock,2,'copies from a file to a socket (fast, all in C)').

p0(that_id,1,'gets identity info from current remote server').
p0(this_id,1,'gets identity info of this server').

p0(in0,1,'polls the Linda server until able to remove a matching term from its blackboard').
p0(launch_in,3,'').

% overridable defaults


p0(default_code,1,'returns default remote code file on Linda server').
p0(set_code,1,'asserts default file on server - for remote code fetching').
p0(code,1,'assumes default remote code file on Linda server').

p0(default_server_action,1,'returns default server-side action').
p0(set_server_action,1,'asserts default server-side action').
p0(server_action,1,'assumes default server-side action').

p0(default_server_interactor,1,'returns default server-side answerer').
p0(set_server_interactor,1,'asserts default server-side interactor').
p0(server_interactor,1,'assumes default server-side intractor').

p0(default_client_action,1,'returns default client-side action').
p0(set_client_action,1,'asserts default client-side action').
p0(client_action,1,'assumes default client-side action').

p0(set_id,1,'sets identification number after receiving it from a server').
p0(get_id,1,'gets identification number previously obtained from server').

p0(default_master_server,2,'returns master server (Host and Port)').
p0(set_master_server,2,'asserts default master server (Host and Port)').
p0(master_server,2,'assumes default master server (Host and Port)').

p0(default_well_known_host,1,'returns IP address of a well known machine useful to connect BinProlog users worldwide').

p0(default_well_known_port,1,'returns well known port for this version of BinProlog').

p0(default_well_known_master,2,'returns IP address of default machine hosting the master server').

p0(default_proxy_server,2,'returns proxy server (Host and Port)').
p0(set_proxy_server,2,'asserts default proxy server (Host and Port)').
p0(proxy_server,2,'assumes default proxy server (Host and Port)').


p0(random_password,1,'if arg 1 var creates and returns random password for user').
p0(random_password,0,'creates new random password for user').

% end defaults

p0(run_master_server,0,'runs master server keeping track of servers').
p0(run_master_server,1,'runs kind of master server given as arg 1').
p0(fork_master_server,0,'forks master server in separate process').

p0(register_server,0,'registers this on master server').
p0(register_server,1,'registers this on master server and returns answer').

p0(all_servers,0,'prints the list of servers registered on master server').
p0(all_servers,1,'returns list of servers registered on master server').
p0(all_servers,2,'returns servers registered on master matching arg 1').

p0(refresh_servers,0,'asks master server to update its list of servers').
p0(refresh_servers,1,'asks master server to update its list of servers matching arg 1').
p0(cleanup_dead_servers,1,'on master: removes servers who do not answer').

p0(ping,1,'checks if server is alive and returns its current CPU time').
p0(ping,0,'checks if server is alive').

% Jinni related

p0(remote_the,3,'remote_the(X,G,R) runs G on remote BinProlog or Jinni server and return R based on X').
p0(first_solution,3,'first_solution(X,G,R) returns copy of first answer R=the(X) or R=no').
p0(the,3,'the(X,G,R) returns locally or remotely computed first answer R=the(X) or R=no').
p0(the,2,'defined as the(X,G):-the(X,G,the(X))').
p0(the,1,'defined as the(G):-the(G,G)').

p0(there,0,'focuses data operations on server').
p0(here,0,'focuses data operations to local process').
p0(where,1,'retrieves here/there focus').

%p0(run_server,0,'runs foreground Linda server on localhost, on a free port').
%p0(run_server,1,'runs foreground Linda server on given or on free Port').

p0(dcg_phrase,3,'(DcgStream,Axiom,Phrase): emulates DCG phrase/3 for given hidden DCGStream'-dcg_phrase(7,(#a,#b,#c),_)).
p0(dcg_phrase,2,'(Axiom,Phrase): emulates DCG phrase/2 for default hidden DCG stream'-dcg_phrase((#a,#b,#c),_)).
p0(dcg_apply,1,'applies a method like +(1) to current hiden DCG stream').

p0(( :=: ),2,'sets a backtrackable global variable to a value'-x(a,3)). % for lval

p0((#),1,'short hand for dcg_connect/1, advances current hidden DCG stream through consumption or generation of this terminal'). % for i.v. dcgs
p0((#<),1,'short hand for dcg_def/1: sets current DCG stream to this value'). % for i.v. dcgs
p0((#>),1,'short hand for dcg_val/1: unifies this with value of current DCG stream'). % for i.v. dcgs
p0((##<),1,'short hand for dcg_tell/1: sets current DCG stream to int arg N'). % for i.v. dcgs
p0((##>),1,'short hand for dcg_telling/1: unifies this with int N pointing to current DCG stream'). % for i.v. dcgs

p0((ag_connect),1,'advances current hidden DCG stream through consumption or generation of this terminal'). % for i.v. dcgs
p0((ag_def),1,'sets current DCG stream to this value'). % for i.v. dcgs
p0((ag_val),1,'unifies this with value of current DCG stream'). % for i.v. dcgs
p0((ag_tell),1,'sets current DCG stream to int arg N'). % for i.v. dcgs
p0((ag_telling),1,'unifies this with int N pointing to current DCG stream'). % for i.v. dcgs

p0(match_word,1,
  'matches a word, using dcg_connect and AGs').
p0(match_before,3,
  'match_before(+Stops,-Word,-Stop): matches a word with AGs until a given delimiter is hit, which is also returned').
p0(to_full_stop,1,
  'matches a word, using dcg_connect and AGs').

p0((delphi),1,'delphi(f/3-10) declares a Delphi predicate f/3 with memoing probability 10/100').
p0(is_delphi,2,
  'recognizes a Delphi predicate, subject be memoed and returns the probability for this to happen').
p0(delphi_call,3,'').
p0(delphi_call_1,3,'').
p0(memo_call,2,'').
p0(memo_call_1,2,'').
p0((memo),1,'declares a predicate Name/Arity to be memoized').

p0(term_hash,3,
  'computes hash code based on main functor and functors of listed argument positions; fails if something is unbound'-
  [term_hash(t(a,b),[1,2],_),
  term_hash(t(a,c),[1,2],_)]
).

p0(assumei,1,'backtrackable: assertz style intuitionistic assumption').
p0(assumel,1,'backtrackable: assertz style linear assumption').
p0(assumeai,1,'backtrackable: asserta style intuitionistic assumption').
p0(assumeal,1,'backtrackable: asserta style linear assumption').
p0(assumed,1,'calls and possibly consumes assumed clause with matching head').
p0(assumed_clause,2,
  'assumed_clause(H,B) retrieves assumed clause with matching head H and body B').
p0(assumed_clause,3,'').

p0((*),1,'backtrackable: assertz style intuitionistic assumption').
p0((+),1,'backtrackable: assertz style linear assumption').
p0((-),1,'calls and possibly consumes assumed clause with matching head').

p0((=>>),2,'intuitionistic implication - stack (asserta) ordering').
p0((=>),2,'intuitionistic implication - assertz ordering').
p0((-::),2,'affine linear implication - asserta ordering').
p0((-:),2,'affine linear implication - assertz ordering').
p0((=::),2,'linear implication - asserta ordering').
p0((=:),2,'linear implication - assertz ordering').

p0(is_assumed,1,'checks if currently assumed').
p0(was_assumed,1,'checks if it was assumed  in current AND-branch').
p0(is_assumed,2,'checks if linear or intuitionistic').

p0(assume_from_chars,1,'assumes all clauses read from a list of chars in assertz order').

p0(dcg_words,1,
  'acts on AG stream: scans list of character codes and builds a list of words').

p0(dcg_word,1,
  'acts on AG stream: scans list of character codes and builds a word').

p0(dcg_star,2,
  'dcg_star(Recognizer,Result): AG based star regexp processor for (Recognizer)*').

p0(dcg_plus,2,
  'dcg_plus(Recognizer,Result): AG based plus regexp processor for (Recognizer)+').

p0(dcg_one,2,
  'dcg_one(Recognizer,Result): AG based regexp processor for exactly one Recognizer').

p0(dcg_is_letter,1,
  'AG based one alphanumeric letter recognizer').

p0(dcg_is_space,1,
  'AG based one space recognizer').

p0(dcg_space,0,
  'AG based 0 or more space recognizer').

p0(dcg_is_punct,1,
  'AG based non-alphanumeric punctuation recognizer').



p0(read_tokens_from_chars,3,
  'reads tokens from a list of char codes').

p0(do_s2tokens,2,'').
p0(do_s2t,2,'').

p0(gg_def,1,'').
p0(gg_val,1,'').
p0(gg_get_code,1,'').
p0(gg_put_code,1,'').
p0(gg_cwrite,1,'').
p0(gg_write_name,1,'').
p0(gg_nl,0,'').

p0(make_appl,1,'').
p0(make_executable_unix_appl,3,'').

p0(memoq,3,'').

p0(remake1,0,'').

p0(mobile_call,1,'calls goal, gets ready to handle uncaught exception and wraps goal for thread mobility').

p0(capture_cont_for,1,'captures current continuation for Goal in arg 1').
p0(call_with_cont,1,'calls closure in arg 1 with captured continuation').
p0(get_cont,1,'captures current continuation, usually an cyclic term').

p0(throw,1,'ISO Prolog exception operator: throws a term to be caught by a matching catch').
p0(catch,3,'ISO Prolog exception operator: executes arg 1 and if it catches arg 2, it executes arg 3').
p0(catch0,4,'').
p0(call_cont,1,'calls arg 1 as current continuation').
p0(swap_cont,2,'calls arg 1 as cc and returns cc in arg 2').
p0(handle_uncaught,1,'calls arg 1 as current continuation').
p0(commit,0,'removes all choicepoints and executes current continuation - a kind of super-cut').

p0(cstring_to_chars,2,'reads an internal C string declared with VSHARE to a list of chars').

p0(assert_from_chars,1,'asserts a program from clauses in list of chars').
p0(assert_from_chars,2,'(Db,Cs) asserts to database Db, a set of clauses parsed from list of char codes Cs').

p0(lval,2,'').

p0((type),1,'accepts type declarations although we are not using them currently').

p0(set_that_host,1,'same as set_host').
p0(set_that_port,1,'same as set_port').

p0(in,2,'waits to remove a term from Linda blackboard and returns no if failed').

*/

