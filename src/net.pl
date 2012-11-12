sock_read(Sock,Cs):-sock_read(Sock,0,Cs). % means arg 3 max is not needed
sock_readln(Sock,Cs):-sock_readln(Sock,0,Cs). % means arg 3 max is not needed
sock_write(Sock,Cs):-sock_write(Sock,Cs,0).
sock_writeln(Sock,Cs):-sock_writeln(Sock,Cs,0).



new_cserver(Port,ServerSocket):- new_server(Port,ServerSocket).
new_cservice(ServerSocket,Socket):-new_service(ServerSocket,0,Socket).
csock_read(Socket,ByteList):-sock_read(Socket,0,ByteList).
csock_write(Socket,ByteList):-length(ByteList,L),sock_write(Socket,ByteList,L).
close_csocket(Socket):-
  % traceln(disconnecting(Socket)),
  disconnect(Socket).


% NEW, ROBUST NETWORKING - BASED ON Jinni's new code

% high throughput client+server with socket reuse reused

% server side

rpc_server:-
  default_this_port(P),
  default_password(W),
  rpc_server(P,W).

rpc_server(Port,Password):-
  default_timeout(T),
  rpc_server(Port,Password,T).

rpc_server(Port,Password,Timeout):-
  new_server(Port,Server),
  serve(Server,Password,Timeout).
    
serve(Server,Password,Timeout):-
  repeat,
    ( new_service(Server,Timeout,Service)->
      bg(service_loop(Service,Password)),
      fail
    ; true
    ),
    !.

starting_service:-let(service_finished,no).
stop_service:-let(service_finished,yes).
service_stopped:-val(service_finished,yes).

service_loop(Service,Password):-
  starting_service,
  repeat,
    ( service_stopped->true
    ; answer_one_query(Service,Password)->fail
    ; true
    ),
  !,
  'disconnect'(Service).
  

% client side

rpc_test:-
  bg(rpc_server),
  sleep(3),
  rpc_test(10000).
  
rpc_test(N):-
  println(starting_rpc(times(N))),
  ctime(T1),
  start_rpc,
  for(I,1,N),
  rpc(X=I),
  X=N,
  stop_rpc,
  ctime(T2),
  T is T2-T1,
  L is 1000*(N/T),
  println([times(N),total_time(T),rpc_per_sec(L)]).
  
start_rpc:-
  default_host(H),
  default_port(P),
  default_password(W),
  start_rpc(H,P,W).
  
start_rpc(H,P,W):-
  new_client(H,P,Client),
  def(host,H),
  def(port,P),
  def(password,W),
  def(client,Client),
  !.
start_rpc(H,P,_W):-
  errmes(rpc_client_alredy_started,on(H,P)).
   
stop_rpc:-
  val(client,Client),
  val(password,W),
  !,
  rm(client),
  rm(host),
  rm(port),
  rm(password),
  stop_service(Client,W).
stop_rpc.

rpc(G):-rpc(G,G,the(G)).

rpc(X,G,R):-get_client_password(C,W),'ask'(C,X,G,W,R).

stop_service(Client,W):-
  'ask'(Client,_,stop_service,W,_),
  'disconnect'(Client).

get_client_password(C,W):-
  val(client,C),
  val(password,W).  

% Jinni compatibility mappings

write_to(Socket,Codes):-
  sock_write(Socket,Codes).

read_from(Socket,Codes):-
  sock_read(Socket,Codes).
  
decode_term(Codes,Term):-call_ifdef(term_decoder(Codes,Term),term_chars(Term,Codes)).
encode_term(Term,Codes):-call_ifdef(term_encoder(Term,Codes),term_chars(Term,Codes)).

% client side RPCs

disconnect(ServiceOrClient):-
  close_socket(ServiceOrClient).

ask_query_get_answer(Client,QW,A):-
  write_to(Client,QW),
  read_from(Client,A).

ask(Client,X,G,W,R):-
  encode_term(the(X,G,W),QW),
  ask_query_get_answer(Client,QW,A),
  decode_term(A,T),
  !,
  R=T.
ask(_,_,_,_,no).
    
ask_server(H,P,X,G,W,R):-
  new_client(H,P,Client),      
  ask(Client,X,G,W,R),
  disconnect(Client).

remote_run(G):-
  default_host(H),
  default_port(P),
  remote_run(H,P,G).

remote_run(H,P,G):-
  default_password(W),
  remote_run(H,P,G,G,W,the(G)).

% API element

remote_run(_H,_P,X,G,_W,R):-val(where,here),!,run_here(X,G,R).
remote_run(_H,_P,X,G,_W,R):-val(where,jinni),!,callj(X,G,R).
remote_run(H,P,X,G,W,R):-ask_server(H,P,X,G,W,R).

run_here(X,G,R):-copy_term((X:-G),(NewX:-NewG)),topcall(NewG),!,R=the(NewX).
run_here(_,_,no).

% server side RPC handling API

trust:-run_server.
   
run_server:-default_this_port(P),run_server(P).

run_server(Port):-run_server(Port,none).

run_server(Port,Password):-
  % can be set with set_default(heap(...)) etc.
  heap_size(Heap),
  stack_size(Stack),
  trail_size(Trail),
  run_server(Port,Password,Heap,Stack,Trail,0).
   
run_server(Port,Password,Heap,Stack,Trail,Timeout):-
  new_server(Port,Server),
  repeat,
    new_service(Server,Timeout,Service),
    %bg(handle_service(Service,Password)),
    bg(handle_service(Service,Password),Heap,Stack,Trail,  _Thread,_Engine,_Id),
  fail.
 
% END RPC API
  
handle_service(Service,Password):-
  if(answer_one_query(Service,Password),true,true),
  disconnect_service(Service).
 
answer_one_query(Service,Password):-
  read_from(Service,QString),
  %ttyprint(qstring(QString)),
  decode_term(QString,QTerm),
  react_to(QTerm,Password,ATerm),
  encode_term(ATerm,AString),
  write_to(Service,AString).
  
disconnect_service(Service):-
  disconnect(Service).
  
% defines how RPC requests are handled

react_to(Term,Password,Answer):-
   run_query_term(Term,Result,Password),
   !,
   Answer=the(Result).
react_to(_,_,Answer):-
   Answer=no.

run_query_term(the(Answer,Goal,CPassword),Answer,SPassword):-call_here(CPassword,SPassword,Goal).
run_query_term(run(CPassword,Answer,Goal),Answer,SPassword):-call_here(CPassword,SPassword,Goal). % BinProlog compatibility?

call_here(CP,SP,G):-CP==SP,!,call_ifdef(rpc_handler(G),topcall(G)).
call_here(CP,_,G):-println(warning(wrong_password_for_query(G),passwd(CP))).




% High level engine operations

/*
   creates new engine

*/

create_new_engine(Engine):-
  heap_size(H),
  stack_size(S),
  trail_size(T),
  create_engine(H,S,T,Engine).

create_engine(Engine):-
  heap_size(H),
  stack_size(S),
  trail_size(T),
  create_or_reuse_engine(H,S,T,Engine).

/*
  creates or reuses existing dead engine
*/
create_or_reuse_engine(H,S,T,Engine):-
  ( ENGINE_TYPE=0,
    DEAD_ENGINE=4,
    list_engines(Es),
    member(E,Es),
    get_engine_prop(E,ENGINE_TYPE,DEAD_ENGINE)
  ->
    get_engine_thread(E,Thread),
    thread_cancel(Thread),
    Engine=E,
    load_engine(E,true,_) % just to escape clean up
  ; 
    create_engine(H,S,T,Engine)
  ),
  clean_up_dead_engines.

% standard Jinni engine interface - interoperates with Jinni

new_engine(AnswerPattern,Goal,Handle):-
  open_engine(Goal,AnswerPattern,Handle).

get(Engine,Answer):-
%  nonvar(Answer),Answer=no->stop(Engine)
  ask_engine(Engine,X)->Answer=the(X)
; Answer=no.

stop(E):-destroy_engine(E).

return(TheData):-return0(TheData).

this_engine(E):-current_engine_addr(E).

to_engine(E,T):-bb_def('$engine_message',E,T).

from_engine(T):-this_engine(E),bb_val('$engine_message',E,R),bb_rm('$engine_message',E),R=T.

reuse_engine(X,G,E):-load_engine(E,G,X).

% fluent operations (mostly on Sources)

element_of(I,X):-pick_element_of(I,X).

pick_element_of(I,X):-get(I,the(A)),select_from(I,A,X).

select_from(_,A,A).
select_from(I,_,X):-pick_element_of(I,X).

% end of Jinni / Kernel Prolog interface

open_engine(Goal,X,Engine):-
  create_engine(Engine),
  load_engine(Engine,Goal,X).

call_engine(E,G,X):-
  load_engine(E,G,X),
  ask_engine(E,A),
  copy_term(A,X).
  
create_remote_engine(E):-
   heap_size(Hx),stack_size(Sx),trail_size(Tx),
   call_ifdef(remote_run(E,create_engine(Hx,Sx,Tx,E)),errmes(unimplemented,remote_run)),
   quietmes(1,created_remote_engine(E)).

load_remote_engine(E,G,X):-
   call_ifdef(remote_run(load_engine(E,G,X)),errmes(unimplemented,remote_run)),
   quietmes(1,loaded_remote_engine(E,G,X)).

start_remote_engine(G,X,E):-
  create_remote_engine(E),
  load_remote_engine(E,G,X).

ask_remote_engine(E,X):-
   call_ifdef(remote_run(ask_engine(E,X)),errmes(unimplemented,remote_run)).

stop_remote_engine(E):-
   call_ifdef(remote_run(destroy_engine(E)),errmes(unimplemented,remote_run)),
   quietmes(1,stopped_remote_engine(E)).

% some code for multiple engines
% to be moved to C
clean_up_engines:-clean_up_engines(_).

% goes to lib.pl
clean_up_dead_engines:-
  DEAD_ENGINE=4,
  clean_up_engines(DEAD_ENGINE).

clean_up_engines(Prop):-
   list_engines(Xs),
   member(X,Xs),
   ENGINE_TYPE=0,
   ROOT_ENGINE=0,
   ( get_engine_prop(X,ENGINE_TYPE,Prop),
     Prop=\=ROOT_ENGINE,
     destroy_engine(X)
   ->
      true,debugmes(destroyed_engine(X))
   ;  true,debugmes(failed_to_destroy_engine(X,Prop))
   ),
   fail.
clean_up_engines(_).

show_engine:-
  show_area(global_stack),
  show_area(local_stack),
  show_area(trail).
  
show_area(Area):-
  statistics(Area,[H1,H2]),
  H is H1+H2,
  debugmes(max(Area)=H),
  fail.
show_area(_).

list_engines:-
  ENGINE_TYPE=0,
  list_engines(Xs),
  member(X,Xs),
    get_engine_thread(X,T),
    get_engine_prop(X,ENGINE_TYPE,Prop),
    write([engine(X),type(Prop),thread(T)]),nl,
  fail
; nl.

% Blackboards and threads

% THREADS CORE PACKAGE

/*
% too verbose at quiet 0
trace_thread_op(Mes,X,Y,Z):-
  quiet(Q),Q=:=0,
  current_engine_addr(E),
  current_engine_id(I),
  current_engine_thread(T),
  write(thread_operation(Mes,op(X,Y,Z),thread(T),engine(addr(E),id(I)))),nl,
  fail.
*/  
trace_thread_op(_,_,_,_).

thread_operation(X,Y,Z):-
  trace_thread_op(entering,X,Y,Z),
  fail.
thread_operation(X,Y,Z):-
  tsync_op(X,Y,Z),
  !,
  trace_thread_op(exiting,X,Y,Z).
thread_operation(X,Y,Z):-
  trace_thread_op(failing,X,Y,Z),
  fail.

thread_exit:-thread_exit(0).

init_mutexes:-thread_operation(0,1,0).

release_mutexes:-thread_operation(0,0,0).

lock_thread_guard(S):-thread_operation(1,S,0).

unlock_thread_guard(S):-thread_operation(2,S,0).

thread_timed_wait(S,T):-thread_operation(1,S,T).

thread_wait(S):-default_timeout(T),thread_timed_wait(S,T).
        
try_unlock_thread_guard(S,T):-
  thread_timed_wait(S,T),
  unlock_thread_guard(S).

try_unlock_thread_guard(S):-try_unlock_thread_guard(S,50).

% begin not implemented on WIN32

thread_notify(S):-thread_operation(4,S,0).

thread_notify_all(S):-thread_operation(5,S,0).

% end not implemented on WIN32

thread_cancel(Thread):-thread_operation(6,Thread,0).

% begin not implemented on Solaris POSIX threads

thread_resume(Thread):-thread_operation(7,Thread,0).

% to be replaced with safer thread_self_suspend
thread_suspend(Thread):-thread_operation(8,Thread,0).

% end not implemented on Solaris Posix threads

println(X):-synchronize((write(X),nl)).

begin_critical:-lock_thread_guard(0).

end_critical:-unlock_thread_guard(0).

synchronize(Goal):-synchronize_on(0,Goal).

synchronize(Goal,R):-synchronize_on(0,Goal,R).

synchronize_on(S,Goal):-has_threads,!,synchronize_on(S,Goal,Ok),Ok.
synchronize_on(_,Goal):-Goal,!.

synchronize_on(S,Goal,R):-
  lock_thread_guard(S),
  (Goal->R=true;R=fail),
  unlock_thread_guard(S).

max_thread_lock(63).

new_thread_guard(Guard):-
  var(Guard),
  max_thread_lock(Max),
  for(I,2,Max),
    synchronize_on(1,bb_def(thread_guard,I,I)),Guard=I,
  !.
new_thread_guard(Guard):-
  nonvar(Guard)->
    synchronize(errmes(new_thread_guard,should_be_var(Guard)),_)
  ; synchronize(errmes(new_thread_guard,no_free_thread_guards_left),_).

free_thread_guard(Guard):-
  Guard>=2,Guard=<63,
  synchronize_on(1,bb_get(thread_guard,Guard,Guard)),
  !.
free_thread_guard(Guard):-
  synchronize(errmes(free_thread_guard,bad_guard(Guard)),_).

put_critical(Guard,Data):-
   synchronize_on(1,bb_def(critical,Guard,Data),Ok),
   ( Ok->sdebug(put_critical(Guard,Data))
   ; synchronize(errmes(error_in,put_critical(Guard,Data)),_)
   ).

get_critical(Guard,Data):-
   synchronize_on(1,bb_get(critical,Guard,Data),Ok),
   ( Ok->sdebug(get_critical(Guard,Data))
   ; synchronize(errmes(error_in,get_critical(Guard,Data)),_)
   ).

sdebug(Mes):-
  quiet(Q),Q<2->begin_critical,write(Mes),nl,end_critical
; true.


% THREADS

% THREAD EXISTENCE QUERY OPERATIONS

get_engine_thread(Engine,Thread):-
  ENGINE_THREAD=5,
  get_engine_prop(Engine,ENGINE_THREAD,Thread).

% uses in an essential way current_engine_addr->wam
current_engine_thread(T):-
  current_engine_addr(E),
  get_engine_thread(E,T).

has_threads:-vread(threads,X),X>0.
has_threads:-fail.


% TEMP

bg(Goal):-bg(Goal,_).

bg(Goal,Thread):-bg(Goal,Thread,_).

bg(Goal,Thread,Engine):-bg(Goal,Thread,Engine,_).

bg(Goal,Thread,Engine,Id):-
  heap_size(H),
  stack_size(S),
  trail_size(T),
  bg(Goal,H,S,T,  Thread,Engine,Id).

bg(Goal,H,S,T,  Thread,Engine,Id):-has_threads,!,
  create_or_reuse_engine(H,S,T,Engine),
  load_engine(Engine,((topcall(Goal)->fail;fail);thread_exit(0)),_),
                       % topcall ??? - lead to bug but it should be ok now
  get_engine_id(Engine,Id),
  ask_thread(Engine,Thread). % now already started with POSIX and WIN32
  % thread_resume(Thread). % not needed anymore
bg(Goal,_H,_S,_T,  nothreads,noengine,noid):-
  debugmes(running_in_forground(Goal,nothreads)),
  topcall(Goal),
  !.

% might have a bug: ^C does not always stops such processes
timed_call(Answer,Goal,Timeout,Result):-
  has_threads,Timeout>0,!, % not worth launching other thread if Timeout==0
  rtime(Stamp),
  bg(
     (
      (Answer^Goal->Ok=the(Answer);Ok=no),
       local_out(goal_done(Stamp,Timeout,Ok))
      ),
     _Thread,
     Engine
  ),
  for(I,1,Timeout),
    sleep(1),
    ( local_cin(goal_done(Stamp,Timeout,R))-> true
    ; rtime(Now),Now < Stamp+Timeout->
        debugmes(waiting(I+Now)),
        fail % continue monitoring
    ; % thread_cancel(_Thread)
      destroy_engine(Engine)
      ->
        R=stopped
    ; R=running
    ),
  !,
  Result=R.
timed_call(Answer,Goal,Timeout,Result):-
  debugmes(may_not_stop_on_timeout(Timeout,Goal)) ,
  ( Goal->Result=the(Answer)
  ; Result=no
  ).

% TESTED with WIN32 THREADS

% Basic multi-threaded Linda operations, Jinni-style 

local_rd(X):-synchronize_on(1,clause(X,true)).

local_all(X,Xs):-local_all(X,X,Xs).

local_all(X,G,Xs):-synchronize_on(1,findall(X,clause(G,true),Xs)).

local_out(X):-
  synchronize_on(1,(
    retract1(waiting(X,T)),assert(X)
  ))
  ,!,
  thread_resume(T).
local_out(X):-
  synchronize_on(1,assert(X)).

local_cin(X):-synchronize_on(1,retract1(X)).

local_in(X):-local_cin(X),!.
local_in(X):-
  current_engine_thread(T),
  synchronize_on(1,assert(waiting(X,T))),
  thread_suspend(T),
  local_cin(X).

local_cout(X):-local_rd(X)->true;local_out(X).

% local_when(X):-local_in(X),local_out(X).

local_when(X):-local_rd(X),!.
local_when(X):-
  current_engine_thread(T),
  synchronize_on(1,assert(waiting(X,T))),
  thread_suspend(T),
  local_rd(X).

% Advanced multi-threaded blackboard operation

% interface
% waits for Term until such that Constraint holds
wait_for(Term,Constraint):-local_rin(Term,Constraint).

% notifies a matching wait_for(Term,Constraint)

notify_about(Term):-local_rout(Term).

% shows all the terms waiting for X 

all_for(X,Xs):-local_rall(X,Xs).

% implementation

local_rin(P,C):-
  take_pattern(available_for(P),C)->true
; local_out(waiting_for(P,C)),
  local_in(holds_for(P,C)).

take_pattern(X,C):-
  synchronize_on(1,(
   clause(X,true),
   C,
   retract1(X)
  )).

local_rout(P):-
  local_rout(P,true).

local_rout(P,B):-
  B,
  (take_pattern(waiting_for(P,C),C)->
     local_out(holds_for(P,C))
  ;
     local_out(available_for(P))
  ).

local_rall(P,Ps):-
  local_all(P,available_for(P),Ps).


%%% operatioins are always local - use remote_run to act remotely

rd(X):-local_rd(X).

all(X,Xs):-local_all(X,Xs).

all(X,G,Xs):-local_all(X,G,Xs).

out(X):-local_out(X).

cin(X):-local_cin(X).

in(X):-local_in(X).

cout(X):-local_cout(X).

when(X):-local_when(X).
