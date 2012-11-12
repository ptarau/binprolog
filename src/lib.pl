% compile time tracers - 0 runtime cost if off - COMMENT OFF ONE OF EACH

hash_trace(T,TT):- is_compiled(T)->TT=T;TT=(prolog:T). % OFF
% hash_trace(T,x_traced_call(T)).                        %ON

x_trace(T,TT):- is_compiled(T)->TT=T;TT=(prolog:T).  % OFF
% x_trace(T,x_traced_call(T)).                         % ON

% RUNTIME TRACE HOOKS - slower

hash_trace(G):-x_traced_call(G).
x_trace(G):-x_traced_call(G).

x_traced_call(G):-
  quiet(Q),
  if(Q<2,println('!!ENTERING'=>G),true),
  ctime(T1),
  x_call_compiled(G,Ok),
  ctime(T2),
  if(Q<2,println('!!EXITING'(Ok)=>G),true),
  to_map_key(G,KeyG),
  (map_get('$profile',KeyG,x(S,K))->true;S=0.01,K=0),
  NewS is S+T2-T1,
  NewK is K+1,
  map_put('$profile',KeyG,x(NewS,NewK)),
  Ok.

to_map_key(G,KeyG):-
  functor(G,F,N),
  (N>0,arg(1,G,A),nonvar(A)->true;A='??'),
  namecat(F,'=>',A,KeyG).

x_profile:-
  bb_let('$ptotal',x(0,0)),
  map_get('$profile',KeyG,x(S,K)),
    S0 is S/K,
    println(KeyG:[calls(K),time(S),time_per_call(S0)]),
    bb_val('$ptotal',x(TS,TK)),
    NewTS is TS+S, NewTK is TK+K,
    bb_let('$ptotal',x(NewTS,NewTK)),
  fail
; quiet(Q),bb_val('$ptotal',x(TS,TK)),
  TS0 is TS/TK,
  if(Q<2,println('TOTAL PROFILED':[calls(TK),time(TS),average_time_per_call(TS0)]),true).

x_call_compiled(T0,Ok):-
  (is_compiled(T0)->T=T0;T=(prolog:T0)),
  % ttyprint(x_call_compiled=>T),
  if_any(T,Ok=true,Ok=fail).

% dynamic db
assert_from_chars(Cs):-current_db(Db),assert_from_chars(Db,Cs).

assert_from_chars(Db,Cs):-
  foreach(
    read_terms_from_chars(Cs,Clause),
    (expand_term(Clause,Expanded),db_assert(Db,Expanded))
  ).  

read_terms_from_chars(Xs,Clause):-read_terms_from_chars(Xs,Clause,_).

read_terms_from_chars(Xs,Clause,Vs):-
  append(Xs," ",Ys),
  chars2prog1(Ys,Css),
  member(Cs,Css),
  read_term_from_chars(Cs,Clause,Vs).

chars2prog1(Xs,Css):-chars2prog0(Css,Xs,_),!.

chars2prog0([Cs|Css])-->
  to_full_stop(Cs),!,
  chars2prog0(Css).
chars2prog0([])-->[].
  
to_full_stop(Xs)-->match_before(".",Xs,_),@(End),{member(End,[10,13,32,9])},!.

%% bboard garbage collector 

area_used(Area,Used):-
  statistics(Area,[Used,_Left]).

heap_used(Used):-area_used(global_stack,Used).

bb_used(Used):-area_used(bboard,Used).

bb_gc:-db_hook,call_ifdef(x_gc,true),fail.
bb_gc:-bbgc_status(0),!,clear_bp_error,errmes(blackboard_gc, disabled).
bb_gc:-bb_gc1.

% used!!! - might triggers bugs
% tailors new engine big enough for temporary copy of the blackboard
bb_gc1:-
  statistics(bboard,[BSize,_]),
  statistics(htable,[TSize0,_]),TSize is TSize0*8, /* upper estimate for the overhead for the list of bb keys */
  Size is BSize+TSize,
  Marg is 500,
  S is Marg, /* should be a small constant - see bb_gc0 which has a bound amount of choice points */
  H is max((2*Size//1024),Marg)+S, /* twice: once for B once for NewB */
  T is max(H//2,Marg), /* should be as big as the copy_term-ed B list?? */
  debugmes(entering_bb_gc1(engine(e=E,h=H,s=S,t=T),bb(Size))),
  create_engine(H,S,T,E),
  load_engine(E,bb_gc0,_),
  vget0(gc,GC),nogc,
  synchronize_on(1,ask_engine(E,_)),
  % ask_engine(E,_),
  vset(gc,GC),
  destroy_engine(E),
  statistics(bboard,NewSizes),
  statistics(global_stack,ThisHeap),
  debugmes(exiting__bb_gc1(engine(e=E,h=H,s=S,t=T),bb(BSize=>NewSizes),main_heap(ThisHeap))).

bb_gc0:-
  bb_used(BB1),
  statistics(global_stack,[H1,HLeft]),HMax is H1+HLeft,
  debugmes(entering_bb_gc0(heap(used(H1),total(HMax)),bboard_used(BB1))),
  bb_list0(3,1,B),              % length(B,_L1),
  heap_used(H2),
  copy_term(B,NewB),            % length(NewB,_L2),
  heap_used(H3),
  H is max(1000,(H3-H2)-(H2-H1)//2), % size of useful data
  bb_reset(H), % if set so, dynamically expands/shrinks blackboard 
  bb_put_back(NewB),
  bb_used(BB2),BB is BB1-BB2,
  debugmes(exiting__bb_gc0(bb_before=BB1,heap(estimated(HMax),bb_list(H2),actual(H3),useful(H)),bb_after=BB2,bb_savings=BB)),
     %     bb_list0(3,1,AfterB),length(AfterB,_L3),bb_valid(_L1,_L2,_L3),
  bb_check, 
  fail.
bb_gc0.

bb_check:-
   statistics(bboard,[Used,Left]),
   ( Left > Used>>4 -> true
   ; bb_fail(not_enough_garbage_colected(bb_used(Used),bb_left(Left)))
   ).

bb_put_back(Xs):-current_engine(E),bb_put_back1(E,Xs),fail. % $$ UNSAFE IN MULTI-ENGINE SITUATION!!!
bb_put_back(_).

bb_put_back1(E,[P,N,F,K|VXs]):-
   unify_to(VXs,[V|_]),
   functor(O,P,N),
   functor(M,F,K),
   % debugmes(putting_back(P/N+F/K)),
	 bb_put_1(E,O,M,V).
bb_put_back1(E,[_P,_N,_F,_K,_V|Xs]):-
	bb_put_back1(E,Xs).

bb_put_1(_,O,M,V):-save_term(V,NewV),set(O,M,NewV),!,fail.
bb_put_1(E,O,M,V):-bb_fail(bb_put_1(E,O,M,V)).

bb_fail(X):-
	statistics(bboard,[_,Z]),
	fatal_error('blackboard overflow, left only'(Z),culprit(X)). %fail
  
% Top-level handler components

prolog_load(yes):-
  call_ifdef(clean_up_engines,true),
  call_ifdef(init_io,true),
  exec_run_time_commands,
  run_first_command,
  check_if_runtime,
  show_help.
prolog_load(no).

prolog_init(yes):-
  run_more_commands,
  check_if_server,
  !.
prolog_init(no).

check_if_server:-vget0(this_port,int(P)),P>0,!,
  vset(err_id,int(76)),
  guess_port(P,Server),
  Server.
check_if_server.

check_if_runtime:-is_compiled(mcompile(_)),!.
check_if_runtime:-
  set_load_method(dconsult),
  Max is 1<<16,
  db_ratio(Max).
   

cmd_root('$run_time_command').

exec_run_time_commands:-
   def(exec_run_time_commands,done,yes),
   cmd_root(Root),
   exec_run_time_commands0(Root),
   fail.
exec_run_time_commands.

exec_run_time_commands0(Root):-
   MaxCmd=1000000,
   for(I,1,MaxCmd),
     symcat(Root,I,Cmd),
     debugmes('trying to execute command !!!'(Cmd)),
     (is_compiled(Cmd)->Cmd,fail
      ; debugmes('no such command'(Cmd))
     ),
   !.

run_first_command:-
  vget(startup_file_name,Cmd/0),
  Cmd\=='$empty',
  vset(startup_file_name,'$empty'/0),
  top_file_cmd(Cmd,Goal),
  Goal,
  !
; true.

run_more_commands:-
  vget(err_arg2,Flag/0),
  Flag=='$empty',
  vset(err_arg2,int(0)),
  unix_argv(Cs),
  read_or_skip_cmd(Cs,Gs),
  call_cmd_goals(Gs),
  !
; true.

read_or_skip_cmd([],[]).
read_or_skip_cmd([C|Cs],Gs):-
  ( top_file_cmd(C,fail),sread(C,G),\+cmd_flag(G)->Gs=[G|MoreGs]
  ; Gs=MoreGs
  ),
  read_or_skip_cmd(Cs,MoreGs).

call_cmd_goals([]).
call_cmd_goals([G|Gs]):-
  G,
  !,
  call_cmd_goals(Gs).

cmd_flag(-_):-!.
cmd_flag(I):-number(I),!.
cmd_flag('-').

% customize this for special handling of some commands

top_file_cmd(Name,Goal):- % patch unquoted cmd line file names
  name(Name,Cs),
  ( append(_,".wam",Cs)->Goal=load(Name)
  ; append(_,".bp",Cs)->Goal=true
  ; [L,R]="[]",append([L|Xs],[R],Cs)->name(New,Xs),Goal=include(New)
  ; member(S,"(,;)"),member(S,Cs)->Goal=fail
  ; Goal=include(Name)
  ).

guess_port(P,Server):-
  default_port_service(P,Server),
  debugmes(default_port_service(P,Server)).

% customize this 

default_port_service(0,start_rpc). 
default_port_service(1,rpc_server).
default_port_service(2,run_server).
default_port_service(10,true). % keep port unbound -> for testing
default_port_service(Port,run_server):-
   integer(Port),Port>10,
   set_this_port(Port).


default_timeout(X):-
    default(timeout(X),X=0).

set_timeout(T):-
    set_default(timeout(T)).

default_login(X):-
    default(login(X),detect_user(X)).

set_login(X):-
    set_default(login(X)).

default_password(P):-default(password(P),(get_password(P);P=none)).

check_password(P):-
    default_password(P0),P==P0->true
  ; errmes(intruder,bad_password(P)).

set_password(P):-
    hide_default(password(P)).

get_password(P):-
    show_default(password(P)).

default_port(X):-default(port(X),X=7001).

% Socket based operations with Java comptible Linda client/server - 

detect_user(U):-unix_getenv('USER',U),!.
detect_user(guest).

hostname(X):-vread(hostname,X).

detect_ip_addr(X):-vread(ip_addr,X).

default_this_host(X):-default(this_host(X),detect_ip_addr(X)).

set_this_host(X):-set_default(this_host(X)).

set_port(Port):-integer(Port),set_default(port(Port)).

default_this_port(P):-vget0(this_port,int(P)),P>10,!.
default_this_port(P):-default(this_port(P),default_port(P)).

set_this_port(Port):-integer(Port),vset(this_port,int(Port)).

default_host(X):-default(host(X),default_this_host(X)).

set_host(Server):-set_default(host(Server)).

        
% High-level compiling and consulting and restart/halt operations

[File]:-include(File).
  
include(File):-
   vget0(load_method,int(I)),
   load_method(I,Method),!,
   get_lineno(L),
   debugmes(lineno_before(L)),
   set_lineno(0),
   (call(Method,File)->true;true),
   debugmes(lineno_after(L)),
   set_lineno(L).

load_method(1,mcompile).
load_method(2,scompile).
load_method(3,oconsult).
load_method(4,dconsult).
load_method(5,sconsult).
load_method(6,load).

set_load_method(Name):-
  load_method(No,Name),!,
  vset(load_method,int(No)).

'~'(F) :- co(F).

co(F):-reconsult(F).

compile(File):-apply_load_method(compile,File).

reconsult(File):-apply_load_method(reconsult,File).

get_lineno(L):-vget_int(lineno,L),!.
get_lineno(('?')).

set_lineno(L):-integer(L),vset_int(lineno,L),!.
set_lineno(_).

get_load_method(Name):-
  vget0(load_method,int(No)),
  load_method(No,Name),
  !.

remember_load_method(Type,M):-
  vget0(Type,M/0),
  load_method(I,M),
  !,
  vset(load_method,int(I)),
  debugmes(load_method(I)).

apply_load_method(Type,File):-
  remember_load_method(Type,M),
  call(M,File).

pc:-push_code(_).

end_of_file :- halt.

abort :- abort0.

restart :- 
 call_ifdef(clean_up_engines,true),
 restart0,
 call_ifdef(init_io,true).

% Operting System (mostly Unix) interface

shell(C):-system(C).

system(Cmd):-system(Cmd,_).

system([],0):-!.
system([X|Xs],Ret):-!,name(Command,[X|Xs]),system0(Command,Ret).
system(Command,Ret):-atomic(Command),system0(Command,Ret).

cd:-unix_cd.

cd(X):-unix_cd(X).

pwd:-system(pwd).

pwd(Cs):-pcollect(pwd,Cs).

unix_cd:-
   ( unix_getenv('HOME',HomeDir)->true
   ; HomeDir=(/)
   ),
   unix_cd(HomeDir).

unix_argv(Xs):-
  unix_argc(Max),
  Last is Max-1, 
  % with unix_argv(0,X) you can also get the program's name
  findall(X,(for(I,1,Last),unix_argv(I,X)),Xs).


unix(argc(X)):-unix_argc(X).
unix(argv(Xs)):-unix_argv(Xs).
unix(argv(N,X)):-unix_argv(N,X).
unix(getenv(X,Y)):-
  unix_getenv(X,Y)->true
; errmes('no such environment variable',X).
unix(access(X,Y)):-unix_access(X,Y).
unix(cd(X)):-unix_cd(X).
unix(cd):-unix_cd.
unix(kill(X,Y)):-unix_kill(X,Y).
unix(popen(X,Y,Z)):-popen(X,Y,Z).
unix(pclose(X)):-pclose(X).
unix(pcollect(X,Y)):-pcollect(X,Y). % collects a pipe to a list
unix(fork(X)):-unix_fork(X). % forks
unix(pid(X)):-unix_pid(X). % forks
unix(cat(X)):-unix_cat(X). % prints a file

% pushes code into the BinProlog kernel
% the code is protected and it will not be cleaned-up by :-[...].

ls:-system('ls -tF').
dir:-system(dir).

unix_cat(FName):-
   fopen(FName,'rb',F),
     fsize(F,N),for(I,1,N),fgetc(F,C),put_code(C),I=:=N,!,
   fclose(F).
   
file2chars(FName,Cs):-
   fopen(FName,'rb',F),
   fgetc(F,C),fcollect(C,F,Cs),
   fclose(F).

fcollect(-1,_,[]):-!.
fcollect(X,F,[X|Xs]):-
   fgetc(F,NewX),
   fcollect(NewX,F,Xs).

codes_words(Cs,Ws):-var(Cs),!,
  findall(C,words_code(Ws,C),Cs).
codes_words(Cs,Ws):-to_words(Cs,Ws).

words_code(Ws,C):-words2nat(Ws,Ns),member(N,Ns),atom_codes(N,Xs),member(C,Xs).

line_of(F,Cs):- Ends=[10,13],sentence_of(F,Ends,Cs).

tokens_of(F,Ts):-
  findall(T,token_of(F,T),Ts).
  
token_of(F,W):-
  line_of(F,Cs),
  codes_words(Cs,Ws),
  member(W,Ws).
  
sentence_of(F,Ws):- Ends=".!?",sentence_of(F,Ends,Xs),codes_words(Xs,Ws).
     
sentence_of(FN,Ends,Xs):-find_file(FN,FName),fopen(FName,'rb',F),pick_code_of(F,Ends,Xs).

pick_code_of(F,Ends,Ys):-fgetc(F,X),collectx(Ends,X,F,Xs,More),select_code_from(F,Ends,Xs,Ys,More).

select_code_from(_,_,As,As,_).
select_code_from(I,Ends,_,Xs,yes):-pick_code_of(I,Ends,Xs).

collectx([E|_],-1,F,[E],no):-!,fclose(F).
collectx(Ends,End,_,[End],yes):-member(End,Ends),!.
collectx(Ends,X,F,[X|Xs],More):-
   fgetc(F,NewX),
   collectx(Ends,NewX,F,Xs,More).

char_of(FName,C):-
  find_file(FName,File),
  fopen(File,'rb',F),
  repeat,
    fgetc(F,C0),
    ( C0 =:= -1-> 
       !,fclose(F),fail
     ; C=C0
     ).
       
term_of(F,C):-
  find_file(F,File),
  seeing(F0),
  see(File),
  repeat,
    read(C0),
    ( C0==end_of_file-> 
       !,seen,see(F0),fail
     ; C0=':-'([_|_])->arg(1,C0,Fs),member(IF,Fs),term_of(IF,C)  
     ; C=C0
     ).

read_file(F,T):-seeing(F0),see(F),read(X),see(F0),T=X.

seen_file(F):-seeing(F0),see(F),seen,see(F0).
      
clause_of(F,C):-
  term_of(F,T),
  add_true(T,C).

edit(_,user):-!,include(user).
edit(Editor,File):-
        find_file0(File,F),
        make_cmd([Editor," ",F],R),
        system(R),
        include(F).

get_editor(_,Ed):-(unix_getenv('EDITOR',Ed);unix_getenv('VISUAL',Ed)),!.
get_editor(Default,Default).

current_user_file(F):-vget0(current_user_file,F/0).

set_current_user_file(File):-vset(current_user_file,File/0).

my_edit(DefaultEditor):-
   current_user_file(File),
   edit(DefaultEditor,File).

my_defedit(DefaultEditor):-
   current_user_file(File),
   defedit(DefaultEditor,File).

defedit(DefaultEditor,File):-
   get_editor(DefaultEditor,Ed),
   edit(Ed,File).

ed:-my_defedit(emacs).

edit:-my_defedit(edit).

textedit:-my_edit(textedit).
emacs:-my_edit(emacs).
notepad:-my_edit(notepad).
pico:-my_edit(pico).
vi:-my_edit(vi).

co:-
   current_user_file(File),
   include(File).
        

% spawns BinProlog in new windows on W95/NT PC and Unix+X

spawn(Goal):-spawn(Goal,[],'temp.pro').

spawn(Goal,Includes,TempFile):-
     tell(TempFile),
      foreach(member(F,Includes),(write((:-[F])),write('.'),nl)),
      nl,pp_clause((main:-Goal)),
     told,
     choose_shell_cmd(Pref,Post),
     namecat(Pref,TempFile,Post,Cmd),
     debugmes(command=Cmd),
     system(Cmd).


choose_shell_cmd(Pref,Post):-   vread(x86,X),
   unix_argv(0,BP),
   (
     X=:=0 -> Shell='xterm -e ',Post=(&) % unix
   ; % else if PC
     Shell='start ',Post=''
   ),
   namecat(Shell,BP,' ',Pref).

stat:-statistics.

  
% LOW-LEVEL ACCESS TO DATA `shared' with C (run-time system)

% relies on HDEFS,HDFI in ru.c

vread(CVar,Val):-val(bp_state,CVar,Val).

heap_size(X):-default(heap(X),vread(heap_size,X)).
stack_size(X):-default(stack(X),vread(stack_size,X)).
trail_size(X):-default(trail(X),vread(trail_size,X)).

% relies on VSHARE in ru.c
% using them on compound objects needs knowing what's going on in C

vget(CVar,Val):-val(bp_state,CVar,A),array_get(A,0,Val).
vget0(CVar,Val):-val(bp_state,CVar,A),array_get0(A,0,Val).

vset(CVar,Val):-val(bp_state,CVar,A),array_set(A,0,Val).

gvset(CVar,Val):-gval(Val,GroundVal),vset(CVar,GroundVal).

gvget(CVar,GroundVal):-vget(CVar,Val),atomize(Val,GroundVal).

vget_int(CVar,Intval):-vread(CVar,Addr),vget_int0(Addr,Intval).
vset_int(CVar,Intval):-vread(CVar,Addr),vset_int0(Addr,Intval).

atomize(var(Adr),Val):-!,array_get(Adr,0,Val).
atomize(Val,Val).

set_bp_error(Id,Mes,Arg1,Arg2):-
  vset(err_id,int(Id)),
  gvset(err_mes,Mes),
  gvset(err_arg1,Arg1),
  gvset(err_arg2,Arg2),
  vset(err_wam,var(0)),
  !.
set_bp_error(Id,Mes,_Arg1,_Arg2):-
  errmes(error_in_error_handler,set_bp_error(Id,Mes)).

clear_bp_error:-
  set_bp_error(0,'$empty','$empty','$empty').

get_bp_error(Id,Mes,Arg1,Arg2):-
  vget0(err_id,int(Id)),
  vget0(err_mes,Mes/0),
  vget0(err_arg1,Arg1),
  vget0(err_arg2,Arg2),!.
get_bp_error(Id,Mes,_Arg1,_Arg2):-
  errmes(error_in_error_handler,get_bp_error(Id,Mes)).

% GC options, enable, disable etc.

gc:-gc_set(1).

nogc:-gc_set(0).

gc_set(Status):-vset(gc,int(Status)).

gc_status(X):-vget0(gc,int(X)).

dynbbgc:-vset(bbgc,int(2)).

bbgc:-bbgc(1).

bbgc(N):-vset(bbgc,int(N)).

nobbgc:-vset(bbgc,int(0)).

bbgc_status(X):-vget0(bbgc,int(X)).

% control on system messages `quietness'

quiet(X):-integer(X),!,vset(quiet,int(X)).
quiet(X):-vget0(quiet,int(X)).

is_bbvar(X):-var(X),gval(X,var(AX)),
   vget0(bboard,var(B)),vget0(bboard_margin,var(M)),
   AX>=B,AX<M.

is_heapvar(X):-var(X),gval(X,var(AX)),
   vget0(heap,var(B)),vget0(heap_margin,var(M)),
   AX>=B,AX<M.


% -----------------------------------------------------------------
% LIBRARY of basic predicates

% accepts but does note use mode/type declarations

mode(_).
type(_).

is_engine(E):-list_engines(Es),member(E,Es).

% expands to strange things in Sicstus: written in binary Prolog here

':'(M,P,Cont)::-module_call(M,P,Cont).

module_call(_,P) :- is_public(P),!,P.
module_call(M,P) :- module_predicate(M,P,MP), MP.

module_predicate(M,Pred,MPred):-
  functor(Pred,F,_),
  module_name(M,F,MF),
  term_append(MF,Pred,MPred).

module_name(M,P,MP):-
  nonvar(M),nonvar(P),!,
  namecat(M,':',P,MP).
module_name(M,P,_):-
  user_error(should_be_nonvar,[M,P]).

module(M):-begin_module(M).

begin_module(M):-
  bb_let(current,module_name,M),
  bb_let(module_name,M,M).

end_module:-begin_module(user).

end_module(M):-current_module(M),!,end_module.
end_module(M):-errmes(bad,end_module(M)).

module(M,FNs):-
  module(M),
  member(FN,FNs),
    public0(FN),
  fail.
module(_,_).

current_module(M):-bb_val(current, module_name , M0),!,M=M0.
current_module(user).

is_module(M):-nonvar(M),!,bb_val(module_name,M,_).
is_module(M):-bb_list(Xs),bb_element(module_name/0+M/0=_,Xs).

modules(Ms):-findall(M,is_module(M),Ms).

hide_atom(G,H):-hide_atom0(G,H),!.
hide_atom(G,G).

hide_atom0(_,_):-current_module(user),!,fail.
hide_atom0(Pred,_):-is_public(Pred),!,fail.
hide_atom0(Pred,MPred):-current_module(M),module_predicate(M,Pred,MPred).

public0(F/N):-
   functor(P,F,N),
   current_module(M),
   bb_def((public),P,M),
   !.
public0(Bad):-
   errmes('bad or repeated declaration',public(Bad)).

public((A,B)):-!,public(A),public(B).
public(A):-public0(A).

is_public(Pred):-is_public(Pred,_).

is_public(Pred,Module):-bb_val((public),Pred,M),!,Module=M.
is_public(Pred,_):-is_builtin(Pred).

init_gensym(Root):-bb_let(gensym,Root,0).

gensym(Root,Symbol):-gensym_no(Root,N),symcat(Root,N,Symbol).

gensym_no(Root,N1):-
        val(gensym,Root,N),!,
        N1 is N+1,
        set(gensym,Root,N1).
gensym_no(Root,N):-N=1,
        def(gensym,Root,N).

current_op(Pri,Assoc,Name):-nonvar(Name),!,
  get_op(Name,Assoc,Pri).
current_op(Pri,Assoc,Name):-
  for(I,0,10000),
  (val(opmark,I,Name)->get_op(Name,Assoc,Pri);!,fail).

op(Pri,Assoc,Name):-op0(Name,Assoc,Pri).

get_op(Name,Assoc,Pri):- % nonvar(Name),
	op_type(Assoc,Cls),
	get_op0(Name,Cls,Assoc,Pri).

get_op0(Name,Cls,Assoc,Pri):- % nonvar(Name), nonvar(Cls),
	val(Cls,Name,Assoc),
	val(Name,Cls,Pri),
	Pri>0.

op_type(xfy,infixop).
op_type(xfx,infixop).
op_type(yfx,infixop).

op_type(fx,prefixop).
op_type(fy,prefixop).

op_type(xf,postfixop).
op_type(yf,postfixop).

set_prop(X,Prop):-
  append_conj(X,true,FNs),
  member_conj(F/N,FNs),
    functor(P,F,N),
    bb_let(P,Prop,yes),
  fail.
set_prop(_,_).

is_multifile(P):-val(P,'$multifile',_).

multifile(X):-set_prop(X,'$multifile').
  
is_discontiguous(P):-is_multifile(P).

discontiguous(X):-multifile(X).

static_prolog_flag(version,BPV):-vread(version,V),symcat('BinProlog',V,BPV).
static_prolog_flag(is_prolog,Which):-is_prolog(Which).
static_prolog_flag(dynamic_compilation,YN):-dynco(YN).

prolog_flag(Flag,Value):-default(dynamic_prolog_flag(Flag,Value),static_prolog_flag(Flag,Value)).

set_prolog_flag(Flag,Value):-set_default(dynamic_prolog_flag(Flag,Value)).

bp_info(F/N,I):-nonvar(F),nonvar(N),!,
  functor(H,F,N),N1 is N+1,functor(H1,F,N1),
  gen_bp_info(H,H1,I),
  !.
bp_info(F/N,I):-
  gen_bp_info(H,_,I),
  functor(H,F,N).

gen_bp_info(H,H1,I):-
  ( bu0(H1,_,_,I),functor(H1,F,N1),N is N1-1,functor(H,F,N)
  ; bu1(H,I)
  ).

bp_info(FN,Text,Exs):-
  bp_info(FN,I),
  (I=''->fail
  ;I=Text-Ex->make_exs(FN,Ex,Exs)
  ;Text=I,Exs=[]
  ).
bp_info(FN,Text,[]):-
  comment2info(FN,Text).
  
comment2info(FN,Text):-
  (var(FN)->current_predicate(FN);true),
  to_bp_comment(FN,G,Xs),
  functor(G,F,1),
  once(current_predicate(F,1)),
  name(Text,Xs).
 
about_to_bp_comment_3(
  "Maps a predicate p/n to a commented predicate about_p_n/1."
).
 
to_bp_comment(F/N,CommentPred,CommentArg):-  
  make_cmd([about,'_',F,'_',N],G),
  functor(CommentPred,G,1),
  arg(1,CommentPred,CommentArg).
  
make_exs(FN,Ex,Exs):-
  to_list(Ex,Xs),
  map(extract_ex(FN),Xs,Exs).

to_list([F|Fs],[F|Fs]):-!.
to_list(F,[F]).

extract_ex(F/_,T0,Vs^T):-
  term_append(F,T0,T), 
  vars_of(T,Vs).

show_help:-quiet(Q),show_help(Q).

show_help(Q):-Q<1,show_defaults,fail.
show_help(Q):-Q<2,
  (member(X,[co,ed,notepad,trace,pc])
  ;load_method(_,X)
  ),
  info(X),
  fail.
show_help(_).

has_info(FN):-bp_info(FN,Text,_),Text\==''.

help:-make_html_help,nl,make_text_help,nl.

make_text_help:-F='help.txt',
 write('generating file: '),write(F),nl,
 write('containing help on builtins and examples'),nl,
 write('please read it with the editor of your choice'),nl,
 tell(F),
   info,
 told.

info:-info(_),fail;true.

info(info):-!.
info(FN):-nonvar(FN),FN=_/_,!,info0(FN).
info(F):-
   for(N,0,254),
     has_info(F/N),write(F/N),write(': '),info0(F/N),
   fail
;  true.

info0(FN):-
  show_info(name,FN,Text+Exs),
  show_info(text,Text),
  show_info(examples,Exs).

show_info(name,FN,Text+Exs):-
  ( var(FN)->Show=yes
  ; Show=no
  ),
  bp_info(FN,Text,Exs),
  ( Show=yes->write('PREDICATE: '),write(FN),nl
  ; Show=no
  ).
show_info(text,Txt):-
  write('INFO: '),write(Txt),nl,nl.
show_info(examples,Exs):-
  ( Exs=[]->true
  ; write('EXAMPLE(S):'),nl,nl,
      member(Vs^G,Exs),namevars0(Vs^G,Ns^G0),
      write('?-'),write(G0),write('.'),nl,
      ( G,
         show_bindings(Ns,Vs),nl,
        fail
      ; (Vs=[]->write(yes);write(no)),nl,nl
      ),
    fail
  ; true
  ).

show_bindings([],[]).
show_bindings([V|Vs],[A|As]):-
  write(V=A),write((;)),nl,
  show_bindings(Vs,As).

/* HTML help */
% now part of BinProlog

make_html_help:-F='help.html',
 write('generating html file: '),write(F),nl,
 write('containing help on builtins and examples'),nl,
 write('please read it with the browser of your choice'),nl,
 tell(F),
   info_html,
 told.

prelude:-write('<html>'),nl,
  T='The BinProlog API: Prolog built-ins and their descriptions',
  write('<head>'),nl,
  write('<title>'),write(T),nl,write('</title>'),nl,
  write('</head>'),nl,nl,
  write('<body>'),nl,
  blue,
  write('<center><h1>'),nl,write(T),nl,write('</h1></center>'),nl,
  write('<ol>'),nl.
postlude:-
  nl,write('</ol>'),nl,
  efont,nl,
  write('</body>'),nl,write('</html>'),nl.

info_html:-prelude,gen_info_html,fail;postlude.

gen_info_html:-
   findall(FN,has_info(FN),FNs),sort(FNs,Sorted),
   length(Sorted,L),
   write('<center><h2>'),b('Total number of documented built-ins:'),b(L),write('</h2></center>'),nl,nl,
   member(F/N,Sorted),
     show_info_html(F/N),
   fail
;  true.

show_info_html(FN):-
  bp_info(FN,Text,Exs),
  li,black,b(FN),b(': '),efont,
  i(Text),
  show_info_html_examples(Exs),
  eli.

show_info_html_examples(Exs):-
  ( Exs=[]->true
  ; red,pre,show_examples_html(Exs),epre,efont
  ).

show_examples_html(Exs):-
  nl,b('EXAMPLE(S):'),nl,
      member(Vs^G,Exs),prolog:namevars0(Vs^G,Ns^G0),
      write('?-'),write(G0),write('.'),nl,
      ( G,
         show_bindings_html(Ns,Vs),nl,
        fail
      ; (Vs=[]->write(yes);write(no)),nl
      ),
    fail
  ; true.

show_bindings_html([],[]).
show_bindings_html([V|Vs],[A|As]):-
  write(V=A),write((;)),nl,
  show_bindings_html(Vs,As).


b(X):-write('<b>'),write(X),write('</b> ').
i(X):-write('<i>'),write(X),write('</i> ').

blue:-nl,write('<font color="blue">').
red:-nl,write('<font color="red">').
black:-nl,write('<font color="black">').
efont:-nl,write('</font>').

pre:-nl,write('<pre>').
epre:-write('</pre>'),nl.

br:-write('<br>').
hnl:-write('<p>').

li:-write('<li>').
eli:-write('</li>').

/* end HTML help */

is_builtin_nonvar(T):-bu1(T,_),!.
is_builtin_nonvar(T):-term_append(T,cont(_),TC),bu0(TC,_,_,_).

is_builtin_var(T):-bu1(T,_).
is_builtin_var(T):-bu0(TC,_,_,_),strip_cont(TC,T,_).

is_builtin(T):-nonvar(T),!,is_builtin_nonvar(T).
is_builtin(T):-is_builtin_var(T).

interactive(X):-var(X),!,(is_interactive->X=yes;X=no).
interactive(X):-nonvar(X),interactive0(X).

is_interactive:- \+ current_op(1199,fx,('?-')).

interactive0(yes):-op(1200,fx,('?-')).
interactive0(no):-op(1199,fx,('?-')).

expand_term(C,E):-portable_expand_term(C,E).
std_expand_term(C,D):-portable_expand_term(C,D).

strip_cont(TC,T,C):- strip_cont0(TC,C-T).
% NASTY BUG IN BUILTIN strip_cont0... => SOLVED !!!

metatrue(TC)::-strip_cont(TC,T,C,metacall(T,C)).

metacall(G):-call_body(G).

bincall(BinCont,Cont,Cont) ::- BinCont.

call(FXs,Y1):-term_append(FXs,args(Y1),G),G.
call(FXs,Y1,Y2):-term_append(FXs,args(Y1,Y2),G),G.
call(FXs,Y1,Y2,Y3):-term_append(FXs,args(Y1,Y2,Y3),G),G.
call(FXs,Y1,Y2,Y3,Y4):-term_append(FXs,args(Y1,Y2,Y3,Y4),G),G.
call(FXs,Y1,Y2,Y3,Y4,Y5):-term_append(FXs,args(Y1,Y2,Y3,Y4,Y5),G),G.
call(FXs,Y1,Y2,Y3,Y4,Y5,Y6):-term_append(FXs,args(Y1,Y2,Y3,Y4,Y5,Y6),G),G.

% override(P,NewP):-override(0,P,NewP). % backtrackable if 0,

safe_override_call(P,NewP,Goal):-
  gc_status(GC),nogc, % gc is unhappy with arbitray ints trailed as code ptrs
  % bbgc_status(BB),nobbgc, % no need to disable bbgc !!!
  findall(Goal,override_call(P,NewP,Goal),Gs),
  gc_set(GC),
  % bbgc(BB),
  member(Goal,Gs).

override_call(P,NewP,Goal):-
  override(0,P,NewP), % backtrackable if 0, hard otherwise
  Goal.

once(G):-G,!.

maplist(F,Xs):-map0(Xs,F).
map(F,Xs):-map0(Xs,F).

map0([],_).
map0([X|Xs],F):-
   call(F,X),
   map0(Xs,F).

maplist(F,Xs,Ys):-map0(Xs,F,Ys).
   
map(F,Xs,Ys):-map0(Xs,F,Ys).

map0([],_,[]).
map0([X|Xs],F,[Y|Ys]):-
   call(F,X,Y),
   map0(Xs,F,Ys).
 
foldl(F,Z,Xs,R):-foldl0(Xs,F,Z,R).
  
foldl0([],_,R,R).
foldl0([X|Xs],F,R1,R3):-call(F,R1,X,R2),foldl0(Xs,F,R2,R3).

foldr(F,Z,Xs,R):-foldr0(Xs,F,Z,R).
  
foldr0([],_,Z,Z).
foldr0([X|Xs],F,Z,R2):-foldr0(Xs,F,Z,R1),call(F,X,R1,R2).

sum(Xs,R):-foldl(+,0,Xs,R).

prod(Xs,R):-foldl(*,1,Xs,R).

termcat(T,C,TC):-term_append(T,cont(C),TC).
 
% true.
% call(X):-X.

otherwise.

false:-fail.

call_ifdef(G,_):-is_compiled(G),!,G.
call_ifdef(G,Else):-debugmes(
    failed(call_ifdef(G))=>calling_instead(Else)
  ),fail.
call_ifdef(_,Else):-Else.

callable(G):-is_compiled(G),!.
callable(G):-is_dynamic(G),!.
callable(G):-nonvar(G),functor(G,call,_).

default(G,_):-callable(G),G,!.
default(G,_):-show_default(G),!.
default(_,AltG):-AltG,!.

set_default(H):-
  functor(H,F,N),functor(H0,F,N),
    retractall(H0),
  assert(H).

hide_default(H):-bb_let(hidden,H,H).

show_default(H):-bb_val(hidden,H,H).

% (A:-B):-assert((A:-B)).

X=X.

A->B :- A,!,B.

A->B ; C :- !,if(A,B,C).
X ; _ :-X.
_ ; Y :-Y.

or(A,_):-A.
or(_,B):-B.

if(A,B,_):-A,!,B.
if(_,_,C):-C.

(X,Y):-X,Y.

\+(X):-X,!,fail.
\+(_).

\=(X,X):-!,fail.
\=(_,_).

halt:-halt(-2).

quit:-halt.
exit:-halt.

stop:-halt(0).

repeat.
repeat:-repeat.

forall(Goal):-Goal,fail.
forall(_).

forall(When,Then):- When,once(Then),fail.
forall(_,_).

foreach(Goal):-Goal,fail.
foreach(_).

foreach(When,Then):- When,once(Then),fail.
foreach(_,_).

for_all(Goal,OtherGoal):- \+ ((Goal, \+ OtherGoal)).

eq(X,X).

and(X,Y):-X,Y.

compute(Op,A1,A2,R):-call(Op,A1,A2,R).

read_line(L):-read_chars(Cs),name(L,Cs).

read_words(Ws):-read_chars(Cs),codes_words(Cs,Ws).

% write_words(Ws):-member(W,Ws),write(W),put(32),fail; nl.

write_words(Ws):-nonvar(Ws),!,write_ws(Ws).
write_words(Ws):-errmes(list_expected,found(Ws)).

write_ws(Ws):-words2nat(Ws,Ns),member(N,Ns),name(N,Cs),member(C,Cs),put(C),fail.
write_ws(_).

words2nat(Ws,Ns):-words2nat(Wss,Ws,[]),appendN(Wss,Vs),once(append(Us,[S],Vs)),(' '=S->Ns=Us;Ns=Vs).

words2nat([])-->[].
words2nat([[W,C,' ']|Wss])-->[W,C],{left_collider(C)},!,words2nat(Wss).
words2nat([[L,C]|Wss])-->[L,C],{collider(C)},!,words2nat(Wss).
%words2nat([[Q,W,Q,' ']|Wss])-->[Q],{Q=('"')},[W],[Q],!,words2nat(Wss).
words2nat([[Q|Vs],[Q]|Wss])-->[Q],{Q=('"')},match_before(Q,Ws),!,{words2nat(Ws,Vs)},words2nat(Wss).
words2nat([[W,' ']|Wss])-->[W],words2nat(Wss).

left_collider(W):-member(W,[(','),(':'),(';'),('.'),('!'),('?')]).

collider(W):-member(W,[(''''),(-)]).

current_engine(X):-current_engine_id(X).

current_engine_id(ID):-
  current_engine_addr(E),
  get_engine_id(E,ID).

get_engine_id(E,ID):-
  ENGINE_ID=7,
  get_engine_prop(E,ENGINE_ID,ID).

% if G has a finite number of solutions 
% returns a list Xs of copies of X each
% instantiated correspondingly

findall(X,G,Xs):-findall(X,G,Xs,[]).
% findall(G,Gs):-findall(G,G,Gs).

% uses 2 alternative implementations
findall(X,G,Xs,End):-synchronize_on(61,qfindall(X,G,Xs,End)).

% persistent queue based findall

qfindall(X,G,Xs):-qfindall(X,G,Xs0,[]),Xs0=Xs.

qfindall(X,G,Xs,End):-
  inc_level('$findall','$level',L),
  qfindall(X,G,Xs,End,L),
  dec_level('$findall','$level',R),
  ( R=:=L->true
  ; debugmes(different_findall_levels(R,L))
  ).

qfindall(X,G,_,_,Level):-
  G,
  addq(findall_bag,Level,X),
  fail.
qfindall(_,_,Xs,End,Level):-
  collect_findall_bag(Cs,End,Level),
  Cs=Xs.

collect_findall_bag([X|Xs],End,Level):-
  cpopq(findall_bag,Level,X),
  !,
  collect_findall_bag(Xs,End,Level).
collect_findall_bag(End,End,_).

inc_level(K,O,X1):-val(K,O,X),!,X1 is X+1,set(K,O,X1).
inc_level(K,O,1):-def(K,O,1).

dec_level(K,O,X):-val(K,O,X),X>0,X1 is X-1,set(K,O,X1).


% The following can together be used to give the illusion of having
% a parallel engine which produces answers lazily, as needed.
% Intended to be used as take and drop work in haskell,
% on possibly infinite streams of solutions,

% they use WAM-level means as change_arg/3 in has_fuel/3
% the reader is challenged to express them in classical Prolog :-)
% To do more, a separate Prolog engine or first order manipulation
% of OR continuations is needed...

% answers G as far as constraint C holds
while(C,G):-G,(C->true;!,fail).

skip_until(C,G):-G,(C->fail;!).

skip_when(C,G):-G,(C->fail;true).

% gives only the N-th answer of G
nth_answer(N,G):-N1 is N-1,Max=s(N1),skip_until(has_fuel(Max),G).

% generates at most the first N answers of G
take_at_most(N,G):-Max=s(N),while(has_fuel(Max),G).

% drops at least the first N answers of G
drop_at_least(N,G):-Max=s(N),skip_when(has_fuel(Max),G).

% re-entrant on-place counter
has_fuel(Max):-arg(1,Max,N),N>0,N1 is N-1,change_arg(1,Max,N1).

% answer_stream to list converters
find_while(C,X,G,Xs):-findall(X,while(C,G),Xs).

find_at_most(N,X,G,Xs):-findall(X,take_at_most(N,G),Xs).

all_but_at_least(N,X,G,Xs):-findall(X,drop_at_least(N,G),Xs).

% signals error if G has more than one answer
det_call(G):-find_at_most(2,G,G,Gs),!,
  ( Gs=[]->fail
  ; Gs=[G]->true
  ; % member(G,Gs),
    errmes('expected to be deterministic',G)
  ).

% SWI, SICStus, Mercury compatible if/3 construct
% which backtracks over Cond (SWI uses *-> for this)

if_any(Cond,Then,Else):-
  Ctr=s(0),
  ( Cond,change_arg(1,Ctr,1),Then
  ; arg(1,Ctr,0),Else
  ).

% if V is not common to X and G then V cannot be bound by all/3
% => such variables should be shared
% this is _different_ from what bagof does, and I think, better !
% try: ?-all_answers(X,member(s(X),[A,B,B,C]),Xs).

all_answers(X,G,Xs):-
  are_shared(X,G,Vs),
  findall(X-Vs,G,Ys),
  share_vars(Ys,Vs,Xs).

are_shared(X,G,vars(Is,Os)):-
  free_variables(X,G,[],Is,0), % free variables in X not occurring in G
  free_variables(G,X,[],Os,0). % free variables in G not occurring in X

share_vars([],_,[]).
share_vars([T-Vs|TVs],Vs,[T|Ts]):-share_vars(TVs,Vs,Ts).

gc_read(R):-findall(X,read(X),[R]).

gc_read_clause(R):-findall(X,read_clause(X),[R]).

gc_call(G):-findall(G,G,Gs),member(G,Gs).

answer_of(G,X):-findall(X,G,Xs),sort(Xs,Sorted),member(X,Sorted).

% Mercury style: Closure+LastArg->ListOfLastArgsCollectedInSolutions

solutions(Closure,Xs):-
  term_append(Closure,lastarg(X),Goal),
  findall(X,Goal,Xs).

member_conj(C,(C,_)).
member_conj(C,(_,Cs)):- member_conj(C,Cs).

% souce level member/2 + for/3

member(C,[C|_]).
member(C,[_|Cs]):- member(C,Cs).

memberchk(X,Xs):-member_scan(X,Xs,[Y|_]),!,Y=X.

/*
for(Min,Min,Max):-Min=<Max.
for(I,Min,Max):-Min<Max,Min1 is Min+1,for(I,Min1,Max).

member(X,Xs) :- member_entry(X,Xs).
member(X,[Y|Ys]) :- member3(X,Ys,Y).

memberchk(C,[C|_]):-!.
memberchk(C,[_|Cs]):- memberchk(C,Cs).
*/

full_mem(X,[Y|Ys]) :- member3(X,Ys,Y).

member3(X,_,X) .
member3(X,[Y|Ys],_) :- member3(X,Ys,Y).

for(I,Min,Max):-for_entry(I,Min,Max).
for(I,Min,Max):-for1(I,Min,Max).

for1(Min,Min,Max):-Min=<Max.
for1(I,Min,Max):-Min<Max,Min1 is Min+1,for1(I,Min1,Max).

between(Min,Max,I):-for(I,Min,Max).

argn(I,T,X):-nonvar(I),!,arg(I,T,X).
argn(I,T,X):-functor(T,_,N),for(I,1,N),arg(I,T,X).

numbervars(VAR_V, N0, N) :- ##numbervar_name(VAR_V,N0),!, N is N0+1.
numbervars(X, N0, N) :- atomic(X), !, N0=N.
numbervars([X|Xs], N0, N) :- !,
        numbervars(X, N0, N1),
        numbervars(Xs, N1, N).
numbervars(X, N0, N) :-
        functor(X, _, A),
        numbervars(0, A, X, N0, N).

numbervars(A, A, _, N0, N) :- !, N0=N.
numbervars(A0, A, X, N0, N) :-
        A1 is A0+1,
        arg(A1, X, X1),
        numbervars(X1, N0, N1),
        numbervars(A1, A, X, N1, N).

%ground(T):-numbervars(T,0,N),N>0,!,fail. ground(_).

ground(T):-deep_hash(T,1024,0,_).

hkey(X,Key):-deep_hash(X,1,0,Key).

term_hash(X,Key):-deep_hash(X,16,0,Key).

copy_term(T,CT):-copy_term(0,T,CT).
save_term(T,CT):-copy_term(1,T,CT).

clone_term(SharedVars,T,CT):-copy_term(SharedVars^T,SharedVars^CT).

flush:-seen_told(2).

see_or_fail(File):-see_tell(0,File).

exists_file(File):-see_or_fail(File),seen.

% symbol <--> term conversion

sread(I,O):-sread(I,O,_).

sread(I,O,Vs):-name(I,Xs),read_term_from_chars(Xs,O,Vs).

swrite(I,O):-t2s(I,Cs),atom_codes(O,Cs).

% OLD swrite definitions

%sread(I,O):-string_op(0,I,O).
%swrite(I,O):-string_op(1,I,O).

swrite0(I,O):-string_op(1,I,O).

% had bug on large write
swrite1(I,O):-term_chars(I,Xs),name(O,Xs).

swrite(I,Vs,O):-write_term_to_chars(I,Vs,Xs),name(O,Xs).

/* TERM TO LIST OF CHARS CONVERSION */


% reads one term from a list of chars

read_term_from_chars(Cs,T):-read_term_from_chars(Cs,T,_).

read_term_from_chars(Codes,Term,Vars):-codes2term(Codes,Term,Vars).

%%% read_term_from_chars(Cs,T,Vs):-s2t(Cs,T-Vs).

write_term_to_chars(T,Cs):-write_term_to_chars(T,[],Cs).

write_term_to_chars(T,Vs,Xs):-
  nonvar(Vs)->
  findall(Xs,write_term_to_chars1(T,Vs,Xs),[Xs])
; errmes(should_be_nonvar,Vs).

write_term_to_chars1(T,Vs,Xs):-
   list2conj(Vs,G),G,
   term_chars(T,Xs).

list2conj([],true).
list2conj([X|Xs],(X,Ys)):-list2conj(Xs,Ys).

% term to string (ascii codes)

t2s(Term,Cs):-string_op(3,Term,Cs).

% string (ascii codes) to term

s2t(Cs,Term-Vars):-codes2term(Cs,Term,Vars).

/* OLD:
%term_chars(T,Cs):-nonvar(T),!,swrite(T,S),name(S,Cs).
%term_chars(T,Cs):-name(S,Cs),sread(S,T).
*/

% string of chars <--> term conversion

term_chars(T,Cs):-term_codes(T,Cs).

term_codes(T,Cs):-nonvar(T),!,t2s(T,Cs).
term_codes(T,Cs):-nonvar(Cs),!,codes2term(Cs,T).
term_codes(_,"_"). % when both args are vars!


atom_codes(T,Cs):-atom(T),!,name(T,Cs0),Cs=Cs0. % do not change here!
atom_codes(N,Cs):-number(N),!,swrite0(N,T),name(T,Cs).
atom_codes(T,Cs):-
 name(T0,Cs),
 ( number(T0)->swrite0(T0,T)
 ; T=T0
 ).

number_codes(N,Cs):-number(N),swrite0(N,T),name(T,Cs),!.
number_codes(N,Cs):-nonvar(Cs),name(T,Cs),sread(T,N),number(N),!.
number_codes(N,Cs):-errmes(bad_arguments,number_codes(N,Cs)).

atom_chars(T,Xs):-nonvar(T),!,atom_codes(T,Cs),codes2chars(Cs,Xs).
atom_chars(T,Xs):-chars2codes(Xs,Cs),atom_codes(T,Cs).

number_chars(T,Xs):-nonvar(T),!,number_codes(T,Cs),codes2chars(Cs,Xs).
number_chars(T,Xs):-chars2codes(Xs,Cs),number_codes(T,Cs).

codes2chars([],[]).
codes2chars([C|Cs],[X|Xs]):-name(N,[C]),swrite0(N,X),codes2chars(Cs,Xs).

chars2codes([],[]).
chars2codes([X|Xs],[N|Cs]):-atom_codes(X,[N]),chars2codes(Xs,Cs).

to_string(Term,Atom):-number(Term),!,swrite0(Term,Atom).
to_string(Term,Atom):-term_chars(Term,Cs),name(Atom,Cs).

% data conversion stuff

to_lower_char(C,LC):- [A,Z,LA]="AZa",C>=A,C=<Z,!, LC is LA-A+C.
to_lower_char(C,C).

to_upper_char(LC,C):- [LA,LZ,A]="azA",LC>=LA,LC=<LZ,!, C is A-LA+LC.
to_upper_char(LC,LC).

to_upper_chars([],[]).
to_upper_chars([X|Xs],[Y|Ys]):-
  to_upper_char(X,Y),
  to_upper_chars(Xs,Ys).

to_lower_chars([],[]).
to_lower_chars([X|Xs],[Y|Ys]):-
  to_lower_char(X,Y),
  to_lower_chars(Xs,Ys).

  
char_type(X,T):-char_type0(T,X).

char_type0(upper,X):-"AZ"=[A,Z],X>=A,X=<Z,!.
char_type0(lower,X):-"az"=[A,Z],X>=A,X=<Z,!.
char_type0(digit,X):-"09"=[A,Z],X>=A,X=<Z,!.
char_type0(space,X):-member(X,[9,10,13,32]),!.
char_type0(period,X):-member(X,".?!"),!.  
  
% DCG alternative to AG stuff

match_word([C|Cs])--> [C],!,match_word(Cs).
match_word([])-->[].


match_before(Stop,Cs)-->match_before([Stop],Cs,_).

match_before(Stops,[],Stop)-->[Stop],{member(Stop,Stops)},!.
match_before(Stops,[C|Cs],Stop)-->[C],match_before(Stops,Cs,Stop).


% FILE operations

file_extension_list([".pl",".pro",""]).

file_library([LIBRARY],[".pl"]):-getenv_path("../library",LIBRARY).

file_search_path(["","myprogs/",PROGS,SRC,LIB,BPROOT,PPATH]):-
  getenv_path('PROLOG_PATH',"",PPATH),
  getenv_path(".",BPROOT),
  getenv_path("../src",SRC),
  getenv_path("../progs",PROGS),
  getenv_path("../library",LIB).
 
getenv_path(Default,Computed):-
  getenv_path('BP_PATH',Default,Computed).

getenv_path(Root,Default,Computed):-
  Slash="/",
  ( unix_getenv(Root,Where)->
    name(Where,Xs),append(Xs,Slash,RootPath)
  ; RootPath=""
  ),
  append(Default,Slash,Local),
  append(RootPath,Local,Computed).

make_file_name(Prefs,File,Sufs,NewFile):-
        member(Pref,Prefs),
        member(Suf,Sufs),
        name(File,L),
        append(L,Suf,R),
        append(Pref,R,Fname),
        name(NewFile,Fname).

statistics(Area,[Used,Free]):-stat_dict(Area,No),stat0(Used,Free,No).

statistics:-
	statistics(Name,Data),
	fast_write(Name),fast_write(=),fast_write(Data),nl,
	fail.
statistics.	


atom(X):-integer(X),!,fail.
atom(X):-atomic(X).

% float(X):-nonvar(X),X='$float'(_,_,_).

float(T):-nonvar(T),functor(T,'$float',3),arg(1,T,X),X>=0.

number(X):-integer(X),!.
number(X):-float(X).

compound(X):-nonvar(X), \+(atomic(X)).

=..(T,[X|Xs]):-var(T),!,list2term([X|Xs],T).
=..(T,L):-term2list(T,('.'),[],L).

term2list(T,L):-term2list(T,('.'),[],L).

term2dlist(T,Head,Queue):-term2list(T,('.'),Queue,Head).

append([],Ys,Ys).
append([A|Xs],Ys,[A|Zs]):-
        append(Xs,Ys,Zs).

/*
append([],Ys,Ys).
append([A|Xs],Ys,[A|Zs]):-
  det_append0(Xs,NewXs,EndXs-Zs),
  append(EndXs,Ys,NewXs).
*/

% det_append0 works with an arbitrary constructor
% it guesses it from the first constructor in Xs

det_append(Xs,Ys,Zs):-det_append0(Xs,Ys,_-Zs).

det_append(Xs,Ys,Zs,End):-det_append0(Xs,Ys,End-Zs).

appendN(Xss,Xs):-appendN(Xss,Xs,[]).

appendN([],End,End).
appendN([Xs|Xss],S1,S2):-det_append0(Xs,End,_-S1),appendN(Xss,End,S2).

append_body(C1,C2,C3):-append_body(C1,C2,C3,_).

append_body(C1,C2,C3,True):-
  functor(C1,F,2),
  det_append0(C1,T,End-C3),
  ( End==True->T=C2 
  ; T=..[F,End,C2]
  ).

append_conj(C1,C2,C3):-append_body((X,C1),C2,(X,C3),true).

append_disj(C1,C2,C3):-append_body((X;C1),C2,(X;C3),fail).

% this allows using list-functions in is: Xs is [1,2]++[3]++[4,5]

'.'(X,Xs,[X|Xs]).

++(Xs,Ys,Zs) :- det_append0(Xs,Ys,[]-Zs). 

reverse(Xs,Ys):-rev(Xs,[],Ys).

rev([],Ys,Ys).
rev([X|Xs],Ys,Zs):-rev(Xs,[X|Ys],Zs).

length(L,N):-var(N),!,get_length(L,0,N).
length(L,N):-make_length(L,0,N).

get_length([],I,I).
get_length([_|L],I0,I):-I1 is I0+1,get_length(L,I1,I).

make_length([],I,I):-!.
make_length([_|L],I0,I):-I0<I,I1 is I0+1,make_length(L,I1,I).

not_subsumes0(A,B):-copy_term(B,CB),numbervars(CB,0,_),CB=A,!,fail.
not_subsumes0(_,_).

subsumes_chk(A,B):-not_subsumes0(A,B),!,fail.
subsumes_chk(_,_).

variant_of(A,B):-subsumes_chk(A,B),subsumes_chk(B,A).

tab(Expr) :- expr(Expr,N),integer(N),!,tab0(N).
tab(Bad) :- user_error('in tab/1: should evaluate to an integer', Bad).

tab0(N):-for(_,1,N),put_code(32),fail.
tab0(_).

get(R):-repeat,get_code(X),(X>32;X<0),!,R=X.

% !! use something like Y=3+4, X is 1+expr(Y) instead of
% !! Y=3+4, X is 1+Y which will not work in compiled code

X is E:-expr(E,R),!,X=R.
X is E:-errmes(error_in_is,X is E).

expr(E,R):-atomic(E),!,R=E.
expr(E,R):-float(E),!,R=E.
expr(E,_Error):-var(E),!,user_error(variable_in_is,E).
expr(E,R):-functor(E,Op,2),!,
	arg(1,E,E1),arg(2,E,E2),
	expr(E1,X1),
	expr(E2,X2),
        call(Op,X1,X2,R),!.
expr(E,R):-functor(E,Op,1),
	arg(1,E,E1),
	expr(E1,X1),
        call(Op,X1,R),!.

compare(R,X,Y):-compare0(X,Y,R).

+(X,N):- +(0,X,N).
-(X,N):- -(0,X,N).
\(X,N):- \(0,X,N).

A==B :- compare0(A,B,=).
A\==B :- compare0(A,B,R),'$noteq'(R).

A @< B :- compare0(A,B,<).
A @> B :- compare0(A,B,>).
A @=< B :- compare0(A,B,R),'$lesseq'(R).
A @>= B :- compare0(A,B,R),'$gteq'(R).

A < B :-expr(A,X), expr(B,Y), less(X,Y).

A > B :-expr(A,X), expr(B,Y), greater(X,Y).

A =< B :-expr(A,X), expr(B,Y), less_eq(X,Y).

A >= B :-expr(A,X), expr(B,Y), greater_eq(X,Y).

A =:= B :-expr(A,X), expr(B,Y), arith_eq(X,Y).

A =\= B :-expr(A,X), expr(B,Y), arith_dif(X,Y).

'$lesseq'(<).
'$lesseq'(=).

'$gteq'(>).
'$gteq'(=).

'$noteq'(<).
'$noteq'(>).

% BinProlog USER LEVEL naming primitives
% DO NOT rely on def/3, set/3, val/3,rm/2. Thay are unsafe
% and their implementation may change in the future...

/********* BASIC operations: safe with respect to bb_gc ***********/

% WARNING: may be moved to C, do not use their componnents directly

bb_def(A,B,X):-saved(X,S),def(A,B,S).
bb_set(A,B,X):-saved(X,S),set(A,B,S).
bb_val(A,B,CX):-val(A,B,X),copy_term(X,CX).
bb_rm(A,B):-rm(A,B).

/******************DERIVED OPERATIONS ***********************/

% safe bboard operations
bb_let(A,B,X):-val(A,B,_),!,bb_set(A,B,X).
bb_let(A,B,X):-bb_def(A,B,X).
bb_get(A,B,X):-bb_val(A,B,X),bb_rm(A,B).

bb_def(A,X):-current_engine(E),bb_def(E,A,X).
bb_set(A,X):-current_engine(E),bb_set(E,A,X).
bb_val(A,X):-current_engine(E),bb_val(E,A,X).
bb_rm(A):-current_engine(E),bb_rm(E,A).
bb_let(A,X):-current_engine(E),bb_let(E,A,X).
bb_get(A,X):-current_engine(E),bb_get(E,A,X).



nb_setval(X,A):-bb_let(X,A).

nb_getval(X,A):-bb_get(X,A).

nb_delete(X):-bb_val(X,_),!,bb_rm(X).
nb_delete(_).

bb_default_val(A,B,_,X):-bb_val(A,B,X),!.
bb_default_val(_,_,D,D).

saved(X,S):-save_term(X,NewX),!,unify_to(NewX,S).
saved(X,S):-bb_gc,save_term(X,NewX),!,unify_to(NewX,S).
saved(X,S):-bb_fail(saved(X,S)).

% unsupported & unsafe

let(X,Y,V):-def(X,Y,V),!.
let(X,Y,V):-set(X,Y,V).

def(A,X):-current_engine(E),def(E,A,X).
set(A,X):-current_engine(E),set(E,A,X).
val(A,X):-current_engine(E),val(E,A,X).
let(A,X):-current_engine(E),let(E,A,X).
rm(A):-current_engine(E),rm(E,A).

% An easy patch to read.pl for basic float input - Paul Tarau, Oct. 1993

try_float(_Ds,[atom('.'),integer(box(_Fraq,FDs))|Ts],I,[atom(Float)|Ts1]):-!,
	try_float_exp(Ts,Ts1,Exp),name(NFraq,FDs),
	input_float(I,NFraq,Exp,Float).
try_float(Ds,Tokens,I,[integer(box(I,Ds))|Tokens]).

try_float_exp([atom(e)|Ts],Ts1,Exp):-try_float_exp1(Ts,Ts1,Exp),!.
try_float_exp([atom(EN)|Ts],Ts,Exp):- [E]="e",
	name(EN,[E|NL]),name(Exp,NL),integer(Exp),!.
try_float_exp(Ts,Ts,0).

try_float_exp1([atom('-'),integer(box(Exp,_))|Ts],Ts,Exp1):-!,
	Exp1 is 0-Exp.
try_float_exp1([atom('+'),integer(box(Exp,_))|Ts],Ts,Exp).

float_minus(F,NegF):-NegF is 1/2-F-1/2.

'**'(A,X,R):-float_fun2(pow,A,X,R). % same as pow
pow(A,X,R):-float_fun2(pow,A,X,R).
log(A,X,R):-float_fun2(log,A,X,R).
atan2(Y,X,R):-float_fun2(atan2,Y,X,R).

%sqrt(X,R):-A=1,B=2,E is A/B,float_fun2(pow,X,E,R).
%hypot(X,Y,R):- '*'(X,X,X2), '*'(Y,Y,Y2),'+'(X2,Y2,S),sqrt(S,R).

sqrt(X,R):-float_fun('Q',X,R).
hypot(X,Y,R):-float_fun2(hypot,X,Y,R).

% F(X+Y) --> R used for Y=0
exp(X,R):-float_fun(exp,X,R).
log(X,R):-float_fun(log,X,R).
sin(X,R):-float_fun(sin,X,R). 
cos(X,R):-float_fun(cos,X,R).
tan(X,R):-float_fun(tan,X,R).

% F(X)+Y --> R used for Y=0
atan(X,R):-float_fun('T',X,R).
asin(X,R):-float_fun('S',X,R).
acos(X,R):-float_fun('C',X,R).

integer(X,R):-float_fun(integer,X,R).
float(X,R):-R is (1/2)+X-(1/2).

% some ISO arithmetic stuff

sign(X,S):- X>0,!,S=1.
sign(0,0):- !.
sign(_,-1).

abs(X,R):- X>=0,!,R=X.
abs(X,R):- R is -X.

floor(X,R):-integer(X,R).
ceiling(X,R):-integer(X,I),R is I+1.
truncate(X,R):-integer(X,R).
round(X,R):-R is integer(1/2+X).

xor(X,Y,Z):-'#'(X,Y,Z).

% for portability...
ctime(T):-statistics(runtime,[T,_]).
rtime(T):-statistics(realtime,[T,_]).
otime(T):-statistics(realtime,[_,T]).
abstime(T):-statistics(realtime,[CT,OT]),T is CT+OT.

is_predicate(H):-is_builtin(H),!.
is_predicate(H):-is_compiled(H),!.
is_predicate(H):-is_dynamic(H).

predicate_property(H,Prop):-nonvar(H),!,predicate_property0(H,Prop).
predicate_property(H,Prop):-
  generate_a_predicate(H),
  predicate_property0(H,Prop).

predicate_property0(H,Prop):-is_asserted(H),!,
  (Prop=(asserted);Prop=(dynamic);Prop=(interpreted)).
predicate_property0(H,(built_in)):-is_builtin(H),!.
predicate_property0(H,Prop):-is_multifile(H),!,
  (Prop=(multifile);Prop=(discontiguous)).
predicate_property0(H,(compiled)):-is_compiled(H).

current_predicate(F,H):-nonvar(H),!,is_predicate(H),functor(H,F,_).
current_predicate(F,H):-generate_a_predicate(H),functor(H,F,_).

current_predicate(F/N):-nonvar(F),nonvar(N),!,
  functor(H,F,N),is_predicate(H),!.
current_predicate(F/N):-
  generate_a_predicate(H),
  functor(H,F,N).

predicate_key0(Assumed/0,PN,V,PN):-current_engine(Assumed),nonvar(V).
predicate_key0(PN,'$first'/0,_V,PN).
predicate_key0(predmark/0,P/N,predmark,P/N1):-N>0,N1 is N-1. % for true/0

generate_a_predicate(P):-
  ( is_builtin_var(P)
  ; generate_compiled(P), \+ is_builtin_nonvar(P)
  ; generate_run_time_predicate(P)
  ).

generate_compiled(P):-
	bb_list0(0,0,Xs),
 	bb_element(K1+K2=V,Xs),
        predicate_key0(K1,K2,V,F/N),
        functor(P,F,N).

% DYNAMIC CODE

% Blackboard related utilities

% bboard visualisation

bb_element(P/N+F/K=V,[P,N,F,K,V|_]).
bb_element(D,[_,_,_,_,_|L]):-bb_element(D,L).

bb_list(L):-bb_list0(3,0,L). % lists for arity >= 0

bb0:-bb(plain).

bb:-bb(unsorted).

bb(How):-
	statistics(bboard,X),write(bboard-X),nl,
	bb_list(L),
	( How\==sorted->findall(E,bb_element(E,L),Es),sort(Es,Bs)
	; Bs=L
        ),
	member(B,Bs),bb_orig(B,B0),
	( How==plain->write(B0),nl;pp_term(B0)),
	fail
; nl.

bb_orig(K1/N1+K2/N2=_,K1/N1+K2/N2=V):-
  functor(T1,K1,N1),
  functor(T2,K2,N2),
  (  val(T1,T2,V)
  ;  lval(T1,T2,V)
  ),!.

% C-based low-level stack/queue bboard operations
% implementation: please consider it as a black box!

popq1(O,M,X):-val(O,M,S),popq0(S,X).

addq1(O,M,X):-val(O,M,S),!,addq0(S,X).
addq1(O,M,X):-addq0(S,X),def(O,M,S).

pushq1(O,M,X):-val(O,M,S),!,pushq0(S,X).
pushq1(O,M,X):-pushq0(S,X),def(O,M,S).

addq(O,M,X):-addq1(O,M,X),!.
addq(O,M,X):-get_bp_error(_,Mes,_,_),
  (
    Mes=='BB_OVERFLOW',bb_gc,addq1(O,M,X),clear_bp_error,!
    ; Mes\=='BB_OVERFLOW',fatal_error(Mes,addq(O,M,X))
  ).

pushq(O,M,X):-pushq1(O,M,X),!.
pushq(O,M,X):-get_bp_error(_,Mes,_,_),
  (
    Mes=='BB_OVERFLOW',bb_gc,pushq1(O,M,X),clear_bp_error,!
    ; Mes\=='BB_OVERFLOW',fatal_error(Mes,pushq(O,M,X))
  ).

cpopq(O,M,X):-popq1(O,M,X),!.
cpopq(O,M,X):-
  get_bp_error(_,Mes,_,_),Mes\=='$empty',
  errmes(Mes,cpopq(O,M,X)).

memberq(X,Xs):-member_scan(X,Xs,[Y|Zs]),mq1(X,Y,Zs).

mq1(X,Y,_):-unify_to(Y,X).
mq1(X,_,Zs):-memberq(X,Zs).

cmember_filt(X0,Xs,[Y|Xs2]):-
   cmember_scan(X0,Xs,[Y|Xs1]),!,
   cmember_filt(X0,Xs1,Xs2).
cmember_filt(_,_,[]).

% vulnerable to bb_gc occurring while code in remaining code is executing
% cmemberq(C,I):-cq0(C,I).

% slightly less efficient for large predicates
cmemberq(C,I):-cmember_filt(C,I,Cs),member(C,Cs).

cq0(X,Xs):-cmember_scan(X,Xs,[Y|Zs]),cq1(X,Y,Zs).

cq1(X,X,_).
cq1(X,_,Xs):-cq0(X,Xs).

cdelq_all(C,Xs):-dq0(C,Xs).

dq0(C,Xs):-
  cdel_scan(C,Xs,[NewC|Ys]),
  dq1(C,NewC,Ys).

dq1(C,C,_).
dq1(C,_,Ys):-dq0(C,Ys).

% removes the first item matching X and takes a copy
% nondestructive on X
cdelq(Key,Name,X,Deleted):-
  tval(Key,Name,var(XsYs)),array_get(XsYs,1,var(Xs)),
  cdel_scan(X,Xs,[Deleted|_]).

cdelq_any(Key,Name,X):-
  tval(Key,Name,var(XsYs)),array_get(XsYs,1,var(Xs)),
  cdelq_all(X,Xs).

% end of (low-level) primitives

% higher level persistent queue/stack manipulation tools

% checks if there
membq(Key,Name,X):- 
  val(Key,Name,XsYs),
  unify_to(XsYs,Xs-_),
  memberq(X,Xs).

% nondestructively checks/copies matching X's
cmembq(Key,Name,X):- 
  tval(Key,Name,var(XsYs)),array_get(XsYs,1,var(Xs)),
  cmemberq(X,Xs).

/* dynamic/1, assert/1 & retract/1 predicates

This is an approximation of other Prologs assert & retract predicates.
For efficiency and programming style reasons we strongly suggest
not to use them too much.

If you want maximal efficiency use bb_def/3 bb_set/3 bb_val/3 
and lval/3. They give you acces to a very fast
hashing table <key,key>--> value, the same that BinProlog
uses internally for indexing by predicate and first argument.

Lval/3 (i,i,?) introduces named global logical variables.
A friendlier syntax (with no extra cost) is

Key1#Key2:=:Var  ...for lval(Key1,Key2,Var)

and Key:=:Var    ...for lval(Key,Key,Var)

Beware that assert & retract are not optimized for large databases
or frequent use of an asserted predicate.

To have dynamic predicates in a compiled file you must declare 
them as such with dynamic/1. All predicated in a consulted file
are dynamic. Dynamic code DOES NOT override compiled code
*/

set_db(DB):-vset(current_db,DB/0).
current_db(DB):-vget0(current_db,DB/0).

this_db(DB):-current_db(DB).

db_hook:-val('$db_hook',on,yes).

db_hook_on:-let('$db_hook',on,yes).

db_hook_off:-rm('$db_hook',on).
dynamic(Ps):-db_hook,!,x_dynamic(Ps).
dynamic(Ps):-dynamic0(Ps).

dynamic0(Ps):-define_dynamic(Ps),fail.
dynamic0(_).
 
%define_dynamic((P1,P2)):-!,define_dynamic(P1),define_dynamic(P2).
define_dynamic(P/N):-
	functor(T,P,N),
	check_dynamic(T,P/N),
	define_dynamic1(T).

check_dynamic(T,PN):-is_compiled(T),!,
  debugmes('WARNING: compiled predicate'(PN)),nl.
check_dynamic(_,_).

define_dynamic1(H):-current_db(Db),!,define_dynamic1(Db,H).

define_dynamic1(Db,H):-db_asserta(Db,H),db_retract1(Db,H),!.

asserta(C):-db_hook,!,x_asserta(C).
asserta(C):-
  current_db(DB),
  db_asserta(DB,C).

assertz(C):-db_hook,!,x_assertz(C).
assertz(C):-
  current_db(DB),
  db_assertz(DB,C).

db_asserta(DB,C0):-
  db_keep_clause(DB,C0,H,C),
  pushq(DB,H,C).

db_assertz(DB,C0):-
  db_keep_clause(DB,C0,H,C),
  addq(DB,H,C).

assert(C):-assertz(C).

db_assert(DB,C):-db_assertz(DB,C).

db_asserted(DB,H):-nonvar(H),db_clause(DB,H,B),call_body(B).

asserted(H):-nonvar(H),clause(H,B),call_body(B).

clause(H,B):-db_hook,!,x_clause(H,B).
clause(H,B):- current_db(DB),db_clause(DB,H,B).

db_clause(DB,H,B):-nonvar(H),!,cmembq(DB,H,(H:-B)).
db_clause(DB,H,B):-db_head(DB,H),cmembq(DB,H,(H:-B)).

clause0(H,B):- current_db(DB),membq(DB,H,(H:-B)).

check_for_clash(DB,DB):-!,
  errmes('known bug: unable to assert in db'(DB),DB/0).
check_for_clash(_,_).

db_ratio(X):-integer(X),!,vset(db_ratio,int(X)).
db_ratio(X):-vget0(db_ratio,int(X)).

% checks call/update ratio that triggers dynamic compilation

dynco(YN):-var(YN),!,db_ratio(X),Max is 1<<16,(X<Max->YN=yes;YN=no).
dynco(yes):-!,db_ratio(50).
dynco(no):-X is 1<<16,db_ratio(X).

get_small_db_ratio(X):-vget0(db_ratio,int(X)),X<1<<16.

dynamize(DB,H):-get_small_db_ratio(R),!,
    val(H,DB,K),
    K1 is K +R,set(H,DB,K1),
    disable_static(DB,H).       % H in DB is ready for updates!
dynamize(_,_).

disable_static(DB,H):-current_db(DB),!,disable_static(H).
disable_static(_,_).

disable_static(H):-is_compiled(H),!,override(2,H,fail).
disable_static(_).

db_keep_clause(DB,C0,H,C):-
  add_true(C0,C),
  unify_to(C,(H:-_)),
  check_for_clash(DB,H),
  ( get_small_db_ratio(R)->
    disable_static(H,DB),
    ( val(H,DB,K)->K1 is K +R,set(H,DB,K1) % dyn_compile related
    ; functor(H,F,N),
      addq('$clauses',DB,F/N),
      def(H,DB, R) % dyn_compile related
    )
  ; 
    ( % def(H,DB,'$has_clauses'),
      Max is 1<<16,def(H,DB,Max),
      functor(H,F,N)->addq('$clauses',DB,F/N)
    ; true
    )
  ).

% db enumerators
db_pred(DB,F,N):-cmembq('$clauses',DB,F/N).

db_pred(F,N):- current_db(DB),cmembq('$clauses',DB,F/N).

db_head(DB,H):-db_pred(DB,F,N),functor(H,F,N).

% retracts at most 1 matching clause
db_retract1(DB,H):-C=(H:-_),dynamize(DB,H),cdelq(DB,H,C,C).

retract1(H):-current_db(DB),db_retract1(DB,H).

% retracts and backtracks - with immediate update semantics -
% might cause problems with multiple engines !!!

db_retract(DB,H):-db_retract(DB,H,_B).

db_retract(DB,H,B):-
  tval(DB,H,var(XsYs)),array_get(XsYs,1,var(Xs)),
  dynamize(DB,H),
  cdelq_all((H:-B),Xs).

/*
% retracts and backtracks - with immediate update semantics -
% restarts from the beginning of a predicate each time
% on backtracking - should be safe with multiple engines
% although concurrent backtracking is STRONGLY discuraged
% anyway because of its too complex operational semantics!

db_retract(DB,H):- \+ db_clause(DB,H,_),!,fail.
db_retract(DB,H):- db_retract1(DB,H).
db_retract(DB,H):- db_retract(DB,H).
*/

retract(H):-db_hook,!,x_retract(H).
retract(H):-
  current_db(DB),
  db_retract(DB,H).

% retracts all matching clauses

db_retractall(Db,H):-db_retractall(Db,H,_B).

db_retractall(Db,H,B):-
  functor(H,_,N),
  for(I,1,N),
  arg(I,H,A),
  nonvar(A),
  !,
  db_retractall0(Db,H,B).
db_retractall(Db,H,_B):-
  functor(H,F,N),
  !,
  db_abolish(Db,F/N),
  db_asserta(Db,H),
  db_retract1(Db,H).
/*  
db_retractall(Db,H):-
  var(H),
  db_clean(Db).
*/

db_retractall0(DB,H,B):-db_retract(DB,H,B),fail.
db_retractall0(_,_,_).

retractall(H):-db_hook,!,x_retractall(H).
retractall(H):-current_db(DB),db_retractall(DB,H).

db_clean:-
  current_db(DB),
  db_clean(DB).

db_clean(DB):-
  db_pred(DB,F,N),
    db_abolish(DB,F,N),
  fail.
db_clean(_).


db_move(From,To):-
  db_head(From,H),
    db_keep_clause(To,(H:-true),_,_),
    val(From,H,Saved),
    let(To,H,Saved),
    (val(H,From,Ctr)->let(H,To,Ctr);true),
    functor(H,F,N),
    db_abolish(From,F/N),
  fail.
db_move(_,_).

db_abolish(DB,F,N):-db_abolish(DB,F/N).

db_abolish(DB,F/N):-
  functor(H,F,N),
  ( val(DB,H,_)->
    rm(DB,H),    
    cdelq('$clauses',DB,F/N,_),
    rm(H,DB)
  ; true
  ).

abolish(F,N):-abolish(F/N).


abolish(FN):-db_hook,!,x_abolish(FN).
abolish(FN):-
  current_db(DB),
  db_abolish(DB,FN).
 
db_save(File):-
  current_db(Db),
  db_save(Db,File).

db_save(Db,File):-
  tell(File),
  ( db_clause(Db,H,B),
    qprint((H:-B)),
    fail
  ; true
  ),
  told.

% db related

xsave(File):-
  namecat(File,'.','plx',F),
  quiet(Q),quiet(1111),
  db_save(F),
  quiet(Q).

xload(File):-
  namecat(File,'.','plx',F),
  quiet(Q),quiet(1111),
  call_ifdef(xinit,true),
  consult(F),
  quiet(Q).

make_compileable(Files,File):-
  % Db='$temp_db',
  current_db(Db),
  Cmds='$temp_cmds',
  db_clean(Db),
  db_clean(Cmds),
  (atomic(Files)->Fs=[Files];Fs=Files),
  (
    member(F,Fs),
    db_collect(F,Cmds,Db),
    fail
  ; true
  ),
  telling(CFile),
  tell(File),
  foreach(
    db_clause(Cmds,cmd(Cmd),_),
    qprint(':-'(Cmd))
  ),
  foreach(
    db_clause(Db,H,B),
    qprint((H:-B))
  ),
  told,
  tell(CFile),
  db_clean(Cmds),
  db_clean(Db).  
  
db_collect(F,Cmds,Db):-
  term_of(F,C),
  % println(C),
  ( C=':-'(Cmd)->
    ( functor(Cmd,G,1),member(G,[(discontiguous),(multifile)])->true
    ; Cmd=vae_dynamic(FN)->db_assert(Cmds,cmd(dynamic(FN)))
    ; db_assert(Cmds,cmd(Cmd)),
      % println(Cmd),
      once(topcall(Cmd))
    )
  ; db_assert(Db,C)
  ),
  fail
; true.

% quick call tracer

'?'(G):-
  pp_clause(calling_:G),
  S=s(0),
  if_any(G,
    (arg(1,S,K),
     K1 is K+1,
     change_arg(1,S,K1),
     pp_clause(exit_(K1):G)
    ),
    (
      pp_clause(failing_:G),
      fail
    )
  ).

% Hilog call'@'(P,Conj):-  P=..[F|As],conj2list(Conj,Bs),  det_append(As,Bs,Xs),  G=..[F|Xs],  metacall(G).
conj2list(Conj,[A|Xs]):-nonvar(Conj),Conj=(A,Bs),!,conj2list(Bs,Xs).conj2list(A,[A]).

% answer counting
count_answers(Goal,Count):-
   Ctr=s(0),
   count_goal(Goal,Ctr,Count).
   
count_goal(Goal,Ctr,_):-   
    Goal,
    arg(1,Ctr,I),
    J is I+1,
    change_arg(1,Ctr,J),
  fail.
count_goal(_,Ctr,Count):-
  arg(1,Ctr,Count).
           
% Spying on compiled/dynamic code.
% Note that for compiled code, declarations ":-spy(F/N)." or
% :-spy(all). or :-trace. should be
% _BEFORE_ the clauses in the file they are intended to refer.
  
spy_goal(G):-trace(G).

trace:-spy(all).
notrace:-nospy(all).

spying_strictly(H):-val(spying,H,yes).

spying(H):-val(spying,H,yes),!.
spying(H):-val(spying,all,all), \+ is_builtin(H).

spy(all):-!,bb_let(spying,all,all),
   seeing(F),
   ttyprint(spying_all_predicates_in_file(F)).
spy(F/N):-functor(H,F,N),\+ is_builtin(H),!,bb_let(spying,H,yes).
spy(X):-user_error(unable_to_spy_on,X).

nospy(all):-!,bb_rm(spying,all).
nospy(F/N):-functor(H,F,N),bb_rm(spying,H).


% consulting `interpreted code'

% reconsult/1 variant:
% clean up data areas, consults, makes all static
sconsult(File):-
  clean_up_data_areas(File,F),
  mainconsult(consult0(F),make_all_static).

% reconsult/1 variant:
% cleans up data areas, consults, allowing dynamic recompilation
dconsult(File):-
  clean_up_data_areas(File,F),
  mainconsult(consult0(F),true).

% reconsult/1 variant:
% consults and overwrites old clauses
oconsult(File):-
	find_file(File,F),
        current_db(Current),
        symcat(Current,F,New),
        set_db(New),
        mainconsult(consult0(F),true),
        db_move(New,Current),
        set_db(Current).

% consults with possible dupplication of clauses,
% allows later dynamic recompilation

consult(File):-db_hook,!,x_consult(File).
consult(File):-
  current_db(DB),
  consult(File,DB).

consult(File,Db):-db_consult(File,Db).
 
% allows consulting into an inactive database
% this allows interference with compiled code
db_consult(File,DB):-
	find_file(File,F),
  mainconsult(consult0(F,DB),true).


    
clean_up_data_areas(File,F):-
  find_file(File,F0),
  survive_cleanup(F0,F),
  set_current_user_file(F).

survive_cleanup(F0,F):-nonvar(F0),
        t2rt(F0,Survivor),  % Survivor is now a list on the heap
        restart,     % total cleanup of name-spaces, strings, files etc...
        %call_ifdef(init_io,true),
        rt2t(Survivor,F).   % F is now a valid name

t2rt(V,R):-var(V),!,R=V.
t2rt(X,R):-integer(X),!,R=int(X).
t2rt(X,R):-atomic(X),!,name(X,Xs),R=struct(Xs,[]).
t2rt(T,struct(Fs,Ys)):-T=..[F|Xs],name(F,Fs),l2rl(Xs,Ys).

l2rl([],[]).
l2rl([X|Xs],[Y|Ys]):-t2rt(X,Y),l2rl(Xs,Ys).

rt2t(V,R):-var(V),!,R=V.
rt2t(int(V),R):-!,R=V.
rt2t(struct(Fs,Xs),T):-name(F,Fs),rl2l(Xs,Ys),T=..[F|Ys].

rl2l([],[]).
rl2l([X|Xs],[Y|Ys]):-rt2t(X,Y),rl2l(Xs,Ys).

mainconsult(ConsultStep,CompileStep):-
   statistics(code,[C1,_]),
   ctime(T1),
   ConsultStep,
   ctime(T2),
   CompileStep,
   ctime(T3),
   statistics(code,[C2,_]),
   CT is T2-T1,
   ST is T3-T2,
   C is C2-C1,
   quietmes(time(consulting=CT,quick_compiling=ST,static_space=C)).

consult0(F):-
   current_db(DB),
   consult0(F,DB).

consult0(F,DB):-
	quietmes(consulting(F)),
	see(F),
        consult1(DB),
	seen,
	quietmes(consulted(F)).

consult1(DB):-
	repeat,
		gc_read_clause(C),consult_embedded(C,DB),C=end_of_file,!.
   
consult_included(F,DB):-seeing(G),find_file(F,F0),consult0(F0,DB),see(G).

consult_embedded(':-'(Cmd),DB):-!,consult_cmd(Cmd,DB).
consult_embedded(C,DB):-assert_it(C,DB).

assert_it(end_of_file,_):-!.
assert_it(C,DB):-db_assertz(DB,C).
  
consult_cmd(Cmd):-current_db(DB),consult_cmd(Cmd,DB).

consult_cmd([F],DB):-!,consult_included(F,DB).
consult_cmd(consult(F),DB):-!,consult_included(F,DB).
consult_cmd(oconsult(F),DB):-!,consult_included(F,DB).
consult_cmd(sconsult(F),DB):-!,consult_included(F,DB).
consult_cmd(dconsult(F),DB):-!,consult_included(F,DB).
consult_cmd(reconsult(F),DB):-!,consult_included(F,DB).
consult_cmd(compile(F),DB):-!,consult_included(F,DB).
consult_cmd(scompile(F),DB):-!,consult_included(F,DB).
consult_cmd(mcompile(F),DB):-!,consult_included(F,DB).
consult_cmd(C,DB):-call_ifdef(file_cmd_hook(C,DB),topcall(C)).


/* Mutant version of R.A. O'Keefe's meta-circular interpreter */
/* first slightly modified by Paul Tarau, */
/* later modified beyond recognition and moved to lib.pl :-) */

call_body(Body):-var(Body),!,user_error(unbound_goal,Body).
call_body(Body) :- % compile time execution
   ## expand_call_body(Body,Code),Code. 

is_delphi(P,DP):-val(P,'$delphi',DP).

is_dynamic(H):-db_hook,!,x_is_dynamic(H).
is_dynamic(H):-is_asserted(H).

db_is_dynamic(DB,H):-val(DB,H,_).

db_is_asserted(DB,H):-val(DB,H,_).

%is_asserted(H):- get_asserted(H,_).
is_asserted(H):- current_db(DB),val(DB,H,_).

do_goal(Goal) :-
   is_compiled(Goal),!,
   Goal.
do_goal(Goal):-
%  spying_strictly(Goal),!,
   val(spying,Goal,yes),!,
   spy_goal(Goal).
do_goal(Goal):-
   db_hook,x_is_dynamic(Goal),!,
   x_clause(Goal,Body),
   ## expand_call_body(Body,Code),Code.
do_goal(Goal):-
   get_asserted(Goal,I),!,
   ( I=:=0,dyn_compile(Goal)->Goal
   ; cmemberq((Goal:-Body),I),
     ## expand_call_body(Body,Code),Code
   ).
do_goal(G):-
   functor(G,call,_),!,
   do_apply(G,Goal),
   do_goal(Goal).
%do_goal(Goal):-db_hook,x_is_dynamic(Goal),!,fail.
do_goal(Goal):-is_asserted(Goal),!,fail.
/*
do_goal(Goal):-
   % to enable this you should do something like:
   % host(..remote_host..)=>code(..remote_file..)=>remote_predicate(...).
   %
   call_ifdef(fetch_pred_for(Goal),fail),!,
   asserted(Goal).
*/
do_goal(Undef):-
   delegate_undefined(Undef).

delegate_undefined(Undef):-is_compiled(on_undefined(_)),!,on_undefined(Undef).
delegate_undefined(Undef):-is_dynamic(on_undefined(_)),!,do_goal(on_undefined(Undef)).
delegate_undefined(Undef):-errmes(undefined_predicate_in_metacall,Undef).

do_apply(G,Goal):-
	G=..[_,Closure|Args],
	Closure=..L1,
  append(L1,Args,L2),
  !,
	Goal=..L2.

do_body((Goal,Body), AfterCut, HadCut):-!,
	do_conj(Goal,Body, AfterCut, HadCut).
do_body((Goal;Body), AfterCut, HadCut):-!,
	do_disj(Goal,Body, AfterCut, HadCut).
do_body(!, true, yes):-!.
do_body(Goal, true, no) :- do_goal(Goal).

do_conj(!,AfterCut, AfterCut, yes) :- !.
do_conj(Goal,Body, AfterCut, HadCut) :- 
	call_body(Goal),
	do_body(Body, AfterCut, HadCut).

do_disj((If->Then),Else, AfterCut, HadCut) :-!,
   do_if_then_else(If,Then,Else, AfterCut, HadCut).
do_disj(Disj1,Disj2, AfterCut, HadCut) :-
   do_disj0(Disj1,Disj2, AfterCut, HadCut).

do_if_then_else(If,Then,_, AfterCut, HadCut) :- call_body(If),!,
	do_body(Then, AfterCut, HadCut).
do_if_then_else(_,_,Else, AfterCut, HadCut) :-
	do_body(Else, AfterCut, HadCut).

do_disj0(Disj1,_, AfterCut, HadCut) :-
	do_body(Disj1, AfterCut, HadCut).
do_disj0(_,Disj2, AfterCut, HadCut) :-
	do_body(Disj2, AfterCut, HadCut).



% sandbox security

sandbox:-
  sandbox([
    system/1,
    system/2,
    system0/2,
    unix_fork/1,
    unix_kill/2,
    unix_access/2,
    see_tell_at/2,
    see_tell/2,
    tell_at_end/1,
    tell/1,
    see/1,
    see_at/1,
    open_stream/4,
    iso_open_stream/3
  ]).

sandbox(FNs):-
  member(F/N,FNs),
  functor(P,F,N),
  override(1,P,in_sandbox(_)),
  override(3,P,in_sandbox(_)),
  debugmes(sandboxing(P)),
  fail
; true.

in_sandbox(X):- 
  \+errmes(no_playing_with_fire_in_sandbox,forbiden_predicate_called_on(X)),
  halt.


% ASSERTED CODE LISTERS

listing(catchmarker,_):-!. % hide this
listing(P,N):-functor(H,P,N),
  (
    is_compiled(H)->write('% dyn_compiled: ')
  ; is_asserted(H)->write('% ') 
  ; fail
  ),
  write(P/N),write(':'),nl,
  listing0(H),
  !.
listing(_,_).

list_clause(C):-portray_clause(C).

listing(FN):-db_hook,!,x_listing(FN).
listing(F/N):-!,
  listing(F,N),nl.
listing(P):-
  listing2(P).

listing2(P):-
  for(N,0,255),
  listing(P,N),
  fail.
listing2(_):-nl.

generate_run_time_predicate(P):-
   generate_run_time_predicate(F,N),
   functor(P,F,N).

generate_run_time_predicate(P,N):-db_pred(P,N).
generate_run_time_predicate(P,N):-
	bb_list(Xs),
  current_engine(E), %RISKY
  K1=E/0,K2=P/N,
	bb_element(K1+K2=V,Xs),nonvar(V).


listing:-db_hook,!,x_listing.
listing:-
  generate_run_time_predicate(P,N),
  listing(P,N),
  fail.
listing.

listing0(H):-
  clause0(H,B),pp_clause((H:-B)),fail
; nl.

db_listing(Db):-db_listing0(Db,_H).

db_listing(Db,F/N):-
  functor(H,F,N),
  db_listing0(Db,H).

db_listing0(Db,H):-
  foreach(
    db_clause(Db,H,B),
    pp_clause((H:-B))
  ),
  nl.
  
help(Name):-apropos(Name).

apropos(Name):-
  write('use info/1 to get a description of a predicate'),nl,
  findall(FN,apropos(Name,FN),FNs),
  sort(FNs,Xs),
  member(X,Xs),
    write(X),nl,
  fail.
apropos(_).

apropos(Name,F/N-[Prop|Info]):-
  name(Name,UnKnown),
  predicate_property(H,Prop),functor(H,F,N),
  name(F,Known),
  near_match(Known,UnKnown),
  (bp_info(F/N,I),I\==''->
    ( I=Text-_ ->Info=[Text]
    ; Info=[I]
    )
  ; Info=[]
  ).

near_match(Known,Known):-!.
near_match(Known,UnKnown):-append3(_,UnKnown,_,Known),!.
near_match(Known,UnKnown):-append3(A,B,C,Known),append3(A,[_|B],C,UnKnown),!.
%near_match(Known,UnKnown):-near_match(UnKnown,Known).

append3(Xs,Ys,Zs,Us):-
  append(Xs,YsZs,Us),
  append(Ys,Zs,YsZs).

% -------------------------------------------------------------------------
%   File   : INTERP
%   Author : R.A.O'Keefe
%   Updated: 2 March 84
%   Purpose: Meta-circular interpreter for Prolog (tracing version)


% Tracing metainterpreter

trace(Goal) :- tr_body(Goal, 0).

tr_body(Body,_):-var(Body),!,user_error(unbound_goal,Body).
tr_body(Body, Depth) :-
	tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		tab(Depth), write('CUT'), nl,
		tr_body(AfterCut, Depth)
	;   HadCut = no
	).

tr_body((Conj1,Conj2), Depth, AfterCut, HadCut) :- !,
	tr_body(Conj1, Conj2, Depth, AfterCut, HadCut).
tr_body(!, _, true, yes) :- !.
tr_body((Disj1;Disj2), Depth, AfterCut, HadCut) :-!,
	tr_disj1(Disj1, Disj2, Depth, AfterCut, HadCut).
tr_body(true, _, true, no) :- !.
tr_body(Goal, Depth, true, no) :- tr_goal(Goal, Depth).

tr_disj1((If->Then),Else, Depth, AfterCut, HadCut):-!,
	tr_if1(If,Then, Else, Depth, AfterCut, HadCut).
tr_disj1(Disj1,_, Depth, AfterCut, HadCut):-
	tr_body(Disj1, Depth, AfterCut, HadCut).
tr_disj1(_,Disj2, Depth, AfterCut, HadCut) :-
	tr_body(Disj2, Depth, AfterCut, HadCut).

tr_if1(If,Then, Else, Depth, AfterCut, HadCut):-
  call_body(If)->tr_body(Then, Depth, AfterCut, HadCut)
;              tr_body(Else, Depth, AfterCut, HadCut).

tr_body(!, AfterCut, _, AfterCut, yes) :- !.
tr_body((A,B), Conj, Depth, AfterCut, HadCut) :- !,
	tr_body(A, (B,Conj), Depth, AfterCut, HadCut).
tr_body((Disj1;Disj2), Conj, Depth, AfterCut, HadCut) :- !,
        tr_disj2(Disj1,Disj2, Conj, Depth, AfterCut, HadCut).
tr_body(true, Body, Depth, AfterCut, HadCut) :- !,
	tr_body(Body, Depth, AfterCut, HadCut).
tr_body(Goal, Body, Depth, AfterCut, HadCut) :-
	tr_goal(Goal, Depth),
	tr_body(Body, Depth, AfterCut, HadCut).

tr_disj2((If->Then),Else, Conj, Depth, AfterCut, HadCut) :-!,
        tr_if2(If,Then, Else, Conj, Depth, AfterCut, HadCut).
tr_disj2(Disj1,_, Conj, Depth, AfterCut, HadCut) :-
	tr_body(Disj1, Conj, Depth, AfterCut, HadCut).
tr_disj2(_,Disj2, Conj, Depth, AfterCut, HadCut) :-
	tr_body(Disj2, Conj, Depth, AfterCut, HadCut).

tr_if2(If,Then, Else, Conj, Depth, AfterCut, HadCut):-
  call_body(If)->tr_body(Then, Conj, Depth, AfterCut, HadCut)
;              tr_body(Else, Conj, Depth, AfterCut, HadCut).

tr_goal(call(Goal), Depth) :- !,tr_body(Goal, Depth).
tr_goal(\+(Goal), Depth) :- tr_body(Goal, Depth), !, fail.
tr_goal(\+(_), _) :- !.
tr_goal(Goal, Depth) :-
	(  tab(Depth), write('Call: '), tprint(Goal), fail
        ;  is_interactive, write(' <ENTER=call, ;=trace, h=help>: '), fail
        ;  true
	),
        (  is_interactive->get_code(X),(X=:=10->true;get_code(_))
        ;  X=32,nl
        ),
        ( X=:=10,\+(spying(Goal))-> !,
                   Goal
        ; [X]="l"->functor(Goal,F,N),nl,listing(F/N),!,
                   tr_goal(Goal, Depth)
        ; member(X,"aq")->nl,abort
        ; [X]="p"->nl,(escape_goal(Depth)->nl;nl),!,
                   tr_goal(Goal, Depth)
        ; member(X,"h?")->nl,trace_help,!,tr_goal(Goal, Depth)
        ; [X]="t"->!,nl
        ; [X]="f"->!,nl,fail
        ; Depth1 is 1+Depth,tr_call(Goal, Depth1)
	),
	( tab(Depth), write('Exit: '), tprint(Goal), nl, fail
	; true
	; tab(Depth), write('Redo: '), tprint(Goal), nl, fail
	)
; 	tab(Depth), write('Fail: '), tprint(Goal), nl, fail.

escape_goal(Depth):-write(Depth),write(' ?- '),read(Goal),
  (
    var(Goal)->fail
    ; Goal=X^G->G,write(X),nl,fail
    ; Goal,write(Goal),fail
  ).

trace_help:-
  nl,
  member(C-Mes,
   [
     'ENTER'-'call without tracing',
     l-listing,
     'q,a'-abort,
     p-'toplevel Prolog query',
     t-'succeed, but do not call this goal',
     f-'fail, and do not call this goal',
     k-'keep goal for futher inspection',
     s-'show saved goal instances',
     h-help,
     (';')-'continue (default)'
   ]
  ),
  write(C),write(' ==> '),write(Mes),nl,
  fail
; nl.

tprint(G):-trim_term(5,G,NewG),print(NewG),fail.
tprint(_).

templ_do_goal(Goal,Body,clause(Goal,Body)):-is_asserted(Goal).

tr_call(bagof(X,Y,Z), Depth) :- !,	% include these 4 lines if you
	bagof(X, tr_body(Y,Depth), Z).	% really want them, but they do
tr_call(setof(X,Y,Z), Depth) :- !,	% slow things down a bit.
	setof(X, tr_body(Y,Depth), Z).
tr_call(findall(X,Y,Z), Depth) :- !,
	findall(X, tr_body(Y,Depth), Z).
tr_call(Goal,Depth):-
        templ_do_goal(Goal,Body,Clause),
        !,
	functor(Clause,Mes,_),
	tab(Depth),functor(Goal,F,N),
        write('!!! '),write(Mes),write(': '),write(F/N),nl,
	Clause,
	tr_body(Body, Depth, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		tab(Depth), write('CUT'), nl,
		tr_body(AfterCut, Depth)
	;   HadCut = no
	).
tr_call(Goal, Depth) :-
	is_compiled(Goal), % <--- checks for compiled predicate
	!, tab(Depth),functor(Goal,F,N),write('!!! compiled'(F/N)),nl,
	Goal.
tr_call(G, Depth):-functor(G,call,_),!,
	do_apply(G,Goal),
	tr_call(Goal,Depth).
tr_call(Undef):-
  delegate_undefined(Undef).

debug(F):- reconsult(F).

% defaults: 
%  overridable with assumptions/assertions/compiled facts

show_defaults:-
  current_predicate(F,P),
  name(F,Xs),
  member(Prefix,
    ["default_",              "detect_",   % "get_",
     "prolog:default_",   "prolog:detect_" % "prolog:get_"
    ]
  ),
  ( append(Prefix,_,Xs)->true;fail),
  call_ifdef(P,
     (debugmes(undefined_default(P)),fail)
  ),
  write(P),nl,
  fail.
show_defaults.

% misc

max(X,Y,Z):-X>Y,!,Z=X.
max(_,Y,Y).

min(X,Y,Z):-X=<Y,!,Z=X.
min(_,Y,Y).

add_true((H:-B),(H:-B)):-!.
add_true(H,(H:-true)).

make_cmd(Cs,Cmd):-
  make_cmd0(Cs,Xs),
  atom_codes(Cmd,Xs).

insert_spaces([],[]).
insert_spaces([X|Xs],[' ',X|Ys]):-
  insert_spaces(Xs,Ys).

make_spaced_cmd([],'').
make_spaced_cmd([X|Xs],C):-insert_spaces(Xs,Ys),make_cmd([X|Ys],C).

make_cmd0(Cs,Xs):-
  findall(X,char_in_cmd(X,Cs),Xs).

char_in_cmd(X,Cs):-member(C,Cs),listify(C,Xs),member(X,Xs).

listify([X|Xs],[X|Xs]):-!.
listify([],[]):-!.
listify(X,L):-atom_codes(X,L).

% compiler wrapper

% loads *.wam runtime bytecode files

load(F):-
  survive_cleanup(F,NewF),
  stat_time(load_file(NewF)),
  abort.

stat_time(Goal):-
  ctime(T1),
  stat_compile(Goal),!,
  ctime(T2),
  T is T2-T1,
  quietmes(processing_time(T)).
stat_time(Goal):-
  ttyprint('compilation has been aborted for'(Goal)),
  restart.

stat_compile(Goal):-
        statistics(code,[C1,_]),
        statistics(strings,[Z1,_]),
        statistics(symbols,[S1,_]),
        statistics(htable,[H1,_]),
        Goal,
        statistics(code,[C2,_]),C is C2-C1,
        statistics(strings,[Z2,_]),Z is Z2-Z1,
        statistics(symbols,[S2,_]),S is S2-S1,
        statistics(htable,[H2,_]),H is H2-H1,
        Total is C+Z+S+H,
        quietmes(bytes_used(
                 code(C),strings(Z),symbols(S),htable(H),
                 total(Total))).

load_file(F0):-
  to_list(F0,Fs),
  member(F,Fs),
    find_file_ext(F,".wam",InFile),
    load0(InFile),
  fail.
load_file(_):-
  call_ifdef(
    terminate_file(mem,1),
    terminate_load
  ).

terminate_load:-fail,errmes(unexpected, terminate_load).
terminate_load:-
   bu_ctr(n_nop,Nop), Clause is Nop+1,Firstarg is Nop+2,
   new_name(mem,Dummy),
   add_instr(Clause,0,Dummy,1),
   add_instr(Firstarg,3,0,0),
   add_instr(17,0,true,1), % execute_?
   add_instr(0,0,mem,0).

/* CODE PORTED FROM Jinni */

codes2term(Codes,Term):-codes2term(Codes,Term,_).

codes2term(Codes,Term,Vars):-
  nonvar(Codes),
  to_tokens(Codes,Tokens),
  parser(Tokens,Term,Vars),
  !.
codes2term(Codes,_,_):-
  atom_codes(A,Codes),
  errmes(syntax_error,parsing=A).
  
% Horn clause + disj + if-then-else parser

parser(Tokens,Term,Vars):-
  det_append([lpar|Tokens],[rpar,eoc],NewTokens),
  run_parser(NewTokens,Term,Dict),
  collect_vars(Dict,Vars).

run_parser(Tokens,Term,Dict):-
  top_term(eoc,Term,Dict,Tokens,[]).

set_dcg_state(Xs,_,Xs).

get_dcg_state(Xs,Xs,Xs).

dcg_look_ahead(Next,[Next|Xs],[Next|Xs]).

dcg_look_ahead(First,Second,[First,Second|Xs],[First,Second|Xs]).

@X-->[X].

top_term(End,HB,D) --> term(H,D), body(End,H,HB,D).

match_end(End)--> @X, {X==End}.

test_end(End)--> get_dcg_state(Xs),{nonvar(Xs),Xs=[End|_]}.

body(End,H,':-'(H,Bs),D) --> @iff,conj_seq(Bs,D),match_end(End).
body(End,H,H,_) --> match_end(End).

check_var(K,V,tree(K,V,_,_)):-!.
check_var(K,V,tree(LK,_,L,_)):-K@<LK,!,check_var(K,V,L).
check_var(K,V,tree(_,_,_,R)):-
  check_var(K,V,R).

collect_vars(Dict,Vars):-collect_vars(Dict,Vars,[]).

collect_vars(Dict)--> {var(Dict)}, !.
collect_vars(tree(K,V,L,R)) --> [K=V],collect_vars(L),collect_vars(R).

term(V,D) --> @var(T),!,{check_var(T,V,D)}.
term(N,_) --> @num(Is),!,float_or_int(Is,N).
term(T,D) --> @const(SignOrF),!,neg_num_or_args(SignOrF,T,D).
term(L,D) --> @lbra,!,term_list(L,D).
term(S,D) --> @lpar,spec_term(S,D).

float_or_int(Is,N)--> @eoc,@num(Ds),!,{nums2float('+',Is,Ds,N)}.
float_or_int(Ns,N)--> {num2int('+',Ns,N)}.

neg_num_or_args(Sign,T,_) --> {is_sign(Sign)},@num(Is),@eoc,@num(Ds),!,{nums2float(Sign,Is,Ds,T)}.
neg_num_or_args(Sign,T,_) --> {is_sign(Sign)},@num(I),!,{num2int(Sign,I,T)}.
neg_num_or_args(F,T,D) --> args(Xs,D),{T=..[F|Xs]}.

is_sign('-').
is_sign('+').

num2int(Sign,Ns,N):-to_integer(Ns,N0),!,apply_sign(Sign,N0,N).
num2int(Sign,Is,F):-nums2float(Sign,Is,".0",F).

to_integer(Is,I):-name(I,Is),check_integer(I,Is).

check_integer(0,_):-!.
check_integer(I,Is):-name(I,Js),Js=Is.

to_float(StringNum,Float):-input_float(0,StringNum,0,Float).

nums2float(Sign,Is,Ds,N):-
  [Dot]=".",
  det_append(Is,[Dot|Ds],Cs),
  atom_codes(StringNum,Cs),
  to_float(StringNum,Float),
  apply_sign(Sign,Float,N).

apply_sign('+',N0,N):-N is 0+N0.
apply_sign('-',N0,N):-N is 0-N0.

token2term(num,Xs,Token):- !,Token=..[num,Xs].
token2term(F,Xs,Token):-name(N,Xs),Token=..[F,N].

args([T|Ts],D) --> @lpar,term(T,D),arglist(Ts,D).
args([],_)-->[].

arglist([],_) --> @rpar.
arglist([T|Ts],D) --> @comma,term(T,D),arglist(Ts,D).

term_list([],_)--> @rbra,!.
term_list([T|Ts],D) --> term(T,D),term_list_cont(Ts,D).

term_list_cont([T|Ts],D)--> @comma, term(T,D),term_list_cont(Ts,D).
term_list_cont(T,D)--> @bar, term(T,D), @rbra.
term_list_cont([],_)--> @rbra.

conj_seq(Xs,D)-->seq(comma,',',term,Xs,_End,D).

seq(InCons,OutCons,Inner,Bs,End,D)--> 
  dcg_call(Inner,B,D),
  cons_args(InCons,OutCons,Inner,B,Bs,End,D).

cons_args(InCons,OutCons,Inner,G,T,End,D) --> @InCons, !, 
  {T=..[OutCons,G,Gs]},
  dcg_call(Inner,NewG,D),
  cons_args(InCons,OutCons,Inner,NewG,Gs,End,D).   
cons_args(_,_,_,G,G,End,_) --> test_end(End). 

spec_term(Xs,D)--> disj_seq(Xs,D),@rpar.
spec_term(Xs,D)--> top_term(rpar,Xs,D).

disj_seq(Xs,D)-->seq(disj,';',disj_term,Xs,_,D).

disj_term(T,D)--> seq(comma,',',term,Xs,End,D),disj_term_cont(End,Xs,T,D).

disj_term_cont(if,Xs,(Xs->Then),D)--> @if, seq(comma,',',term,Then,_,D).
disj_term_cont(disj,Xs,Xs,_)-->[].
disj_term_cont(rpar,Xs,Xs,_)-->[].

% NL tokenizer

to_words(Codes,_Ws):-var(Codes),!,errmes(nonvar_expected,to_words(Codes)).
to_words(Codes,Ws):-to_word_codes(Codes,Css),codes2names(Css,Ws).

codes2names([],[]).
codes2names([Cs|Css],[N|Ns]):-name(N,Cs),codes2names(Css,Ns).

to_word_codes(Cs,Css):-nwords(Css,[32|Cs],End),!,End=[].

nwords(Ws)-->star(nword,Ws),space.

nword(W)-->space,stoken(W).

stoken(Xs)-->plus(is_digit,Xs),!.
stoken(Xs)-->plus(is_letter,Xs),!.
stoken([C])--> @C.

to_tokens(Cs,Ws):-words(Ws,[32|Cs],End),!,End=[].

words(Ws)-->star(word,Ws),space.

word(W)-->space,token(W).

token(lpar)-->c("(").
token(rpar)-->c(")").
token(lbra)-->c("[").
token(rbra)-->c("]").
token(bar)-->c("|").
token(comma)-->c(",").
token(disj)-->c(";").
token(if)-->c("->").
token(eoc)-->c(".").
token(iff)-->c(":-").
token(Token)-->token(F,Xs),{token2term(F,Xs,Token)}.

token(num,Xs) --> plus(is_digit,Xs).
token(const,Xs) --> one(is_punct,Xs).
token(F,Xs) --> @X,sym(X,F,Xs).

sym(Q,const,Xs)-->{is_quote(Q)},!,quoted_with(Q,Xs),@Q.
sym(X,const,[X|Xs])-->{is_min(X)},!,star(is_letter,Xs).
sym(X,var,[X|Xs])-->{is_maj(X)},star(is_letter,Xs).

c([])-->[].
c([X|Xs]) --> @X,c(Xs).

quoted_with(Q,[X|Xs]) --> @X,{Q\==X},!,quoted_with(Q,Xs).
quoted_with(Q,[Q|Xs]) --> @Q,@Q,!,quoted_with(Q,Xs).
quoted_with(_,[])-->[].

space-->star(is_space,_).

% regexp tools with  AGs + high order

one(F,[X])--> dcg_call(F,X).

star(F,[X|Xs])--> dcg_call(F,X),!,star(F,Xs).
star(_,[])-->[].

plus(F,[X|Xs])--> dcg_call(F,X),star(F,Xs).

dcg_call(F,X,D,S1,S2):-FX=..[F,X,D,S1,S2],topcall(FX). %,println(called=FX).

dcg_call(F,X,S1,S2):-FX=..[F,X,S1,S2],topcall(FX). %,println(called=FX).

is_quote(X):-[X]="'".

% recognizers

is_space(X)--> @X, {member(X,[32,7,9,10,13])}.

is_letter(X)--> @X, {is_an(X)}.

is_punct(X)--> @X, {(is_spec(X);member(X,"!;`""[]{}*"))}.

is_digit(X)--> @X, {is_num(X)}.




% sort, adapted from public domain code written by R.A. O'Keefe
% use merge_sort(<,_,_) if you do not want duplications eliminated
% use merge_sort(>,_,_) for descending order

sort(L1,L2):-merge_sort(<,L1,DupL),remdup(DupL,L2).

msort(L1,L2):-merge_sort(<,L1,L2).

remdup([],[]):-!.
remdup([X,Y|Xs],Ys):-compare(=,X,Y),!,remdup([X|Xs],Ys).
remdup([X|Xs],[X|Ys]):-remdup(Xs,Ys).
      
merge_sort(Rel, L,S ):-
	length(L,N),
	merge_sort1(N, Rel, L,S,[] ).

merge_sort1( 0,_,L,[],L ):-!.
merge_sort1( 1,_,[X|L],[X],L ):-!.
merge_sort1( N,Rel,L,S,R ):-	% N >= 2
	N1 is N >> 1,	N2 is N-N1,
	merge_sort1( N1,Rel,L,S1,R1),	
	merge_sort1( N2,Rel,R1,S2,R),
	merge_2( S2,Rel,S1,S ).

merge_2([],_,S,S ):-!.
merge_2([X|L1],Rel,[Y|L2],[X|L] ):-compare(Rel,X,Y),!,
	merge_2(L1,Rel,[Y|L2],L ).
merge_2(L1,Rel,[Y|L2],[Y|L] ):-
	merge_2(L2,Rel,L1,L ).

%   Keysorting.  Adapted by Mats Carlsson from R.O'Keefe's code, 
%		but uses recursion instead of an auxiliary stack.  
%		Takes care to check validity of arguments.
%   Could be speed up if there were an inline keycompare/3.

ksort(List, Sorted) :-
	keysort(List, -1, S, []), !,
	Sorted = S.
ksort(X, Y):-user_error('illegal_arguments',keysort(X,Y)).

keysort([Head|Tail], Lim, Sorted, Rest) :- !,
	nonvar(Head),
	Head = _-_,
	Qh = [Head|_],
	samkr(Tail, Qh, Qh, Run, Rest0),
	keysort(Rest0, 1, Lim, Run, Sorted, Rest).
keysort(Rest, _, [], Rest).

keysort([Head|Tail], J, Lim, Run0, Sorted, Rest) :-
	J =\= Lim, !,
	nonvar(Head),
	Head = _-_,
	Qh = [Head|_],
	samkr(Tail, Qh, Qh, Run1, Rest0),
	keysort(Rest0, 1, J, Run1, Run2, Rest1),
	keymerge(Run0, Run2, Run),
	K is J+J,
	keysort(Rest1, K, Lim, Run, Sorted, Rest).
keysort(Rest, _, _, Sorted, Sorted, Rest).

samkr([Hd|Tail], QH, QT, Run, Rest) :-
	nonvar(Hd),
	Hd = H-_,
	QT = [Q-_|QT2], 
	Q @=< H, !,
	QT2 = [Hd|_],
	samkr(Tail, QH, QT2, Run, Rest).
samkr([Hd|Tail], QH, QT, Run, Rest) :-
	nonvar(Hd),
	Hd = H-_,
	QH = [Q-_|_],
	H @< Q, !,
	samkr(Tail, [Hd|QH], QT, Run, Rest).
samkr(Rest, Run, [_], Run, Rest).

% keymerge(+List, +List, -List).
keymerge([], L2, Out) :- !,
	Out = L2.
keymerge([H1|T1], L2, Out) :-	
	L2 = [K2-_|_],
	H1 = K1-_,
	K1 @=< K2, !,
	Out = [H1|Out1],
	keymerge(T1, L2, Out1).
keymerge(L1, [H2|L2], Out) :- !,
	Out = [H2|Out1],
	keymerge(L1, L2, Out1).
keymerge(List, _, List).

% -------------------- ---------------------

%   File   : SETOF.PL
%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: define setof/3, bagof/3, findall/3, and findall/4
%   Needs  : Not.Pl

% Adapted by Paul Tarau for BinProlog: uses a heap based findall/3.
% therefore some database hacking predicates have been removed.
% Updated: 19 July 1992

/*  This file defines two predicates which act like setof/3 and bagof/3.
    I have seen the code for these routines in Dec-10 and in C-Prolog,
    but I no longer recall it, and this code was independently derived
    in 1982 by me and me alone.

    Most of the complication comes from trying to cope with free variables
    in the Filter; these definitions actually enumerate all the solutions,
    then group together those with the same bindings for the free variables.
    There must be a better way of doing this.  I do not claim any virtue for
    this code other than the virtue of working.  In fact there is a subtle
    bug: if setof/bagof occurs as a data structure in the Generator it will
    be mistaken for a call, and free variables treated wrongly.  Given the
    current nature of Prolog, there is no way of telling a call from a data
    structure, and since nested calls are FAR more likely than use as a
    data structure, we just put up with the latter being wrong.  The same
    applies to negation.

    Would anyone incorporating this in their Prolog system please credit
    both me and David Warren;  he thought up the definitions, and my
    implementation may owe more to subconscious memory of his than I like
    to think.  At least this ought to put a stop to fraudulent claims to
    having bagof, by replacing them with genuine claims.

    Thanks to Dave Bowen for pointing out an amazingly obscure bug: if
    the Template was a variable and the Generator never bound it at all
    you got a very strange answer!  Now fixed, at a price.

	bagof/3,		%   Like bagof (Dec-10 manual p52)
	setof/3.		%   Like setof (Dec-10 manual p51)

:- mode
	bagof(+,+,?),
	concordant_subset(+,+,-),
	concordant_subset(+,+,-,-),
	concordant_subset(+,+,+,+,-),
	replace_key_variables(+,+,+),
	setof(+,+,?).

%   setof(Template, Generator, Set)
%   finds the Set of instances of the Template satisfying the Generator..
%   The set is in ascending order (see compare/3 for a definition of
%   this order) without duplicates, and is non-empty.  If there are
%   no solutions, setof fails.  setof may succeed more than one way,
%   binding free variables in the Generator to different values.  This
%   predicate is defined on p51 of the Dec-10 Prolog manual.
*/

setof(Template, Filter, Set) :-
	bagof(Template, Filter, Bag),
	sort(Bag, Set).


%   bagof(Template, Generator, Bag)
%   finds all the instances of the Template produced by the Generator,
%   and returns them in the Bag in they order in which they were found.
%   If the Generator contains free variables which are not bound in the
%   Template, it assumes that this is like any other Prolog question
%   and that you want bindings for those variables.  (You can tell it
%   not to bother by using existential quantifiers.)
%   bagof records three things under the key '.':
%	the end-of-bag marker	       -
%	terms with no free variables   -Term
%	terms with free variables   Key-Term
%   The key '.' was chosen on the grounds that most people are unlikely
%   to realise that you can use it at all, another good key might be ''.
%   The original data base is restored after this call, so that setof
%   and bagof can be nested.  If the Generator smashes the data base
%   you are asking for trouble and will probably get it.
%   The second clause is basically just findall, which of course works in
%   the common case when there are no free variables.

bagof(Template, Generator, Bag) :-
	free_variables(Generator, Template, [], Vars,1),
	Vars \== [],
	!,
	Key =.. [.|Vars],
	functor(Key, ., N),
	findall(Key-Template,Generator,Recorded),
	replace_instance(Recorded, Key, N, OmniumGatherum),
	keysort(OmniumGatherum, Gamut), !,
	concordant_subset(Gamut, Key, Answer),
	Bag = Answer.
bagof(Template, Generator, [B|Bag]) :-
	findall(Template,Generator,[B|Bag]).

_^Goal:-Goal.

replace_instance([], _, _, []) :- !.
replace_instance([NewKey-Term|Xs], Key, NVars, [NewKey-Term|OldBag]) :-
		replace_key_variables(NVars, Key, NewKey), !,
		replace_instance(Xs,Key, NVars, OldBag).

replace_key_variables(0, _, _) :- !.
replace_key_variables(N, OldKey, NewKey) :-
	arg(N, NewKey, Arg),
	nonvar(Arg), !,
	M is N-1,
	replace_key_variables(M, OldKey, NewKey).
replace_key_variables(N, OldKey, NewKey) :-
	arg(N, OldKey, OldVar),
	arg(N, NewKey, OldVar),
	M is N-1,
	replace_key_variables(M, OldKey, NewKey).

keygroup(KsVs,K,Vs):-
  keysort(KsVs,Sorted),
  concordant_subset(Sorted,K,Vs).

%   concordant_subset([Key-Val list], Key, [Val list]).
%   takes a list of Key-Val pairs which has been keysorted to bring
%   all the identical keys together, and enumerates each different
%   Key and the corresponding lists of values.

concordant_subset([Key-Val|Rest], Clavis, Answer) :-
	concordant_subset(Rest, Key, List, More),
	concordant_subset(More, Key, [Val|List], Clavis, Answer).


%   concordant_subset(Rest, Key, List, More)
%   strips off all the Key-Val pairs from the from of Rest,
%   putting the Val elements into List, and returning the
%   left-over pairs, if any, as More.

concordant_subset([Key-Val|Rest], Clavis, [Val|List], More) :-
	Key == Clavis,
	!,
	concordant_subset(Rest, Clavis, List, More).
concordant_subset(More, _, [], More).


%   concordant_subset/5 tries the current subset, and if that
%   doesn't work if backs up and tries the next subset.  The
%   first clause is there to save a choice point when this is
%   the last possible subset.

concordant_subset([],   Key, Subset, Key, Subset) :- !.
concordant_subset(_,    Key, Subset, Key, Subset).
concordant_subset(More, _,   _,   Clavis, Answer) :-
	concordant_subset(More, Clavis, Answer).


% default behaviour changed: quantification is intended to be lexical and
% not dynamic as this code supposes. Strange things tend to happen
% otherwise, when, for instance, terms containing ^ are passed out of 
% their scope.
% -- Paul Tarau Thu Sep 14 10:45:09 ADT 1995

% 0 disables use of explicit_binding, 1 enables them
% setof stuff still uses 1, that's closer to it's usual implementation
free_variables(A,B,C,D) :- free_variables(A,B,C,D,0). 

% ---extracted from: not.pl --------------------%

%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: "suspicious" negation 

%   In order to handle variables properly, we have to find all the 
%   universally quantified variables in the Generator.  All variables
%   as yet unbound are universally quantified, unless
%	a)  they occur in the template
%	b)  they are bound by X^P, setof, or bagof
%   free_variables(Generator, Template, OldList, NewList,CheckBindings=0,1)
%   finds this set, using OldList as an accumulator.

free_variables(Term, Bound, VarList, [Term|VarList],_) :-
	var(Term),
	term_is_free_of(Bound, Term),
	list_is_free_of(VarList, Term),
	!.
free_variables(Term, _, VarList, VarList,_) :-
	var(Term),
	!.
free_variables(Term, Bound, OldList, NewList,B) :- B=:=1,
	explicit_binding(Term, Bound, NewTerm, NewBound),
	!,
	free_variables(NewTerm, NewBound, OldList, NewList,B).
free_variables(Term, Bound, OldList, NewList,B) :-
	functor(Term, _, N),
	free_variables(N, Term, Bound, OldList, NewList,B).

free_variables(0,    _,     _, VarList, VarList,_) :- !.
free_variables(N, Term, Bound, OldList, NewList,B) :-
	arg(N, Term, Argument),
	free_variables(Argument, Bound, OldList, MidList,B),
	M is N-1, !,
	free_variables(M, Term, Bound, MidList, NewList,B).

%   explicit_binding checks for goals known to existentially quantify
%   one or more variables.  In particular \+ is quite common.

explicit_binding(\+ _,	               Bound, fail,	Bound    ).
explicit_binding(not(_),	       Bound, fail,	Bound	 ).
explicit_binding(Var^Goal,	       Bound, Goal,	Bound+Var).
explicit_binding(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var).
explicit_binding(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var).

term_is_free_of(Term, Var) :-
	var(Term), !,
	Term \== Var.
term_is_free_of(Term, Var) :-
	functor(Term, _, N),
	term_is_free_of(N, Term, Var).

term_is_free_of(0, _, _) :- !.
term_is_free_of(N, Term, Var) :-
	arg(N, Term, Argument),
	term_is_free_of(Argument, Var),
	M is N-1, !,
	term_is_free_of(M, Term, Var).

list_is_free_of([], _).
list_is_free_of([Head|Tail], Var) :-
	Head \== Var,
	list_is_free_of(Tail, Var).

keysort(L,S):-ksort(L,S).


% for compatibility with various prologs

% SOUND negation -> replaced with (I think) an improved one

% REPLACED:
% not(X):-ground(X),!, \+ X.
% not(X):-user_error('should be ground',not(X)).

% I see no reason to prohibit free variables when the negation succeeds
% as no future bindings can change the logical meaning in this case.
% the case of failure is (of course) different

not(X):- \+ X,!.
not(X):-ground(X),!,fail.
not(X):-user_error('should be ground',not(X)).

vars_of(Term,Vars):-free_variables(Term,[],[],Vars,0).
