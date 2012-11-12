% ## predefined opcodes: see global.h
% regenerate:-[oper]=>(go,reboot).

go:-go(66).

/* ADD NEW BUILTINS HERE */

% simple inline builtins of arity 0,1 with no returned value

b0(fail/0,simple,in_body,'always fails').
b0(cwrite/1,simple,in_body,'basic but quick C-version of write/1').
b0(cnl/0,simple,in_body,'writes a new line').
b0(var/1,simple,in_body,'true if currently an unbound variable').
b0(nonvar/1,simple,in_body,'true if currently instantiated').
b0(integer/1,simple,in_body,'true if an integer').
b0(atomic/1,simple,in_body,'true if an integer or symbolic constant').
b0(is_compiled/1,simple,in_body,'true if head of a compiled predicate').

% builtin inline functions of arity >= 1 f(i,....,i,optional:o)

b0((+)/3,arith(1),in_body,'add'- x(10,3,_)).
b0((-)/(3),arith(1),in_body,'subtract'- x(10,3,_)).
b0((*)/3,arith(1),in_body,'multiply'- x(10,3,_)).
b0(mod/3,arith(1),in_body,'modulo'- x(10,3,_)).

b0('//'/3,arith(1),in_body,'integer division'- x(10,3,_)).
b0((/)/3,arith(1),in_body,'division'- x(10,3,_)).

b0(random/1,arith(1),in_body,'returns a random integer'-x(_)).

b0(get0/1,arith(1),in_body,'reads a char as an ascii code').

b0(put/1,arith(0),in_body,'writes and ascii code as a char'-x(99)).

b0(less/2,arith(0),in_body,'arithmetic comparison').
b0(greater/2,arith(0),in_body,'arithmetic comparison').
b0(less_eq/2,arith(0),in_body,'arithmetic comparison').
b0(greater_eq/2,arith(0),in_body,'arithmetic comparison').
b0(arith_eq/2,arith(0),in_body,'arithmetic comparison').
b0(arith_dif/2,arith(0),in_body,'arithmetic comparison').

b0((<<)/3,arith(1),in_body,'left shifts arg 1 by arg 2 bits'-x(1,5,_)).
b0((>>)/3,arith(1),in_body,'right shifts arg 1 by arg 2 bits'-x(16,2,_)).
b0((/\)/3,arith(1),in_body,'bitwise AND'-x(1,2,_)).
b0((\/)/3,arith(1),in_body,'bitwise OR'-x(1,2,_)).
b0((#)/3,arith(1),in_body,'bitwise XOR'-x(1,2,_)).
b0((\)/3,arith(1),in_body,'bitwise or of first arg with bitwise complement of second'-x(0,2,_)). % complement

b0(compare0/3,arith(1),in_body,'').
b0(arg/3,arith(1),in_body,
   'arg(I,T,X) extracts arg I of term T to be unified with X'-
   x(2,f(a,b),_)
 ).
%%
b0(setarg/3,arith(0),in_body,'backtrackable: setarg(I,T,X) replaces arg I of T with X'-x(2,f(a,b),c)).
b0(change_arg/3,arith(0),in_body,
   'destructive: change_arg(I,T,X) replaces arg I of T with X'-
   x(2,f(a,b),c)
 ).

b0(def/3,arith(0),in_body,'').
b0(rm/2,arith(0),in_body,'').
b0(set/3,arith(0),in_body,'').
b0(val/3,arith(1),in_body,'').

b0(lval/3,arith(1),in_body,'backtrackable: lval(K1,K2,V) associates V to keys K1 and K2'-x(a,b,f(_))).

b0(symcat/3,arith(1),in_body,
  'makes new identifier from arg 1 and arg 2'-
  [x(a,b,_),x(a,1,_)]
).

% C-ification of frequently used builtins ends here
% it is not clear that for the remaining ones it is a good idea to do it

b0(namecat/4,arith(1),in_body,'concatenates 3 names'-x(a,':',b,_)).

b0(deep_hash/4,arith(1),in_body,
  '(Key,Depth,Mod) computes hashvalue of Key modulo Mod, up to max recursion Depth'-[x(f(a),5,0,_),x(f(b),1,32,_)]).
b0(gval/2,arith(1),in_body,'').
b0(hval/2,arith(1),in_body,'').
b0(tval/3,arith(1),in_body,'').
b0(tlet/3,arith(0),in_body,'').
b0(get_asserted/2,arith(1),in_body,'').

b0(array_set/3,arith(0),in_body,
  'sets array element').
b0(array_get0/3,arith(1),in_body,
  'gets  array element').
b0(array_get/3,arith(1),in_body,
  'gets and dereferences array element').
b0(make_array/2,arith(1),in_body,
  'creates an array').
b0(destroy_array/1,arith(0),in_body,
  'frees an array').

b0(vget_int0/2,arith(1),in_body,
  'gets from arg 1 - an int* C variable - to a 28 bit int').
b0(vset_int0/2,arith(0),in_body,
  'sets arg 1 - an int* C variable - to a 28 bit int').

b0(addq0/2,arith(0),in_body,'').
b0(pushq0/2,arith(0),in_body,'').
b0(popq0/2,arith(1),in_body,'').

b0(dcg_connect/1,arith(1),in_body,
   'handles a terminal symbol in HAGs, as [a] in DCGs').
b0(list2term/2,arith(1),in_body,'').
b0(term2list/4,arith(1),in_body,'').

b0(call_external/3,arith(1),in_body,
  'args: StringToStringFunctionAddress,InputChars,OuputChars - calls a C function').

b0(add_instr/4,arith(0),in_body,'').
b0(det_append0/3,arith(1),in_body,'').

b0(copy_term/3,arith(1),in_body,'').
b0(unify_to/2,arith(1),in_body,'').
b0(bb_list0/3,arith(1),in_body,'').

b0(older_file/2,arith(0),in_body,
  'true if arg 1 is a file older than arg 2').
b0(seeing_telling/2,arith(1),in_body,''). % seeing=0,telling=1
b0(see_tell/2,arith(0),in_body,''). % see=0,tell=1
b0(seen_told/1,arith(0),in_body,''). % seen=0,told=1,flush=2
b0(seeing_telling_at/2,arith(1),in_body,''). % seeing_at=0,telling_at=1
b0(see_tell_at/2,arith(0),in_body,''). % see_at=0,tell_at=1

b0(string_op/3,arith(1),in_body,''). % was: swrite0=1, cstring_to_chars=2

b0(op0/3,arith(0),in_body,''). 

b0(term_append/3,arith(1),in_body,
  'efficiently concatenates 2 terms'-x(f(a,b),g(c,d),_)).

b0(float_fun2/4,arith(1),in_body,'').
b0(float_fun/3,arith(1),in_body,'').
b0(input_float/4,arith(1),in_body,'').

b0(strip_cont0/2,arith(1),in_body,'').

% invisible dcgs (up to 256 streams)
b0(dcg_def/1,arith(0),in_body,
   'backtrackable: sets current Assumption Grammar stream - usually a hidden DCG list').
b0(dcg_val/1,arith(1),in_body,
   'backtrackable: retrieves current Assumption Grammar stream - usually a hidden DCG list').
b0(dcg_tell/1,arith(0),in_body,'switches to hidden DCG-stream number K (form 0 to MAXDCG=255)').
b0(dcg_telling/1,arith(1),in_body,'retrieves which hidden DCG-stream we a re processing'-x(_)).

b0(open_stream/4,arith(1),in_body,'(Type,FileOrCmd,ReadWriteAppend,?StreamID) opens various streams').
b0(close_stream/2,arith(0),in_body,'(Type,StreamID) closes various streams').
b0(fgetc/2,arith(1),in_body,
  'fgetc(IntegerStreamNo,CharCode) inputs a char code from a C stream').
b0(fputc/2,arith(0),in_body,
  'fputc(IntegerStreamNo,CharCode) outputs a char code to a C stream').
b0(fflush/1,arith(0),in_body,
  'fflush(IntegerStreamNo) flushes a C-stream').
b0(fsize/2,arith(1),in_body,
  'returns the size of the file associated to a C stream, in bytes').

% b0(setref/2,arith(0),in_body,'').

b0(unix_argc/1,arith(1),in_body,
  'gets cmd line arg counter'-x(_)).
b0(unix_argv/2,arith(1),in_body,
   'gets a cmd line arg from 0 to argc'-x(0,_)).
b0(unix_getenv/2,arith(1),in_body,'gets an environment variable').
b0(unix_access/2,arith(0),in_body,
  'checks if arg1 (a path+file) is accessible in arg 2 (integer) mode').
b0(unix_cd/1,arith(0),in_body,'changes local dir to arg 1').
b0(unix_fork/1,arith(1),in_body,'starts child process with Unix fork').
b0(unix_pid/1,arith(1),in_body,'returns process id of current process').
b0(unix_kill/2,arith(0),in_body,
    'sends signal arg 1 to process with pid arg 2').

b0(create_engine/4,arith(1),in_body,
 'create_engine(Heap,Stack,Trail,IntHandle) creates an engine IntHandle').
b0(destroy_engine/1,arith(0),in_body,
  'destroy_engine(E) frees memory of engine E (an integer)').
b0(load_engine/3,arith(0),in_body,
  'load_engine(E,Goal,Answer) prepares engine E to execute Goal/Answer').
  
b0(ask_engine/2,arith(1),in_body,
  'ask_engine(E,X) retrieves from engine E (a copy of) answer X').
b0(list_engines/1,arith(1),in_body,'lists available engine handles').
b0(current_engine_addr/1,arith(1),in_body,'returns current engine handle').
b0(get_engine_prop/3,arith(1),in_body,'args: Engine,PropertyNo,Val').

b0(ask_thread/2,arith(1),in_body,
  '(E,R): asks an answer of engine E on a new thread R').
b0(tsync_op/3,arith(0),in_body,
  'same as thread_operation(Op, MutexOrParam, ActionOrValue) various thread synchronization operations').
b0(thread_exit/1,arith(0),in_body,'exits a thread').
b0(thread_join/1,arith(0),in_body,'joins thread').
b0(current_thread/1,arith(1),in_body,
 'gets thread id number of current thread').

b0(untrail_to/1,arith(0),in_body,'unwinds the trail up to a choice point').
b0(get_neck_cut/1,arith(1),in_body,'gets the choice point as an integer').

b0(override/3,arith(0),in_body,'overrides a compiled predicate - to be used with care').
%b0(trim_cp/1,arith(0),in_body,'').
b0(random_seed/1,arith(0),in_body,
  'initializes random/1 with an integer, uses clock if 0').

b0(member_scan/3,arith(1),in_body,
  'finds first element without unifying to it'-
  member_scan(s(_),[1,s(s(_)),2],_)).
b0(cmember_scan/3,arith(1),in_body,'').
b0(cdel_scan/3,arith(1),in_body,'').

b0(push_code/1,arith(1),in_body,
  'moves code compiled in workspace to kernel and returns top of code area - used by pc/0').

b0(system0/2,arith(1),in_body,'').
b0(new_name/2,arith(1),in_body,'returns a new name based on arg 1').

b0(new_client/3,arith(1),in_body,'from a (host,port) to a client').
b0(new_server/2,arith(1),in_body,'opens on a port a new server').
b0(new_service/3,arith(1),in_body,'from (server,timeout) to a service').

b0(peer_addr/2,arith(1),in_body,
  'gets address of peer connected to socket').

b0(peer_port/2,arith(1),in_body,'gets port of peer connected to socket').

b0(close_socket/1,arith(0),in_body,'closes a server, service or client').
b0(sock_readln/3,arith(1),in_body,'reads from a socket, a line').
b0(sock_writeln/3,arith(0),in_body,'writes to a socket, a line').
b0(sock_read/3,arith(1),in_body,'reads from a socket, a string').
b0(sock_write/3,arith(0),in_body,'writes to a socket, a string').
b0(sock2file/2,arith(0),in_body,'reads from a socket, to a file').
b0(file2sock/2,arith(0),in_body,'writes to a socket, from a file').
b0(sleep/1,arith(0),in_body,'waits arg 1 seconds').
b0(host2ip/2,arith(1),in_body,'converts a host name to an IP address').
b0(qprint/1,arith(0),in_body,'prints out a clause such that a variant of it can be always read back').
% $$ add here
b0(term_store_op/4,arith(1),in_body,'API for external term storage'). % arity 3+1 by binarization
b0(new_builtin/3,arith(1),in_body,'sample 3 arg user added builtin - used for the C interface'). % arity 3+1 by binarization
b0(halt/1,arith(0),in_body,'stops Prolog with given return code when used in main thread- or halts current thread').

% unspecified i-o pattern builtins

b0(true/0,simple,in_head,'always succeeds').
b0(call/1,simple,in_head,'executes (atomic!) arg 1'). 

b0(abort0/0,simple,in_head,'').
b0(restart0/0,simple,in_head,'').

b0(functor/3,simple,in_head,
   'builds or decomposes a coumpound term'-
  [x(f(a,b),_,_),x(_,f,3),x(f(a),f,1)]).

b0(name/2,simple,in_head,
  'bidirectional: converts atomic to/from list of chars'-
  [x(hello,_),x(_,[98,121,101])]).

b0(load0/1,simple,in_head,'').

b0(stat0/3,simple,in_head,'').

b0(list_asm/3,simple,in_head,'').
b0(bb_reset/1,simple,in_head,
'cleans up and resizes to at least arg 1 bytes compound term area of the blackboard').

b0(garbage_collect/0,simple,in_head,'performs heap gc now').

b0(profile/0,simple,in_head,'in specialy compiled profiler mode prints out info accumulated so far').

b0(member_entry/2,simple,in_head,'').
b0(for_entry/3,simple,in_head,'').

b0(return0/1,simple,in_head,'returns a term from an engine left in a state ready to resume').

b0(fcall/3,simple,in_head,'calls a list to list function: to be implemented').

b0(if0/3,simple,in_head,'').

% things defined in Prolog which should look like builtins

p0('!',0,'succeeds like true/0, but removes pending choices in calls at its LEFT and makes things look as if this were the LAST clause of the predicate').
p0(get_deep_cut,2,'gets a choice point address, used with 1 arg only').
p0(cut_to,1,'cuts to an int in arg 1, a choicepoint address').
p0(translate_clause,2,'').
p0(translate_def,2,'').
p0('.',2,'').
p0(end_of_file,0,'Prolog atom returned by read when at the end of a file').
p0(pc,0,'pushes code compiled into the workspace to the persistent kernel').
p0(restart,0,'cleans up data areas and reinitializes symbol tables').
p0(abort,0,'returns to toplevel').
p0(init_io,0,'intialises fast C-based IO routines').
p0(heap_size,1,'').
p0(stack_size,1,'').
p0(trail_size,1,'').

p0(gc,0,'enables heap gc').
p0(nogc,0,'disables heap gc').
p0(gc_status,1,'shows if heap gc is enabled or not').

p0(dynbbgc,0,'makes blackboard dynamic with gc on').
p0(bbgc,0,'enables blackboard gc ').
p0(nobbgc,0,'disables blackboard gc').
p0(bbgc_status,1,'shows if blackboard gc is enabled or not').

p0(bb_gc,0,'performs blackboard gc now, if enabled').
p0(bb_gc0,0,'performs blackboard gc now, in this engine').

p0(quiet,1,'gets/sets level of "quietness"').

p0(current_engine_id,1,'returns a unique id associated to an engine at creation time').
p0(current_engine,1,'gets the unique id of the current engine').
p0(get_engine_id,2,'(+Engine,-Id) gets the unique id associated to an engine at creation time').

p0(create_engine,1,'makes a new engine or reuses a dead one').
p0(create_new_engine,1,'creates an engine as big as the current one').
p0(open_engine,3,
  'open_engine(G,X,E) creates an engine E ready to execute goal G with answer X').

%Kernel Prolog engine operations
p0(new_engine,3,
  'new_engine(X,G,E) creates an engine E ready to execute goal G with answer X').
p0(reuse_engine,3,
  'reuse_engine(X,G,E) initializes engine E with goal G and answer pattern X').
p0(get,2,
  'get(E,A) returns a new answer A=the(...) from engine E or returns no if no (more) answers exist').
p0(stop,1,
  'stops and frees resources held by an engine (may happen automaticaly if an engine fails)').
p0(return,1,
  'returns data from an engine as if it were an answer - such that the engine can be resumed with get/2 to execute the next goal').
p0(this_engine,1,'gets a handle to the current engine').  
p0(to_engine,2,'(E,T) sends to engine E a term T, and fails if E has an empty message box').
p0(from_engine,1,'(T) tries to take a term T from the message box of this engine and sets the message box empty').  
p0(element_of,2,'(Engine,Answer: backtracks over the answers of a fluent (usually an engine)').
      
%end Kernel Prolog
p0(show_engine,0,
  'if debugmes/1 is on (as with quiet(1)), shows params of current engine').

p0(list_engines,0,'prints out the list of active engines').
p0(clean_up_engines,1,'internal predicate').
p0(clean_up_engines,0,'frees resources used by all engines except main').
p0(clean_up_dead_engines,0,'frees resources used by dead engines').

p0(has_threads,0,'succeeds if threads available on this platform').
p0(thread_exit,0,'exits a thread').
p0(lock_thread_guard,1,'locks thread guard created with new_thread_guard').
p0(unlock_thread_guard,1,'unlocks thread guard').
p0(try_unlock_thread_guard,1,'try to unlock a thread guard').
p0(try_unlock_thread_guard,2,'try to unlock a thread guard for a specified timeout').
p0(thread_wait,1,'waits on guard until notified by thread_notify').
p0(thread_notify,1,'notifies a thread waiting on guard with thread_wait').
p0(thread_notify_all,1,'notifies all threads waiting on guard with thread_wait').
p0(thread_timed_wait,2,'waits on first arg Guard second arg msec or until notified').
p0(thread_cancel,1,'terminates (cancels) thread given in arg 1').
p0(thread_resume,1,'resumes execution of suspended thread').
p0(thread_suspend,1,'suspends execution of thread').
p0(get_engine_thread,2,'gets from an Engine the thread it is running on').
p0(current_engine_thread,1,'gets from the current Engine the thread it is running on').
p0(bg,7,'bg(Goal,H,S,T,-Thread,-EngineAddr,-EngineID): runs goal in background on engine with given heap,stack,trail').
p0(bg,4,'bg(Goal,Thread,EngineAddr,EngineID): runs goal in background on new engine - with unique EngineID').
p0(bg,3,'bg(Goal,Thread,EngineAddr): runs goal in background if threads are available').
p0(bg,2,'runs Goal in new background thread, which is returned in second arg ').
p0(bg,1,'runs Goal in background thread - you can set engine size like in heap(500)=>bg(...)').

p0(begin_critical,0,'begin serialized execution - enters critical region').
p0(end_critical,0,'ends serialized execution - exits critical region').
p0(put_critical,2,'(Guard,Data): updates mutex Guard-protected Data on blackboard').
p0(get_critical,2,'(Guard,Data): accesses mutex Guard protected Data on blackboard').

p0(synchronize_on,3,'wraps Goal for sync on given mutex for serialized execution and returns true or fail').
p0(synchronize_on,2,'wraps Goal for sync on given mutex for serialized execution').
p0(synchronize,2,'wraps Goal for serialized execution and returns true or fail').
p0(synchronize,1,'wraps Goal in arg 1 for serialized execution').
p0(sdebug,1,'emits mt-safe synchronized debug message').
p0(new_thread_guard,1,'returns a new free thread guard from pool').
p0(free_thread_guard,1,'gives back to pool thread guard in arg 1').

% Linda operations between threads
p0(local_out,1,'produces a term and possibly wakes up the thread at a matching local_in/1').
p0(local_cout,1,'puts a term on local blackbord unless already there').
p0(local_in,1,'waits for a term produced by a matching local_out/1').
p0(local_rd,1,'tests if a term is available on the local blackboard').
p0(local_when,1,'waits until a term is available on the local blackboard').
p0(local_cin,1,'removes a term if available on the local blackboard').
p0(local_all,3,'local_all(X,G,Xs) collects facts X such that G on the blackboard').
p0(local_all,2,'local_all(X,Xs) collects all facts matching X on the blackboard').
p0(wait_for,2,
  'wait_for(Term,Constraint) waits for a term on the blackboard, such that Constraint holds').
p0(notify_about,1,'notifies a suspended matching wait_for(Term,Contraint), if Constraint holds, that Term is available').
p0(all_for,2,'all_for(X,Xs) collects all constrained terms X on the blackboard to list Xs').

p0(out,1,'puts a term on Linda server or trigers resumption of a matching in/1 waiting for this data').
p0(in,1,'waits to remove a term from Linda blackboard').

p0(all,2,'gets the list of terms matching arg 1 from Linda blackboard').
p0(all,3,'gets a selection arg 1 of terms matching arg 2 from Linda blackboard').
p0(rd,1,'reads a term matching arg 1 from Linda blackboard').
p0(cin,1,'tries to get and remove a term from Linda blackboard').

p0(cout,1,'adds a term to the blackboard, unless already a matching one is there').

p0(include,1,'includes/loads a file with current load method, unless it has already been included by the same method').

p0(load_method,2,'args: Number, Name'-x(_,_)).

p0(set_load_method,1,'sets the current load method by name').
p0(get_load_method,1,'gets the current load method by name').

p0(dconsult,1,
  'reconsult/1 variant: cleans up data areas, consults/compiles based on db_ratio/1').
p0(sconsult,1,
 'reconsult/1 variant: cleans up data areas consults, makes all static').
p0(oconsult,1,
  'reconsult/1 variant: consults/compiles based on db_ratio and overwrites old clauses').
p0(scompile,1,
  'smart compile/1 variant: if the *.wam file is newer reloads, otherwise fcompiles first').
p0(mcompile,1,'compile/1 variant: cleans up data areas and compiles to memory').
p0(qcompile,1,'compile/1 variant: compiles a file to memory after pushing current user code to kernel - where it becomes read only').
p0(compile,1,
  'applies current compilation method to the file arg 1').
p0(load,1,'clean loads from a bytecode *.wam file').
p0(fcompile,1,'compiles a *.pl file to a *.wam bytecode file').

p0(debug,1,'').
p0(consult,1,
  'consults with possible duplication of clauses, allows later dynamic recompilation depending on db_ratio/1').
p0(consult,2,
  'consult(File,DB) consults File into DB)').

p0(reconsult,1,'applies current consult method to file given as arg 1, set db_ratio/1 for specifying dynamic recompilation of heavily used interpreted code').
p0((~),1,'short hand for reconsult').

p0(assert_from_chars,1,'asserts a program from clauses in list of chars').
p0(assert_from_chars,2,'(Db,Cs) asserts to database Db, a set of clauses parsed from list of char codes Cs').
p0(read_terms_from_chars,2,'(Chars,Clause): backtracks over Clause(s) parsed from a list of char codes').
p0(read_terms_from_chars,3,'(Chars,Clause,VarsInClause): backtracks over Clause(s) parsed from a list of char codes'-
  [x("b(X,Y):-a(Y,X). a(1,1). a(_,2). ",_,_)]
  ).
  
p0(get_lineno,1,'gets line number counter in current file').
p0(set_lineno,1,'sets line number counter in current file - use with care').

p0(consult_cmd,1,'').
p0(consult_cmd,2,'').
p0(file_cmd_hook,2,'file_cmd_hook(Cmd,Db): allows defining user actions on commands read from files').
p0(consult_included,2,'').
p0(consult0,1,'').
p0(consult0,2,'').

p0(terminate_load,0,'').
p0(terminate_file,2,'').
p0(load_file,1,'').
p0(fcompile_file,1,'').
p0(scompile_file,1,'').
p0(mcompile_file,1,'').

p0(translate_all,2,'').
p0(get_a_predicate,3,'').
p0(or,2,'').
p0(if,3,'').

p0(termcat,3,'').

p0(asm,0,'shows transformations and readable BinWAM assembler for Prolog code entered at terminal').
p0(asm,1,'generates readable binarized form and BinWAM assembler to a file').
p0('=..',2,'called univ -this is bidirectional- it converts between a term and its view as a alist of components'-
  [x(f(a,b),_),x(_,[f,a,b])]
).
p0('::-',2,'variant of :- for hand transformed binary clauses - use with care').

p0('##',1,'executes arg 1 at compile time - make sure the executed code terminates').
p0(proto_append,2,'').
p0(proto_member,2,'').
p0(member3,3,'').

p0(n_inline,1,'').
p0(n_arith,1,'').
p0(n_builtin,1,'').
p0(n_nop,1,'').

p0(statistics,0,'shows info about data areas'-x).
p0(stat,0,'short hand for statistics').
p0(interactive,1,'toggles interactive query answering/tracing with arg 1 = yes or no').

p0(def_to_mbin,2,'').
p0(bincall,2,'call binary predicate with given continuation').
p0(metacall,1,'calls the interpreter').
p0(on_undefined,1,'(Goal): defines handler for undefined predicates matching Goal').
p0(metatrue,0,'calls the interpreter on current continuation').

p0(call,2,'efficient call/N variant').
p0(call,3,'efficient call/N variant').
p0(call,4,'efficient call/N variant').
p0(call,5,'efficient call/N variant').
p0(call,6,'efficient call/N variant').
p0(call,7,'efficient call/N variant').

p0(override_call,3,'').
p0(safe_override_call,3,'').

p0(once,1,'executes once, with no backtracking').

p0(map,2,'maps a predicate with 1 arg to a list'-
  x(println,[10,20,30])).

p0(map,3,'maps a predicate with 2 args to a list'-
  x(+(1),[10,20],_)).
  
p0(maplist,2,'maps a predicate with 1 arg to a list'-
  x(println,[10,20,30])).

p0(maplist,3,'maps a predicate with 2 args to a list'-
  x(+(1),[10,20],_)).  

p0(foldl,4,'(Op,InitialValue,List,?Result) accumulates values interating over List with binary Op'-
  x(+,0,[10,20,30],_)).
p0(foldr,4,'(Op,InitialValue,List,?Result) accumulates values interating over List with binary Op'-
  x(+,0,[10,20,30],_)).

p0(sum,2,'(List,?Result): sum of a list'-
  x([10,20],_)).
p0(prod,2,'(List, ?Result): product of a list'-
  x([10,20],_)).

p0(expand_term,2,'expands a term according to DCG expansion rules').
p0(term_expansion,2,'can be used to define a hook into the default DCG expansion mechanism').
% p0(':-',2,'').
p0('=',2,'(X,Y) true if (possibly cyclic) terms X and Y unify - cyclic terms can result from =/2, as occur check is not performed'-
  x(f(_,s(a)),f(Y,Y))
).
p0('->',2,
  'Cond->Then executes Cond once; if it succeeds it also executes Then').
p0('if_any',3,
  '(Cond,Then,Else): executes Cond; each time when Cond succeeds it also executes Then; if Cond never succeds it executes Else').
p0(';',2,'A;B succeeds if A succeeds or B, called after A, succeeds').
p0(',',2,'A,B succeeds if A suceeds and B, called after A, succeeds').
p0(repeat,0,
  'backtracks until its continuation succeeds; defined as repeat. repeat:-repeat. ').
p0(foreach,1,
  'foreach(G) backtracks over all answers to G and succeeds').
p0(foreach,2,
  'foreach(G,D) executes D once for each answer of generator G').
p0(forall,1,
  'forall(G) backtracks over all answers to G and succeeds').
p0(forall,2,
  'forall(G,D) executes D once for each answer of generator G').  
p0(for_all,2,
  'foreach(A,B) fails for all cases when A succeeds and B fails').
p0((\+),1,'succeeds if its argument is executed and fails').
p0((\=),2, 'true if args fail to unify').
p0(findall,3,
  'findall(X,G,Xs) collects copies of all answers X of G to Xs. If less then half of the heap is free, it allocates new engine for running G'-
     x(s(X),
       (member(X,[1,2,3]),X>1),
       _)
).
p0(findall,4,
 'findall(X,G,Xs,Ys) appends the list of answers X of G to Ys to obtain Xs'
 -
  x(s(X),
       (X=1;X=2),
       _,[3,4])
).

p0(qfindall,4,
 'qfindall(X,G,Xs,Ys): queues based, slightly faster findall/4, not MT-safe'
 -
  x(s(X),
       (X=1;X=2),
       _,[3,4])
).

p0(all_answers,3,
  '(X,G,Xs): like findall/3, but such that if V is not common to X and G then V cannot be bound by execution of G'-x(X,member(s(X),[_,B,B,_]),_)).
p0(while,2,'(Cond,Goal): findall variant which explores alternative answers for Goal, while Cond holds, ').
p0(skip_until,2,'findall variant').
p0(skip_when,2,'findall variant').
p0(find_while,4,'findall variant').
p0(nth_answer,2,'(N,Goal) returns only the nth answer of Goal, if such an anser exists').
p0(take_at_most,2,'(N,Goal) computes at most N answers of Goal').
p0(drop_at_least,2,'(N,Goal) drops at least N answers of Goal G').
p0(has_fuel,1,'').
p0(find_at_most,4,'(N,X,G,Xs) findall variant, computing at most N answers X of G').
p0(all_but_at_least,4,'(N,X,G,Xs) findall variant, computing all but the first N answers X of G').
p0(det_call,1,'calls a Goal and warns if it was not deterministic').

p0(for,3,'generates an integer in a range'-
  x(_,1,3)).
p0(between,3,'generates an integer between Min and Max'-
  x(1,3,_)).
p0(argn,3,'generates all n args of term'-
  x(_,f(a,b),_)
).
p0(append,3,'concatenates/decomposes lists'-
 [
   x([1,2],[3,4],_),
   x(_,_,[1,2])
 ]
 ).
p0(member,2,'(X,Xs): checks if an element X unifies with an element on a list Xs or generates sucessively longer lists if Xs is unbound or open ended'-
  [x(2,[1,2]),
   x(_,[1,2])
  ]).
p0(memberchk,2,'(X,Xs) checks if an X is a the list Xs').
p0(member_conj,2,'like member/2, for a comma separated conjunction, ending with true'-x(_,(a,b,true)) ).
p0(det_append,3,'').
p0('.',3,'(Head,Tail,List) builds List=[Head|Tail]').
p0('++',3,'concatenates N lists, usable in is/2').
p0(length,2,'generates/mesures length of a list').
p0(make_cmd0,2,'concatenates a list of strings and atomic elements into a string').
p0(make_cmd,2,'concatenates a list of strings and atomic elements into an atom').
p0(make_spaced_cmd,2,'concatenates a list of elements with inserted space separtors into an atom').
p0(listify,2,'(T,Cs): transforms T, unless it is already such, to list of chars').

p0(is_spec,1,'true if a spacial character code').
p0(is_terminator,1,'true if a terminator character code').
p0(is_maj,1,'true if is an upper case char code').
p0(is_min,1,'true if a lower case char code').
p0(is_an,1,'true if an alphanumerical char code').
p0(is_num,1,'true if a digit char code').

p0(numbervars,3,
  'binds to $VAR(I) with I over distinct integers variables in a term').
p0(numbervar_name,2,'').

p0(answer_of,2,'(X,G): X is an answer for G, after finding all, sorting and removing duplicates').
p0(solutions,2,
  '(GX,Xs): adds (last) output arg X to closure G then works like findall(X,GX,Xs)'
  -x(argn(_,f(a,b,c)),_) ).
p0(gc_call,1,'G: executes G and ensures that no more space is consumed than the total size of the terms bound to its variables').
p0(ground,1,'true if arg has no free variables'-x(f(a,b))).
p0(atom,1,'true if symbol (functor of arity 0)'-x(a)).
p0(float,1,'true if represented as a 64 bit float number (C-double)'-x(3.14)).
p0(number,1,'true if integer or float').
p0(compound,1,'true if it has arity > 0'-x(f(a))).
p0(appendN,2,'concatenates N lists'-x([[a,b],[],[c]],_)).
p0(reverse,2,'reverses a list'-x([a,b,c],_)).
p0(append_conj,3,'concatenates 2 conjunctions').
p0(append_disj,3,'concatenates 2 disjunctions').
p0(subsumes_chk,2,'checks if arg 1 is subsumed by arg 2, after renaming vars').
p0(variant_of,2,'checks if args are the same up to a renaming of vars').
p0(tab,1,'outputs N blanks').
p0(get,1,'inputs the next char code after skiping over white space').
p0(is,2,'calls the function evaluator, mostly for arithmetics'-
  x(_,3+4*2)
).
p0(expr,2,'').

p0(compare,3,'returns <,=,> in arg 1 after comparing arg 2 with arg 3'-
  [x(_,1,2),x(_,f(b),f(a)),x(_,s(X),s(X))]).
p0(( == ),2,'true if args are identical terms').
p0(( \== ),2,
  'true if arg 1 is not identical to arg 2').
p0(( @< ),2,'instance of compare/3 with arg 1: <').
p0(( @> ),2,'instance of compare/3 with arg 1: >').
p0(( @=< ),2,'instance of compare/3 with arg 1: = or <').
p0(( @>= ),2,'instance of compare/3 with arg 1: > or =').
p0((  <  ),2,'numeric comparison').
p0((  >  ),2,'numeric comparison').
p0(( =< ),2,'numeric comparison').
p0(( >= ),2,'numeric comparison').
p0(( =:=),2,'numeric comparison').
p0(( =\=),2,'numeric comparison').
p0((+),2,'returns 0 + arg 1').
p0((-),2,'returns 0 - arg 1').
p0((\),2,'complement').
p0(max,3,'(X,Y,Max): Max is the max of 2 numbers X, Y').
p0(min,3,'(X,Y,Min): Min is the min of 2 numbers X, Y').

p0(bb_def,3,
  'bb_def(K1,K2,T) associates to K1 and K2 (a copy of) T on the blackboard').
p0(bb_set,3,
  'bb_set(K1,K2,T) updates the term associated with K1 and K2 to be a copy of T').
p0(bb_let,3,
  'bb_let(K1,K2,T) updates or defines the term associated with K1 and K2 to be T').
p0(bb_get,3,
  'bb_get(K1,K2,T) consumes the term T associated with K1 and K2').
p0(bb_val,3,
  'bb_val(K1,K2,T) T is (a copy of) the term associated with keys K1 and K2').
p0(bb_rm,2,
  'removes the term associated with K1 and K2 from the blackboard').

p0(let,3,'').

p0(let,2,'').
p0(def,2,'').
p0(set,2,'').
p0(val,2,'').
p0(rm,1,'').

p0(bb_def,2,'').
p0(bb_set,2,'').
p0(bb_val,2,'').
p0(bb_rm,1,'').
p0(bb_let,2,'').
p0(bb_get,2,'').

p0(nb_delete,1,'').
p0(nb_setval,2,'').
p0(nb_getval,2,'').

p0(prolog_flag,2,'(Flag,Value): retrieves the value of a Prolog flag').
p0(set_prolog_flag,2,'(Flag,Value): sets the value of a Prolog flag').
p0(static_prolog_flag,2,'contains read-only Prolog flag values').

p0(vread,2,'reads HDEFI or HDEFS defined C constant into a Prolog integer or atom').

p0(vget_int,2,'gets a VSHARE defined int C variable to a 28 bit int').
p0(vset_int,2,'sets a VSHARE defined int C variable to a 28 bit int').

p0(vget0,2,'gets the type and value of a C word').
p0(vget,2,'gets a VSHARE declared C data object in a term like int(N),F/N or var(V)').
p0(vset,2,'sets a VSHARE declared C data object in a term like int(N),F/N or var(V)').
p0(gvset,2,'').
p0(gvget,2,'').

p0(set_bp_error,4,'(Id,Mes,Arg1,Arg2): notifies emulator about error condition').
p0(get_bp_error,4,'(Id,Mes,Arg1,Arg2): gets error sate from emulator').
p0(clear_bp_error,0,'clears errors set by various conditions').

p0(pow,3,'(Base,Expo,Val) computes power function'-x(2,3,_)).
p0('**',3,'returns arg 1 at power arg 2, a float'-x(2,3,_)).
p0(log,3,'returns log in base arg 1 of arg 2, a float'-x(2,8,_)).
p0(atan2,3,'float function').
p0(hypot,3,'float function').

p0(sqrt,2,'returns square root of arg 1, a float'-x(2,_)).
p0(exp,2,'float function').
p0(log,2,'float function').
p0(sin,2,'float function').
p0(cos,2,'float function').
p0(tan,2,'float function').

p0(asin,2,'float function').
p0(acos,2,'float function').
p0(atan,2,'float function').

p0(integer,2,'float to int cast').
p0(float,2,'float function').
p0(sign,2,'int function').
p0(abs,2,'int function').
p0(floor,2,'float to int function'-x(1.3,_)).
p0(ceiling,2,'float to int function'-x(1.3,_)).
p0(truncate,2,'float to int function'-x(1.51,_)).
p0(round,2,'float to int function'-x(1.51,_)).
p0(xor,3,'bitwise exclusive or').

p0(statistics,2,'returns info about data areas'-x(_,_)).
p0(predicate_property,2,'returns a property of a predicate'-
  predicate_property(write(_),_)
).
p0(current_predicate,2,'generates/checks name and head of a currently defined predicate').
p0(current_predicate,1,'generates/checks the head of an existing predicate').
p0(ctime,1,'gets elapsed cpu time in ms'-x(_)).
p0(rtime,1,'gets elapsed real time in secs'-x(_)).
p0(otime,1,'gets time in secs from arbitrary origin to start'-x(_)).
p0(abstime,1,'gets time in secs since arbitrary origin'-x(_)).

p0(help,0,'generates file help.txt with info and examples').
p0(help,1,'same as apropos/1'-x(assert)).
p0(apropos,1,'prints names of predicates defined in the system'-
   x(garbage)).
p0(apropos,2,'returns names of predicates defined in the system'-
   x(retract,_)).
p0(unix,1,'executes various Unix commands').

p0(unix_argv,1,'gets the list of cmd line args from 1 to argc'-x(_)).

p0(unix_cat,1,'prints a file to user terminal').

p0(file2chars,2,'reads a file to a list of ascii codes').
p0(char_of,2,'reads a Prolog file to a set of ascii codes - on backtracking').
p0(sentence_of,2,'reads a natural language file to a sentence built as a list of words - on backtracking').
p0(sentence_of,3,'(File,Ends,Sent) reads a file to a sentence separated by Ends - on backtracking').
p0(line_of,2,'(File,Line) reads a file to lines ending with eol - on backtracking').

p0(term_of,2,'reads a Prolog file to a set of terms - on backtracking').
p0(tokens_of,2,'reads a Prolog file to a slist of tokens').
p0(token_of,2,'reads a Prolog file to a set of tokens - on backtracking').
p0(clause_of,2,'reads a Prolog file to a set of clauses - on backtracking').

p0(shell,1,'passes a command to the OS').
p0(system,1,'passes a command to the OS').
p0(system,2,'passes a command to the OS and gets back return code').
p0(cd,1,'changes local dir to arg 1').
p0(cd,0,'changes local dir to HOME directory or / if no such env var').
p0(pwd,0,'shows current dir').
p0(pwd,1,'returns current dir as a list of chars').

% extra.pl

p0(trace,1,'traces execution of a goal').
p0((dynamic), 1,'states that a predicate can be updated').
p0((multifile), 1,'states that clauses of a predicate can be in different files').
p0(is_multifile, 1,'').
p0((discontiguous), 1,'states that clauses of a predicate can be in different places').
p0(is_discontiguous, 1,'checks if a predicate has been declared as discontiguous').

p0(hkey,2,
  'computes hash code on atomic argument; fails on variables and compound terms'-
  [term_hash(t(a,b),_),
  term_hash(t(a,c),_)]
).

p0(term_hash,2,
  'computes hash code on terms ground up to depth 64; fails if something is unbound or the limit is reached'-
  [term_hash(t(a,b),_),
  term_hash(t(a,c),_)]
).

p0(global_set,3,'(A,B,X): associates X to ground keys A,B').
p0(global_get,3,'(A,B,X): retrieves X associated to ground keys A,B').
p0(global_rm,2,'(A,B): removes value associated to ground keys A,B').

p0(set_hash_max,1,'Set the range of values from -1 to -Max, that should be a prime number, defaults to 1999').
p0(hash_trace,2,'tracer for hash maps').

p0(hash_push,2,'(GroundKey,Term): attaches a term to a ground key in constant time as a the first element').
p0(hash_put,2,'(GroundKey,Term): attaches a term to a ground key in constant time as the last element').
p0(hash_get,2,'(GroundKey,Term) retrieves in constant time a term attached to a ground key').
p0(hash_rm_one,2,'(GroundKey) removes a term attached to a key and returns it').
p0(hash_rm,2,'(GroundKey) removes a term attached to a key and returns it - backtracks').
p0(hash_clear,2,'(GroundKey,Term) removes all matching terms attached to a key').
p0(hash_clear,1,'(GroundKey) removes all terms attached to a key').
p0(hash_clear,0,'removes all terms from all hash keys').
p0(hash_gc,0,'frees space used by hash keys and values').
p0(hash_save,2,'(File,PredNMame) saves the hashed map to a database in predicate PredName ready for 1-arg indexing if compiled').
p0(hash_save,1,'(File) saves the hashed map to a database ready for 1-arg indexing if compiled').
p0(hash_compile,0,'compiles the hashed map to a predicate benefiting from 1-arg indexing').
p0('$hx',3,'hidden compiled hashed database predicate').
p0(hash_load,1,'(File) loads the hashed map to a database').
p0(hash_key,1,'(IntKey) backtrack over hash keys').
p0(hash_find_unique,3,'like findall(X,G,Xs) but uses hashing on ground term X to collect unique answers').

p0(push_term,2,'(GroundKey,Term): attaches a term to a ground key in constant time as a the first element').
p0(put_term,2,'(GroundKey,Term): attaches a term to a ground key in constant time as the last element').
p0(new_iterator,2,'(GroundKey,Iterator) gets an iterator to terms attached to ground key').
p0(close_iterator,1,'(Iterator) closes an iterator').
p0(has_terms,1,'(GroundKey) succeds if at least on term is attached to GroundKey').
p0(get_next_term,2,'(Iterator,Term) returns the next term attached to an iterator, fails if no more left').
p0(remove_current_term,1,'(Iterator) removes current term attached to an iterator').
p0(update_current_term,2,'(Iterator,NewTerm): replaces current term of an iterator').
p0(delete_all_terms,1,'(GroundKey) removes all terms attached to a key').
p0(count_terms,2,'returns the number of terms attached to a key').
p0(get_term,2,'(Iterator,Term): backtracks over terms associated to a key').

p0(get_all_terms,2,'(GroundKey,Ts) collects to alist all terms attached to a key').

p0(new_term,2,'(Term,Handle): creates a new external Term and returns an integer Handle to it').
p0(instance_of,2,'(Handle,Term): creates an internal instance of an external term given as a Handle').
p0(free_term,1,'(Handle): frees external term given as a Handle').
p0(new_key_iterator,1,'(Iterator): returns an iterator over the set of external keys').
p0(tstest,3,'(K,V,R): performs various tests with external terms').
p0(process_term,3,'(OpCode,Input,Output): applies various user defined C functions to external terms').

p0(mmap_new,1,'(D): returns a new mmap handle D - a mmap holds multiple elements for each key').
p0(mmaps_iterate,1,'(D): backtracks over mmap handles created with mmap_new D').
p0(mmaps_gc,0,'frees memory for all mmaps created with mmap_new').
p0(mmaps_clean,0,'empties and cleans up all mmaps created with mmap_new').
p0(mmaps_show,0,'lists the content of all mmaps created with mmap_new').

p0(mmap_push,3,'(D,K,V): adds K,V as the first element of mmap D').
p0(mmap_put,3,'(D,K,V): adds K,V as the last element of mmap D').
p0(mmap_get,3,'(D,K,V): gets V, given K in mmap D,  backtracks over K if K unbound').
p0(mmap_rm_one,3,'(D,K,X): removes first term X attached to key K from mmap D').
p0(mmap_rm,3,'(D,K,X): removes a term X attached to key K from mmap D and backtracks on each such term').
p0(mmap_rm_all,2,'(D,K): removes all terms X attached to key K from mmap D').
p0(mmap_gc,1,'(D): frees unused memory in D').
p0(mmap_clear,1,'(D): empties and cleans up D').
p0(mmap_key,2,'(D,K): succeds if K is known, otherwise finds V backtracks over each key K in D').
p0(mmap_show,1,'(D): lists the content of a mmap').

p0(map_new,1,'(D): returns a new map handle D').
p0(maps_iterate,1,'(D): backtracks over map handles created with mmap_new D').
p0(maps_gc,0,'frees memory for all maps created with mmap_new').
p0(maps_clean,0,'empties and cleans up all maps created with mmap_new').
p0(maps_show,0,'lists the content of all maps created with mmap_new').

p0(map_put,3,'(D,K,V): adds K,V to map D').
p0(map_get,3,'(D,K,V): gets V, given K in map D, backtracks over K if K unbound').
p0(map_rm,2,'(D,K): removes key K from map D').
p0(map_gc,1,'(D): frees unused memory in D').
p0(map_clear,1,'(D): empties and cleans up D').
p0(map_key,2,'(D,K): succeds if K is known, otherwise finds V backtracks over each key K in D').
p0(map_show,1,'(D): lists the content of a map').

p0(addq,3,'adds to end of persistent queeue'-
 x(key1,key2,33)).
p0(pushq,3,'adds to beginning of persistent queeue'-
 x(key1,key2,f(X,X))).
p0(cpopq,3,'pops (copy of) first element of persistent queue'-
 x(key1,key2,_)).
p0(cmembq,3,'generates (copies of) members of a queue'-
  x(key1,key2,_)).
p0(cdelq,4,'deletes first matching element from a queue'-
 x(key1,key2,_,_)).

p0(cdelq_any,3,'deletes any matching element from a queue'-
 x(key1,key2,_)).
 
p0(membq,3,'').


%p0(persistent,1,'').
%p0(is_persistent,1,'').

p0(set_db,1,'sets the name of active database for dynamic code').
p0(current_db,1,'gets the name of currently active database').

p0(asserted,1,'runs a predicated if asserted').
p0(asserta,1,'adds a clause to be first in a predicate definition').
p0(assertz,1,'adds a clause to be last in a predicate definition').
p0(assert,1,'adds a clause').
p0(retract,1,'backtracks over deleting matching clauses').
p0(retract1,1,'deletes first matching clause in the current database').
p0(retractall,1,'deletes all matching clauses').
p0(clause,2,
 'clause(H,B) generates a clause with head matching H and body B').
p0(abolish,1,'abolish(F/N) deletes predicate F/N').
p0(abolish,2,'').

% indexed database
p0(db_hook,0,'').
p0(db_hook_on,0,'').
p0(db_hook_off,0,'').
p0(x_traced_call,1,'traces calls if compile-time x_trace triggers it').
p0(x_trace,1,'traces and profiles predicates, in combination with x_profile').
p0(x_trace,2,'db hook, internal').
p0(x_profile,0,'db hook, shows the result of profiling compiled predicates').
p0((x_dynamic), 1,'db hook, states that a predicate can be updated').
p0(x_is_dynamic,1,'db hook, checks if dynamic').
p0(x_asserta,1,'db hook, adds a clause to be first in a predicate definition').
p0(x_assertz,1,'db hook, adds a clause to be last in a predicate definition').
p0(x_retract,1,'db hook, backtracks over deleting matching clauses').
p0(x_retractall,1,'db hook, deletes all matching clauses').
p0(x_abolish,1,'db hook, deletes all matching clauses and unmarks the clause as dynamic').
p0(x_clause,2,
  'db hook, clause(H,B) generates a clause with head matching H and body B').
p0(x_consult,1,
  'db hook, consults with possible duplication of clauses').
p0(x_listing,0,'db hook, lists given predicate if in current database').
p0(x_listing,1,'lists predicate F of arity N if in current database').
p0(x_gc,0,'db hook, recovers memory used by the database').

p0(disable_static,1,'').
p0(disable_static,2,'').

p0(disable_builtins,0,'').

p0(db_asserted,2,'runs predicate arg 2 if asserted in database arg 1').
p0(db_asserta,2,'does asserta/1 arg 2 into database given as arg 1').
p0(db_assertz,2,'does assertz/1 arg 2 into database given as arg 1').
p0(db_assert,2,'does assert/1 arg 2 into database given as arg 1').
p0(db_retract,2,'does retract/1 arg 2 from database given as arg 1').
p0(db_retract,3,'db_retract(Db,H,B) retracts clause with head H and body B from database Db').
p0(db_retract1,2,'deletes from database given as arg 1 a matching clause').
p0(db_retractall,2,
  'removes from database given as arg 1, all matching clauses').
p0(db_retractall,3,
  'removes from database given as arg 1, all matching clauses seen as head + body').  
p0(db_head,2,'generates/checks a predicate head in database (arg 1)').
p0(db_clause,3,
 'clause(DB,H,B) generates a clause found in database DB with head matching H and body B').
p0(db_abolish,2,'db_abolish(DB,F/N) removes predicate F/N from DB').
p0(db_abolish,3,'').
p0(db_clean,1,'db_clean(DB) abolishes all predicates in DB').
p0(db_clean,0,'abolishes all predicates in currently active database').
p0(db_move,2,'db_move(FromDB,ToDB) moves the content of database FromDB over database ToDB while replacing similar predicates').
p0(this_db,1,'same as current_db, returns the current database').

p0(db_consult,2,'db_consult(File,Db) consults a file to a database Db').
p0(db_save,2,'db_save(Db,File) saves using qprint/1 all the clauses of Db to File').
p0(db_save,1,'db_save(File) saves all the clauses of the current database to File').
p0(xsave,1,'xsave(File) saves all the clauses of the current database to a binary File').
p0(xload,1,'xload(File) loads clauses to the current database, from a binary File').

p0(make_compileable,2,'make_compilable(Files,File) rewrites Files as a canonical File with all predicates contiguous and no operators').

p0(dynco,1,'yes/no: activates/desactivates dynamic recompilation').

p0(db_ratio,1,'sets/gets call/assert ratio which triggers dynamic recompilation').
p0(make_all_static,0,'').
p0(make_all_dynamic,0,'').

p0(make_static,1,'').
p0(make_dynamic,1,'').
p0(dyn2stat,1,'dyn2stat(H): compiles at runtime a predicate with head H').
p0(dyn2stat,2,'dyn2stat(Db,H): compiles at a predicate from database Db with head H').
p0(stat2dyn,1,'stat2dyn(H): reverts to uncompiled representation for a predicate with head H').
p0(stat2dyn,2,'stat2dyn(Db,H): reverts to uncompiled representation for a predicate in database Db with head H').

p0(listing,0,'lists current database').

p0(listing,1,'lists given predicate if in current database').
p0(listing,2,'lists predicate F of arity N if in current database').

p0(db_listing,2,'lists predicate F/N in given database').
p0(db_listing,1,'lists caluses given database').

p0(topcall,1,'calls arg 1 as if it were entered by the user at Prolog prompt').

p0(is_asserted,1,'checks if currently asserted').
p0(is_dynamic,1,'checks if dynamic').
p0(db_is_dynamic,2,'checks if dynamic in a given database').

p0(bb,0,'lists (bp_long!) content of the blackboard').
p0(bb0,0,'').
p0(bb,1,'').
p0(bb_list,1,'').
p0(bb_element,2,'').

p0(sort,2,'sorts and removes duplicates'-
  x([2,1,3,1,4,4,2],_)).
p0(merge_sort,3,'(Order,List,Sorted)'-x('>',[1,3,2,2,4],_)).
p0(msort,2,'sorts and keeps duplicates'-
  x([2,1,3,1,4,4,2],_)).
p0(keysort,2,'sorts while putting similar keys one after the other in a list'-
  x([3-a,1-a,2-b,1-c,2-d],_)).


p0(keygroup,3,'sorts while grouping similar keys'-
  x([3-a,1-a,2-b,1-c,2-d],_k,_xs)).

p0(bagof,3,'all solutions predicate generating unsorted bags of possibly dupplicated answers'-x(X,member(X,[3,2,2,1]),_)).
p0(setof,3,'all solutions predicate generating sorted sets of unduplicated answers'-x(X,member(X,[3,2,2,1]),_)).
p0((^),2,'calls arg 2 and binds arg 1'-x(X,eq(X,1))).
p0(free_variables,4,'').
p0(not,1,'sound negation').
p0(vars_of,2,'(Term,Vars): lists free vars of a term'-
  x( f(X,t(X,_,Y),Y),_)
).

p0(term_codes,2,
  '(Term,Chars): converts between a term and its list of char code representation'-
[x(f(a,b),_),x(_,"f(a,b)")]).

p0(term_chars,2,
  '(Term,Chars): converts between a term and its list of char code representation'-
[x(f(a,b),_),x(_,"f(a,b)")]).

p0(atom_codes,2,
  '(Atom,CharCodes): converts between an atom and its list of char code representation'-
[x(hello,_),x(_,"hello")]).

p0(number_codes,2,
  '(Number,CharCodes): converts between a number and its list of char code representation'-
[x(1999,_),x(_,"2001")]).

p0(atom_chars,2,
  '(Atom,CharAtoms): converts between an atom and its list of char atoms representation'-
[x(hello,_),x(_,"hello")]).

p0(number_chars,2,
  '(Number,CharAtoms): converts between a number and its list of char atoms representation'-
[x(1999,_)]).

p0(to_string,2,
  'converts a term to a string constant (in paricular, converts numbers to strings)'-
[x(13,_),x(3.14,_),x(f(X,X),_)]).

p0(read_term_from_chars,2,
  'reads a term from a list of char codes'-
[x("f(X,X,Y,Y)",_)]).


p0(('@'),3,
  'alternative form for C/3 DCG connect relation').

p0(star,4,
  'star(Recognizer,Result): DCG based star regexp processor for (Recognizer)*').

p0(plus,4,
  'plus(Recognizer,Result): DCG based plus regexp processor for (Recognizer)+').

p0(one,4,
  'one(Recognizer,Result): DCG based regexp processor for exactly one Recognizer').

p0(dcg_call,4,'(F,X,I,O): DCG metacall for star,plus,one').

p0(dcg_call,5,'(F,X,Y,I,O): DCG metacall for star,plus,one').
    
p0(is_space,3,'').
p0(is_letter,3,'').
p0(is_punct,3,'').
p0(is_digit,3,'').
p0(word,3,'').
p0(nword,3,'').
p0(to_tokens,2,'to_tokens(Codes,PrologTokens)').
p0(to_words,2,'to_words(Codes,NaturalLanguageWords').
p0(to_word_codes,2,'to_word_codes(Codes,ListOfWordCodes)').
p0(term,4,'').
p0(disj_term,4,'').

p0(match_word,3,
  'match_word(+Word,I,O): matches/consumes a word, in the conext of DCGs').
p0(match_before,5,
  'match_before(+Stops,-Word,-Stop,I,O): matches a word with DCGs until a given delimiter set is hit, one of which is also returned').
p0(match_before,4,
  'match_before(+Stop,-Word,I,O): matches a word with DCGs until a given delimiter is hit').

p0(codes_words,2,
  'converts a list of character codes to a list of words and back').

p0(words_code,2,
  'generates a code ready to print, one a t a time with put/1 from a list of words').
  
p0(write_term_to_chars,2,
  'writes a term to a list of char codes'-
[x(f(X,X,Y,Y),_)]).

p0(read_term_from_chars,3,
  'reads a term with variable names from a list of char codes'-
[x("f(X,X,Y,Y)",_,_)]).

p0(write_term_to_chars,3,
  'writes a term with variables names to a list of char codes'-
[x(f(X,Y),['X'=X,'Y'=Y],_)]).

p0(write_term_to_chars1,3,'').




% init.pl

p0(trim_term,2,'').
p0(trim_term,3,'').
p0(trim_term,4,
 'trim_term(D,Filler,T,NewT) replaces subterms of T deeper than D with Filler').
p0(user_error,2,'writes basic error message and fail').
p0(errmes,2,'writes error message and fails').
p0(fatal_error,2,'writes error message and aborts').
p0(quietmes,1,'writes message if in low quietness mode').
p0(quietmes,2,'writes message if quietness is lower than arg 1)').
p0(debugmes,1,'writes message in debug mode (low quietness)').
% entry predicates the emulator
p0(main,0,'user defined optional startup predicate').
p0(main,1,'default entry predicate, does both prolog_init/1 and prolog_run/1').
p0(prolog_load,1,'loads code and/or executes first command line arg').
p0(prolog_init,1,'runs more command line args representing prolog goals').
p0(prolog_run,1,'starts main/0 if defined, otherwise toplevel/0').
% toplevel eval loop predictes
p0(toplevel,0,'interactive toplevel Prolog loop').
p0(topstep,1,'interactive toplevel Prolog step').



% read.pl

p0(is_prolog,1,'recognizes binprolog - useful for portability').
p0(bp_only,1,'runs goal only if is_prolog(binprolog) is true').
p0(bp_only,2,'(Goal,Alternative): runs goal only if is_prolog(binprolog) is true otherwise runs Alternative').

p0(read,1,'reads a term').
p0(read_term,2,'reads a term and also a list of variable-name associations').
p0(top_read_term,2,'').
p0(warn_singletons,3,'').
p0(read_with_singletons,3,'').
p0(read_clause,1,'').
p0(read_tokens,2,'').
p0(get_code,1,'ISO char code reader').
p0(put_code,1,'ISO char code writer'-x(99)).
p0(nl,0,'writes a new line character').
p0(fast_write,1,'').
p0(write_float,1,'').
p0(generic_write,1,
  'overridable write/1, style (writeq, write, display) given with write_style/1 assumption').
p0(generic_write,2,'').
p0(write,1,'writes to current output stream set with tell/1, defaults to <user> - Prolog''s stdio').
p0(print,1,'variant of write/1').
p0(writeq,1,'variant of write which quotes if needed, so that term is read back correctly/1').
p0(portable_display,1,'').
p0(display,1,'writes to terminal while ignoring operator definitions').
p0(portray,1,'').
p0(portray_clause,1,'pretty prints a clause').
p0(pp_clause,1,'prints out a clause with some care on how it looks').
p0(pp_term,1,'pretty prints a term').

p0(read_chars,1,'reads to a list of ascii codes').
p0(write_chars,1,'writes a list of ascii codes'-x("hello")).

p0(get_code,2,'inputs a char code from a stream - ISO').
p0(put_code,2,'outputs a char code to a stream - ISO').
p0(get_char,2,'(Stream,CharAsOneLetterConstant): inputs a char from a stream -ISO Prolog').
p0(put_char,2,'(Stream,CharAsOneLetterConstant): outputs a char to a stream -ISO Prolog').

p0(flush_output,0,'flushes current output stream').
p0(flush_output,1,'flushes a stream').

p0(file_size,2,'returns the size of a file, in bytes').

p0(ttyin,1,'').
p0(ttyout,1,'').

p0(ttyput,1,'').
p0(ttynl,0,'').

p0(ttyprin,1,'writes to terminal').
p0(ttyprint,1,'writes to terminal with a new line').

p0(ttycwrite,1,'').
p0(ttycwriteln,1,'').

p0(popen,3,
  'popen(Cmd,read/write,Stream) opens Stream using a pipe from/to process executing Cmd').
p0(pclose,1,'closes a pipe generated stream').
p0(pcollect,2,'collects output from a command to a list of char codes').

p0(ls2list,2,'(Dir,Files): converts ls cmd output to list of files and dirs').
p0(dir2list,3,'(DirListerCmd,DirName,Files): converts OS specific DirLister output to list of files and/or directories').
p0(dir2dirs,2,'(Dir,Dirs): converts dir cmd output to list of sub directories of Dir').
p0(dir2files,2,'(Dir,Files): converts dir cmd output to list of files (which are not dirs) contained in Dir').

p0(fopen,3,
  'Prolog equivalent of C-function: opens a stream in a given mode and returns an integer handle to it').
p0(fclose,1,
  'closes the C-stream specifiend as an integer handle').

p0(open,3,
  'returns a stream (arg 3) on a file (arg 1) in read/write/append mode (arg 2)').
p0(close,1,'closes a stream opened by open/3').
p0(set_input,1,'sets current input stream').
p0(set_output,1,'sets current output stream').
p0(current_input,1,'gets current input stream').
p0(current_output,1,'gets current output stream').


p0(see,1,'focuses input on a file').
p0(seeing,1,'gets file name opened and set by see/1').
p0(seen,0,'close file opened by see/1').

p0(tell,1,'focuses output on a file').
p0(telling,1,'gets file name opened and set by tell/1').
p0(told,0,'closes file opened by tell/1').
p0(tell_at_end,1,'focuses output on file opened in append mode').

p0(see_at,1,'seeks a seekable file at a give offset (in bytes)').
p0(seeing_at,1,'retrieves position in current file opened by see/1').

p0(tell_at,1,'moves output file pointer to a given offset (in bytes)').
p0(telling_at,1,'retrieves output file position (in bytes)').

p0(file_search_path,1,'defines search path relative to BP_PATH (home of BinProlog) and PROLOG_PATH (home of user files)').
p0(file_extension_list,1,'defines default file extensions for find_file').
p0(file_library,2,'').

p0(make_file_name,4,'').
p0(find_file,2,'finds a file name on search path').
p0(exists_file,1,'true if file exists').
p0(see_or_fail,1,'opens a file if it exists, otherwise fails').
p0(flush,0,'').

p0(sread,3,'reads a term and a list of vars from a string (atom)'-x('f(X,Y)',_,_)).
p0(sread,2,'reads a term from a string (atom)').
p0(swrite,3,'writes a term with a liste of vars to a string (atom)').
p0(swrite,2,'writes a term to a string (atom)').

p0(bp_val,3,'unifies with 2 key indexed global logical variable').

p0(bu0,4,'').
p0(bu1,2,'').
p0(bu_ctr,2,'').

p0(current_op,3,'generates/check current op/3 operator definition(s)').
p0(op,3,
  'op(Pri,A,Op) defines an operator Op of priority Pri and associativity A').

%p0(pset,2,'').
%p0(pget,2,'').

p0(is_builtin,1,'recognizes a predicate head as a builtin'-x(var(_))).

p0(edit,2,'edit(E,F) edits with editor E, file F').
p0(my_edit,1,'').

p0(edit,0,'calls DOS editor edit on last compiled file').
p0(ed,0,'edits last compiled/consulted file with default editor and refreshes it in memory').
p0(textedit,0,'calls texedit editor on last compiled file').
p0(emacs,0,'calls emacs editor on last compiled file').
p0(pico,0,'calls pico editor on last compiled file').
p0(notepad,0,'calls notepad editor on last compiled file').
p0(vi,0,'calls vi editor on last compiled file').

p0(spawn,1,
  'spawns Goal in a new bp window on W95/NT PC and Unix/X').
p0(spawn,3,
  'spawns(Goal,Includes,TempFile): spawns a new bp window').

p0(co,0,'reconsults/recompiles last file').
p0(co,1,'reconsults using fast reader').
p0(current_user_file,1,'').
p0(ls,0,'list files under Unix').
p0(dir,0,'lists files under DOS').

p0(reboot,0,'regenerates BinProlog from its sources').
p0(remake,0,'').


p0(make,0,'').
p0(make,1,'').
p0(make,2,'').
p0(make,4,'').

p0(make,5,'').
p0(make0,4,'').

p0(qmake,1,'compiles Project to fast C code - for packaging as standalone executable').
p0(qmake,2,'(Project,Module): compiles to fast C code a project in Module: uses set_threshhold(12,60) before dooing cmake/2, for a good speed/code size ratio').

p0(cmake,0,'compiles BinProlog''s Prolog components to compact C code - for packaging as standalone executable').
p0(cmake,1,'compiles a Project to compact C code - for packaging as standalone executable').
p0(cmake,2,'(Project,Module): compiles to C a project with all clauses belonging to Module').
p0(kmake,0,'').
p0(kcmake,0,'').
p0(rmake,0,'').
p0(crmake,0,'').

p0(tboot,0,'').
p0(tboot,1,'').
p0(tboot,2,'').

p0(tmake,0,'').
p0(tmake,1,'').
p0(tmake,2,'').

p0(cboot,0,'').
p0(boot,0,'regenerates file wam.bp in BinProlog src directory').

p0(add_true,2,'').
p0(gc_read,1,'').
p0(gc_read_clause,1,'').
p0(call_body,1,'').
p0(expand_call_body,2,'').
p0(tr_body,2,'').
p0(char_in_cmd,2,'').
p0(show_code0,2,'').
p0(patch_it,4,'').
p0(lwrite,1,'').

p0(is_engine,1,'recognizes and integer as an engine handle').
p0((mode),1,'accepts mode declarations although we are not using them currently').

p0(module_call,2,'').
p0((module),1,'starts a module').
p0((begin_module),1,'').
p0((end_module),0,'ends current module').
p0((end_module),1,'ends module if current, signals erro if not').

p0((module),2,'starts a module specifying a list of visible predicates').
p0(is_module,1,'recognizes/generates a module name').
p0(modules,1,'returns a list of existing modules').
p0(current_module,1,'gets name of current module').
p0(module_predicate,3,'').
p0(module_name,3,'').
p0((public),1,'declares globally visible predicate Name/Arity').
p0(is_public,1,'checks predicate head if globally visible').

p0(gensym,2,'generates a new name based on arg 1').
p0(gensym_no,2,'generates a new number based on arg 1').
p0(init_gensym,1,'resets gensym for names based on arg 1').

p0(spy_goal,1,'').
p0(spy,1,'set spy point on goal, triggering trace when interpreted').
p0(spying,1,'checks what we are spying').
p0(nospy,1,'do not spy on Pred/Arity anymore').
p0(trace,0,'trace all predicates when interpreted').
p0(notrace,0,'do not trace predicates when interpreted').

p0(otherwise,0,'always succeeds').
p0(false,0,'always fails').

p0(call_ifdef,2,'calls if predicate head is defined, calls arg 2 if not').
p0(callable,1,'checks if predicate head is defined (callable)').

p0(default,2,'default that can be overriden if asserted'-
  x(host(X),X=localhost)
).

p0(set_default,1,'asserts arg 1 as default state for use by default/2').

p0(default_host,1,'returns default host for remote Linda server').
p0(set_host,1,'asserts IP adress or name of server host we want to talk to').
p0(host,1,'assumes default host for Linda server').

p0(default_this_host,1,'returns default IP address or hostname this machine').
p0(set_this_host,1,'asserts IP adress or name of this machine').
p0(this_host,1,'assumes default IP adress or name this machiner').

p0(default_port,1,'returns default port for remote Linda server').
p0(set_port,1,'asserts port number of the server we want to talk to').
p0(port,1,'assumes default port for Linda server').

p0(default_this_port,1,'returns default port to work as a server on').
p0(this_port,1,'assumes default port for to work as a server on').
p0(set_this_port,1,'asserts default port for to work as a server on').

p0(default_timeout,1,'returns default timeout').
p0(set_timeout,1,'asserts default timeout').
p0(timeout,1,'assumes default timeout').

p0(default_login,1,'returns default (nick)name for user').
p0(set_login,1,'asserts default (nick)name for user').
p0(login,1,'assumes default (nick)name for user').

p0(default_password,1,'returns default password for user').
p0(set_password,1,'sets default password for user').
p0(get_password,1,'gets default password for user').
p0(check_password,1,'checks that password matches default password').
p0(password,1,'assumes default password for user').

p0(copy_term,2,'returns a copy of arg 1 with fresh variables'-
 x(f(X,X,Y,Y),_)
).
p0(clone_term,3,
  'clone_term(Vs,T,CT) does copy_term(T,C) while keeping unchanged variables Vs - useful if doing things like setarg/3 on the new copy'-
   x([X,Z],f(X,Y,Y,Z),_)
).

p0(phrase,3,'(Axiom, ?InputChars, ?OutputChars): DCG evaluator, staring from Axiom'-x( ([a],[b]), [a,b|X], X) ).
p0(phrase,2,'(Axiom, ?InputChars): DCG evaluator, starting from Axiom, consuming/producing InputChars').

p0(nth_member,3,'retrieves N-th element of a list'-x(_,[a,b,c],_)).
p0(member_i,4,'').

p0(saved,2,'').
p0(stat_dict,2,'').
p0(c_threshold,1,'').
p0(c_threshold,2,'').
p0(set_c_threshold,1,
   'related to C generator: sets length K of WAM instruction block such that block larger than K will get compiled to C code').
p0(set_c_threshold,2,
   '(Min,Max): related to C generator: sets Min,Max length of WAM instruction block such that blocks between Min and Max size will get compiled to C code').
p0(set_c_trace,1,'').

p0(('C'),3,'DCG connect predicate').

p0(halt,0,'stops BinProlog').
p0(quit,0,'same as halt').
p0(exit,0,'same as halt').

p0(stop,0,'exits thread or process').

p0((:),2,'M:P calls predicate P hidden in module M').

p0(sock_read,2,'reads from a socket when size of the data is described by int before chars to be read').
p0(sock_readln,2,'reads from a socket until an end of line LF (ascii 10) or char 0 is found and discards possible previous CR (ascii13)').

p0(sock_write,2,'writes a string to a socket prefixed by its lenght').
p0(sock_writeln,2,'writes a string to a socket and adds an ascii 10 to the end').

p0(rpc_test,0,'tests rpc server and client with socket reuse').
p0(rpc_test,1,'tests rpc client with socket reuse for a given number of operations').
p0(rpc_handler,1,'(Goal): user-defined rpc handler - filters/calls Goal received on server').

p0(rpc_server,3,'Port,Password,Timout: runs Jinni compatible server with socket reuse').
p0(rpc_server,2,'Port,Password: runs Jinni compatible server with socket reuse').
p0(rpc_server,0,'runs Jinni compatible server with socket reuse on default port').

p0(service_loop,2,'ServiceSocket,Password:starts service loop on reusable ServiceSocket - works on server side with server/2').
% p0(disconnect,1,'disconnects a Socket'). % use close_socket

p0(start_rpc,0,'starts rpc client on default local reusable socket and port').
p0(start_rpc,3,'(Host,Port,Password): starts rpc client on local reusable socket').
p0(stop_rpc,0,'stops rpc client on local reusable socket').

p0(rpc,1,'(Query): calls server on current local reusable socket').
p0(rpc,3,'(Answer,Goal,Result): calls server on local reusable socket and gets back Result as the(Answer) or no').

p0(ask,5,'ask(ClientSocket,X,G,W,R): calls rpc server on on ClientSocket with query X goal G password W and gets result R back - supports socket reuse').
p0(stop_service,0,'stops server with socket reuse - acts on the server side').

% p0(stop_service,2,'(ClientSocket,PassWord): stops server with socket reuse connected to ClientSocket').
% p0(start_service,3,'(Host,Port,PassWord): starts client with socket reuse connected server on Host, Port').

p0(show_defaults,0,'show default values of some system variables').
p0(hide_default,1,'makes unavailable a default value').
p0(show_default,1,'makes available a default value').

p0(hostname,1,'the name of current host, if detected, localhost if not').
p0(detect_ip_addr,1,'the ip address of current host, if detected, that of localhost if not').
p0(detect_user,1,'guesses the user from environment information').

p0(run_server,0,'runs foreground server on localhost (default port) for Jinni clients').
p0(run_server,1,'runs foreground server on Port to provide services to Jinni clients').
p0(run_server,2,'(Port,Password): runs server on Port, Password required from clients').
p0(run_server,6,'(Port,Password,Heap,Stack,Trail,Timeout): runs server with specified service parameters').

p0(remote_run,1,'runs Goal on remote server using default password').
p0(remote_run,3,'(Host,Port,Goal): runs Goal on rmote server at Host, Port with default password').
p0(remote_run,6,'(Host,Port,Answer,Goal,Password,Reply): runs Goal on server at Host, Port with given Password and returns Reply. However, if you do let(where,here) before calling it, a local goal is called instead.').

p0(handle_service,2,'handles a Jinni service S with password P - always succedes').
p0(answer_one_query,2,'handles a Jinni service S with password P').

p0(term_decoder,2,'(Encrypted,Plain): user provided encoder for secure communications - works on lists of ascii codes').
p0(term_encoder,2,'(Plain,Encrypted): user provided decoder for secure communications - works on list of ascii codes').

p0(is_interactive,0,'checks if toplevel is interactive - use interactive/1 with yes/no to set it the way you want it').

% Jinni compatibility layer 
p0(eq,2,'unifies arg 1 and arg 2, like =').
p0(and,2,'conjunction, like comma').
p0(compute,4,'applies Op to arg 2 and arg 3 giving a result').
p0(println,1,'synchronized printing of a term on a line').
p0(read_line,1,'reads a line into a constant').

p0(read_words,1,'reads a line into a list of words').
p0(write_words,1,'write list of words to a space separated line').

p0(near_match,2,'matches 2 lists of chars').


% data conversion

p0(to_lower_char,2,'(Upper,Lower): converts a char to lower case').
p0(to_upper_char,2,'(Lower,Upper): converts a char to upper case').
p0(to_lower_chars,2,'converts a list of chars to lower case').
p0(to_upper_chars,2,'converts a list of chars to upper case').
p0(char_type,2,'returns the type of char code: upper,lower,digit, etc. ').

% info and help predicates 

p0(about_to_bp_comment_3,1,'').
p0(to_bp_comment,3,'gets a predicate of form p_n_info("...") and its first arg, holding comments for p').
p0(bp_info,2,'keeps basic help info on BinProlog builtins').
p0(bp_info,3,'').
p0(extract_ex,3,'').
p0(info,0,'generates info on predicates with examples').
p0(info,1,'generates info and examples of use for predicate Pred/Arity').
p0(show_info,2,'generates components of info/1 output').
p0(has_info,1,
  'checks/generates predicates Pred/Arity for which info is available').

p0(timed_call,4,
  '(Answer,Goal,Timeout,Result) - calls and possibly stops Goal after Timout secs').


p0(('#<'),3,'(Xs): sets the dcg token list to be Xs').
p0(('#>'),3,'(Xs): unifies current dcg token list with Xs').
p0(('#:'),3,'(X): matches X against current dcg token').
p0(('#+'),3,'(X): adds linear assumption +(X) to be consumed at most once, by a #- operation').
p0(('#*'),3,'(X): adds intuitionisic assumption *(X) to be used indefinitely by #- operation').
p0(('#='),3,'(X): unifies X with any matching existing or future +(X) linear assumptions').
p0(('#-'),3,'(X): consumes +(X) linear assumption or matches *(X) intuitionistic assumption').
p0(('#?'),3,'(X): matches +(X) or *(X) assumptions without any binding').

p0(the,3,'the(X,G,R) first answer R=the(X) or R=no, without binding G').
p0(the,2,'defined as the(X,G):-the(X,G,the(X))').
p0(the,1,'defined as the(G):-the(G,G)').


%p0(mobile_call,1,'calls goal, gets ready to handle uncaught exception and wraps goal for thread mobility').
%p0(capture_cont_for,1,'captures current continuation for Goal in arg 1').
%p0(call_with_cont,1,'calls closure in arg 1 with captured continuation').

p0(strip_cont,3,'').
p0(add_cont,3,'').

p0(throw,1,'ISO Prolog exception operator: throws a term to be caught by a matching catch').
p0(catch,3,'ISO Prolog exception operator: executes arg 1 and if it catches arg 2, it executes arg 3').
p0(catch0,4,'').
p0(get_cont,1,'captures current continuation, usually an cyclic term').
p0(call_cont,1,'calls arg 1 as current continuation').
p0(swap_cont,2,'calls arg 1 as cc and returns cc in arg 2').
p0('$catch_looking_for_throw',1,'(CatchThrowData): continuation marker used by catch/throw').
p0('$process_catch',3,'processes data sent by catch for throw').
p0('throw_with_cont',2,'works for throw/1: used to rethrow with new continuation').

p0(callj,3,'callj(X,G,R) calls Jinni in Twin Prolog with first answer R=the(X) or R=no, without binding G').
p0(callj,2,'callj(G,R) calls Jinni in Twin Prolog with first answer R=the(G) or R=no, without binding G').
p0(callj,1,'callj(G) calls Jinni in Twin Prolog with first answer binding G and fails if no answers are found').

p0('@',2,'Hilog call predicate').
p0('?',1,'simple call tracer').
p0(count_answers,2,'counts answers to a goal').

p0(pushTerm,2,'writes term to array of ints').
p0(popTerm,2,'reads term from array of ints').
  
p0(cserver,1,'runs simple remote server at given port').
p0(cserver,0,'runs simple remot server at default port').

p0(shell_server,1,'runs simple shell server at given port').
p0(shell_server,0,'runs simple shell server at default port').
p0(traceln,1,'prints messages on default console').
p0(hi,0,'prints hi on default console').

% ---------- tools to generate builtins.pl -------------- %

make_ps:-
  test_ps,
  make_ps0.

test_ps:-
  findall(F/N,p0(F,N,_),Xs),length(Xs,L1),sort(Xs,Ys),length(Ys,L2),
  (L2=:=L1->true
  ; tell(user),write('redundant relation in p0/3'),
    nl,halt
  ).

make_ps0:-
  p0(F,N,X),
  functor(P,F,N),
  pp_c(bu1(P,X)),
  fail.
make_ps0:-
  nl.

get_all_bs(Bs):-
  G0=(b0(F/N,X,Y,Z),N1 is N+1,functor(B,F,N1)),
  G=b0(B,X,Y,Z),
  findall(G,G0,Bs).

filter_b(Class,All,N):-
  arg(1,Class,I),
  findall(I-B,
    ( member_i(OldB,All,0,_),
      rewrite_b(Class,OldB,B)
    ),
  Bs),
  length(Bs,N),
  ( member_i(I-B,Bs,0,I),
    pp_c(B),
    fail
  ; nl
  ).

rewrite_b(n_inline(N),b0(B,simple,in_body,X), bu0(B,N,in_body,X)).
rewrite_b(n_arith(N),b0(B,arith(K),in_body,X),bu0(B,arith(N,K),in_body,X)).
rewrite_b(n_builtin(N),b0(B,simple,in_head,X),bu0(B,N,in_head,X)).

pp_c(C):-portray_clause(C). % had a bug with writing out anonymous vars

make_builtins(Start,Info):-
  Info=[P1,P2,P3,P4],
  get_all_bs(All),
  P1=n_inline(A1),filter_b(P1,All,N1),
  P2=n_arith(A2),filter_b(P2,All,N2),
  P3=n_builtin(A3),filter_b(P3,All,N3),
  P4=n_nop(A4),
  A1=Start,
  A2 is A1+N1,
  A3 is A2+N2,
  A4 is A3+N3,
  pp_ctr(P1),
  pp_ctr(P2),
  pp_ctr(P3),
  pp_ctr(P4),
  nl.

pp_ctr(P):-P=..[F,X],NewP=bu_ctr(F,X),pp_c(NewP).
  
make_pl(N,Info):-
  BU='builtins.pl',
  write(making(BU)),nl,
  tell(BU),
  make_builtins(N,Info),
  make_ps,
  gen_tables,
  told.

go(Start):-
  make_pl(Start,Info),
  make_h(Info),
  check_headers.

make_h(Info):-
  DEF='defs.h',PROF='prof.h',
  Info=[n_inline(First),n_arith(Arith),n_builtin(Bu),n_nop(Nop)],
  Last is Nop-1,
  write([inline=First,arith=Arith,in_body=Bu,last=Last]),nl,
  write(making(DEF)),nl,
  tell(DEF),
  ( make_defs(First,Arith,Bu,Last)
  ; tell(user),write(failing_on(DEF)),nl,tell(DEF)
  ),
  !,
  make_stats,
  make_vals,
  make_stamp,
  !,
  told,
  write(making(PROF)),nl,
  tell(PROF),
  (   make_names(Info)
    ; tell(user),write(failing_on(PROF)),nl,tell(DEF)
  ),
  !,
  told.

make_defs(First,Arith,Bu,Last0):-
  number_bu(First,Last,N-X),
  write('#define '),write(X),write(' '),write(N),nl,
  N=:=Last,
  !,
  Last=:=Last0,
  nl,write('#define INLINE '),write(First),nl,
  write('#define ARITH '),write(Arith),nl,
  write('#define BUILTIN '),write(Bu),nl,
  write('#define LAST_BUILTIN '),write(Last),nl.

make_stats:-
        nl,
        stat_dict(Name,Val),name(Name,L),
        to_upper(L,L1),
        lappend("STAT_",L1,L2),
        name(SName,L2),
        write('#define '),write(SName),write(' '),write(Val),nl,
        fail.
make_stats:-nl.

make_names(Info):-
  Info=[n_inline(Max)|_],
  write('#if TRACE>0 || PROF '),nl,
  write('char *bu_name[]={'),nl,
  number_bu(Max,Last,N-X),
  write('"'),write(X),write('",'),nl,
  N=:=Last,
  !,
  write('"LAST_BUILTIN"};'),nl,write('#endif'),nl,nl.

number_bu(First,Last,N-X):-
  bagof(B,bname(B),Bs),
  length(Bs,L),!,Last is First+L-1,
  member_i(X,Bs,First,N).

to_upper1(X,Y):-[A]="a",[Z]="z",X>=A,X=<Z,[AA]="A",!,
  Y is AA+(X-A).  
to_upper1(X,X).

to_upper([],[]).
to_upper([X|Xs],[Y|Ys]):-
  to_upper1(X,Y),
  to_upper(Xs,Ys).

b_idiom(+,"PLUS").
b_idiom(-,"SUB").
b_idiom(*,"MUL").
b_idiom(//,"DIV").
b_idiom(/,"FDIV").
b_idiom(put,"PUT0").

b_idiom(<<,"LSHIFT").
b_idiom(>>,"RSHIFT").
b_idiom(/\,"L_AND").
b_idiom(\/,"L_OR").
b_idiom(#,"L_XOR").
b_idiom(\,"L_NEG").

to_bname(F,U):-b_idiom(F,U),!.
to_bname(F,U):-name(F,L),to_upper(L,U).

bname(Name):-
  [X]="_",
  b0(F/N,_,_,_),name(N,Arity),
  to_bname(F,U),
  lappend(U,[X|Arity],List),
  name(Name,List).

lappend([],Ys,Ys).
lappend([A|Xs],Ys,[A|Zs]):-
        lappend(Xs,Ys,Zs).

lmember(X,[X|_]).
lmember(X,[_|Xs]):-lmember(X,Xs).

range(Min,Min,Max):-Min=<Max.
range(I,Min,Max):-Min<Max,Min1 is Min+1,range(I,Min1,Max).

% char tables

maj(Ch):- 
	Ch >= 65, Ch =< 90
;	Ch=95
;	latin1_maj(Ch).

min(Char):- 
	Char >= 97, Char =< 122
;	latin1_min(Char).

num(Char) :- Char >= 48, Char =< 57.

% support for latin1 - thanks to Ulrich Neumerkel

latin1_maj(Ch) :- 
	Ch >= 192, Ch =< 214
;	Ch >= 216, Ch =< 222. 

latin1_min(Ch) :- 
	Ch >= 223, Ch =< 246
;	Ch >= 248, Ch =< 254.

an(Char):- 
	min(Char)
;	maj(Char)
;	num(Char).

spec(35).
spec(36).
spec(38).
spec(42).
spec(43).
spec(45).
spec(46).
spec(47).
spec(58).
spec(60).
spec(61).
spec(62).
spec(63).
spec(64).
spec(92).
spec(94).
spec(96).
spec(126).

terminator(10).
terminator(13).
terminator(-1).

% CHAR=MIN|MAJ|NUM|LMAJ|LMIN|SPEC|TERMIN|OTHER

gen_table(Arg,Generator,Name,Target):-
  lsymcat(is,Name,IsName),
  Source=..[Name,Arg],
  Target=..[IsName,Arg],
  Generator,
  Source.
  
lsymcat(Op,Type,OpType):-	
	[Link]="_",
	name(Op,Pref),
	name(Type,Suf),
	lappend(Pref,[Link|Suf],Chars),
	!,
	name(OpType,Chars).

gen_tables:-
  nl,
  lmember(Name,[maj,min,num,an,spec,terminator]),nl,
  gen_table(Char,range(Char,-1,254),Name,Target),
  write(Target),write('.'),nl,
  fail
; nl.

make_vals:-
  nl,
  make_one_val,
  fail
; write('}'),nl,nl.


make_one_val:-
  write('#define MAKE_BP_VALS() {\'),nl,
  bp_val(V,H,I),
  functor(H,F,N),
  write('  BP_VAL(INPUT_STRING("'),write(V),
  write('"),new_func("'),write(F),write('",'),write(N),
  write('),INPUT_INT('),write(I),write('));\'),nl.
  

make_stamp:-
   (prolog_flag(is_prolog,binprolog),call_ifdef(otime(T),fail)->true
   ; T is 0
   ),
   nl,write('#define RTIME_STAMP '),write(T),nl,nl.

check_headers:-
    prolog_flag(is_prolog,binprolog),
    write('Unused, possibly new public predicates: '),nl,
    ( b0(F,N,_,_)
    ; p0(F,N,_)
    ),
    functor(P,F,N),
    \+ is_compiled(P),
    write('unused, possibly new predicate: '),write(F/N),nl,
    fail.
check_headers.
