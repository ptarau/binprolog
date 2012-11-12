bu0(fail(_A),0,in_body,'always fails').
bu0(cwrite(_A,_B),1,in_body,'basic but quick C-version of write/1').
bu0(cnl(_A),2,in_body,'writes a new line').
bu0(var(_A,_B),3,in_body,'true if currently an unbound variable').
bu0(nonvar(_A,_B),4,in_body,'true if currently instantiated').
bu0(integer(_A,_B),5,in_body,'true if an integer').
bu0(atomic(_A,_B),6,in_body,'true if an integer or symbolic constant').
bu0(is_compiled(_A,_B),7,in_body,'true if head of a compiled predicate').

bu0(+(_A,_B,_C,_D),arith(0,1),in_body,add-x(10,3,_E)).
bu0(-(_A,_B,_C,_D),arith(1,1),in_body,subtract-x(10,3,_E)).
bu0(*(_A,_B,_C,_D),arith(2,1),in_body,multiply-x(10,3,_E)).
bu0(mod(_A,_B,_C,_D),arith(3,1),in_body,modulo-x(10,3,_E)).
bu0(//(_A,_B,_C,_D),arith(4,1),in_body,'integer division'-x(10,3,_E)).
bu0(/(_A,_B,_C,_D),arith(5,1),in_body,division-x(10,3,_E)).
bu0(random(_A,_B),arith(6,1),in_body,'returns a random integer'-x(_C)).
bu0(get0(_A,_B),arith(7,1),in_body,'reads a char as an ascii code').
bu0(put(_A,_B),arith(8,0),in_body,'writes and ascii code as a char'-x(99)).
bu0(less(_A,_B,_C),arith(9,0),in_body,'arithmetic comparison').
bu0(greater(_A,_B,_C),arith(10,0),in_body,'arithmetic comparison').
bu0(less_eq(_A,_B,_C),arith(11,0),in_body,'arithmetic comparison').
bu0(greater_eq(_A,_B,_C),arith(12,0),in_body,'arithmetic comparison').
bu0(arith_eq(_A,_B,_C),arith(13,0),in_body,'arithmetic comparison').
bu0(arith_dif(_A,_B,_C),arith(14,0),in_body,'arithmetic comparison').
bu0(<<(_A,_B,_C,_D),arith(15,1),in_body,'left shifts arg 1 by arg 2 bits'-x(1,5,_E)).
bu0(>>(_A,_B,_C,_D),arith(16,1),in_body,'right shifts arg 1 by arg 2 bits'-x(16,2,_E)).
bu0(/\(_A,_B,_C,_D),arith(17,1),in_body,'bitwise AND'-x(1,2,_E)).
bu0(\/(_A,_B,_C,_D),arith(18,1),in_body,'bitwise OR'-x(1,2,_E)).
bu0(#(_A,_B,_C,_D),arith(19,1),in_body,'bitwise XOR'-x(1,2,_E)).
bu0(\(_A,_B,_C,_D),arith(20,1),in_body,'bitwise or of first arg with bitwise complement of second'-x(0,2,_E)).
bu0(compare0(_A,_B,_C,_D),arith(21,1),in_body,'').
bu0(arg(_A,_B,_C,_D),arith(22,1),in_body,'arg(I,T,X) extracts arg I of term T to be unified with X'-x(2,f(a,b),_E)).
bu0(setarg(_A,_B,_C,_D),arith(23,0),in_body,'backtrackable: setarg(I,T,X) replaces arg I of T with X'-x(2,f(a,b),c)).
bu0(change_arg(_A,_B,_C,_D),arith(24,0),in_body,'destructive: change_arg(I,T,X) replaces arg I of T with X'-x(2,f(a,b),c)).
bu0(def(_A,_B,_C,_D),arith(25,0),in_body,'').
bu0(rm(_A,_B,_C),arith(26,0),in_body,'').
bu0(set(_A,_B,_C,_D),arith(27,0),in_body,'').
bu0(val(_A,_B,_C,_D),arith(28,1),in_body,'').
bu0(lval(_A,_B,_C,_D),arith(29,1),in_body,'backtrackable: lval(K1,K2,V) associates V to keys K1 and K2'-x(a,b,f(_E))).
bu0(symcat(_A,_B,_C,_D),arith(30,1),in_body,'makes new identifier from arg 1 and arg 2'-[x(a,b,_E),x(a,1,_F)]).
bu0(namecat(_A,_B,_C,_D,_E),arith(31,1),in_body,'concatenates 3 names'-x(a,:,b,_F)).
bu0(deep_hash(_A,_B,_C,_D,_E),arith(32,1),in_body,'(Key,Depth,Mod) computes hashvalue of Key modulo Mod, up to max recursion Depth'-[x(f(a),5,0,_F),x(f(b),1,32,_G)]).
bu0(gval(_A,_B,_C),arith(33,1),in_body,'').
bu0(hval(_A,_B,_C),arith(34,1),in_body,'').
bu0(tval(_A,_B,_C,_D),arith(35,1),in_body,'').
bu0(tlet(_A,_B,_C,_D),arith(36,0),in_body,'').
bu0(get_asserted(_A,_B,_C),arith(37,1),in_body,'').
bu0(array_set(_A,_B,_C,_D),arith(38,0),in_body,'sets array element').
bu0(array_get0(_A,_B,_C,_D),arith(39,1),in_body,'gets  array element').
bu0(array_get(_A,_B,_C,_D),arith(40,1),in_body,'gets and dereferences array element').
bu0(make_array(_A,_B,_C),arith(41,1),in_body,'creates an array').
bu0(destroy_array(_A,_B),arith(42,0),in_body,'frees an array').
bu0(vget_int0(_A,_B,_C),arith(43,1),in_body,'gets from arg 1 - an int* C variable - to a 28 bit int').
bu0(vset_int0(_A,_B,_C),arith(44,0),in_body,'sets arg 1 - an int* C variable - to a 28 bit int').
bu0(addq0(_A,_B,_C),arith(45,0),in_body,'').
bu0(pushq0(_A,_B,_C),arith(46,0),in_body,'').
bu0(popq0(_A,_B,_C),arith(47,1),in_body,'').
bu0(dcg_connect(_A,_B),arith(48,1),in_body,'handles a terminal symbol in HAGs, as [a] in DCGs').
bu0(list2term(_A,_B,_C),arith(49,1),in_body,'').
bu0(term2list(_A,_B,_C,_D,_E),arith(50,1),in_body,'').
bu0(call_external(_A,_B,_C,_D),arith(51,1),in_body,'args: StringToStringFunctionAddress,InputChars,OuputChars - calls a C function').
bu0(add_instr(_A,_B,_C,_D,_E),arith(52,0),in_body,'').
bu0(det_append0(_A,_B,_C,_D),arith(53,1),in_body,'').
bu0(copy_term(_A,_B,_C,_D),arith(54,1),in_body,'').
bu0(unify_to(_A,_B,_C),arith(55,1),in_body,'').
bu0(bb_list0(_A,_B,_C,_D),arith(56,1),in_body,'').
bu0(older_file(_A,_B,_C),arith(57,0),in_body,'true if arg 1 is a file older than arg 2').
bu0(seeing_telling(_A,_B,_C),arith(58,1),in_body,'').
bu0(see_tell(_A,_B,_C),arith(59,0),in_body,'').
bu0(seen_told(_A,_B),arith(60,0),in_body,'').
bu0(seeing_telling_at(_A,_B,_C),arith(61,1),in_body,'').
bu0(see_tell_at(_A,_B,_C),arith(62,0),in_body,'').
bu0(string_op(_A,_B,_C,_D),arith(63,1),in_body,'').
bu0(op0(_A,_B,_C,_D),arith(64,0),in_body,'').
bu0(term_append(_A,_B,_C,_D),arith(65,1),in_body,'efficiently concatenates 2 terms'-x(f(a,b),g(c,d),_E)).
bu0(float_fun2(_A,_B,_C,_D,_E),arith(66,1),in_body,'').
bu0(float_fun(_A,_B,_C,_D),arith(67,1),in_body,'').
bu0(input_float(_A,_B,_C,_D,_E),arith(68,1),in_body,'').
bu0(strip_cont0(_A,_B,_C),arith(69,1),in_body,'').
bu0(dcg_def(_A,_B),arith(70,0),in_body,'backtrackable: sets current Assumption Grammar stream - usually a hidden DCG list').
bu0(dcg_val(_A,_B),arith(71,1),in_body,'backtrackable: retrieves current Assumption Grammar stream - usually a hidden DCG list').
bu0(dcg_tell(_A,_B),arith(72,0),in_body,'switches to hidden DCG-stream number K (form 0 to MAXDCG=255)').
bu0(dcg_telling(_A,_B),arith(73,1),in_body,'retrieves which hidden DCG-stream we a re processing'-x(_C)).
bu0(open_stream(_A,_B,_C,_D,_E),arith(74,1),in_body,'(Type,FileOrCmd,ReadWriteAppend,?StreamID) opens various streams').
bu0(close_stream(_A,_B,_C),arith(75,0),in_body,'(Type,StreamID) closes various streams').
bu0(fgetc(_A,_B,_C),arith(76,1),in_body,'fgetc(IntegerStreamNo,CharCode) inputs a char code from a C stream').
bu0(fputc(_A,_B,_C),arith(77,0),in_body,'fputc(IntegerStreamNo,CharCode) outputs a char code to a C stream').
bu0(fflush(_A,_B),arith(78,0),in_body,'fflush(IntegerStreamNo) flushes a C-stream').
bu0(fsize(_A,_B,_C),arith(79,1),in_body,'returns the size of the file associated to a C stream, in bytes').
bu0(unix_argc(_A,_B),arith(80,1),in_body,'gets cmd line arg counter'-x(_C)).
bu0(unix_argv(_A,_B,_C),arith(81,1),in_body,'gets a cmd line arg from 0 to argc'-x(0,_D)).
bu0(unix_getenv(_A,_B,_C),arith(82,1),in_body,'gets an environment variable').
bu0(unix_access(_A,_B,_C),arith(83,0),in_body,'checks if arg1 (a path+file) is accessible in arg 2 (integer) mode').
bu0(unix_cd(_A,_B),arith(84,0),in_body,'changes local dir to arg 1').
bu0(unix_fork(_A,_B),arith(85,1),in_body,'starts child process with Unix fork').
bu0(unix_pid(_A,_B),arith(86,1),in_body,'returns process id of current process').
bu0(unix_kill(_A,_B,_C),arith(87,0),in_body,'sends signal arg 1 to process with pid arg 2').
bu0(create_engine(_A,_B,_C,_D,_E),arith(88,1),in_body,'create_engine(Heap,Stack,Trail,IntHandle) creates an engine IntHandle').
bu0(destroy_engine(_A,_B),arith(89,0),in_body,'destroy_engine(E) frees memory of engine E (an integer)').
bu0(load_engine(_A,_B,_C,_D),arith(90,0),in_body,'load_engine(E,Goal,Answer) prepares engine E to execute Goal/Answer').
bu0(ask_engine(_A,_B,_C),arith(91,1),in_body,'ask_engine(E,X) retrieves from engine E (a copy of) answer X').
bu0(list_engines(_A,_B),arith(92,1),in_body,'lists available engine handles').
bu0(current_engine_addr(_A,_B),arith(93,1),in_body,'returns current engine handle').
bu0(get_engine_prop(_A,_B,_C,_D),arith(94,1),in_body,'args: Engine,PropertyNo,Val').
bu0(ask_thread(_A,_B,_C),arith(95,1),in_body,'(E,R): asks an answer of engine E on a new thread R').
bu0(tsync_op(_A,_B,_C,_D),arith(96,0),in_body,'same as thread_operation(Op, MutexOrParam, ActionOrValue) various thread synchronization operations').
bu0(thread_exit(_A,_B),arith(97,0),in_body,'exits a thread').
bu0(thread_join(_A,_B),arith(98,0),in_body,'joins thread').
bu0(current_thread(_A,_B),arith(99,1),in_body,'gets thread id number of current thread').
bu0(untrail_to(_A,_B),arith(100,0),in_body,'unwinds the trail up to a choice point').
bu0(get_neck_cut(_A,_B),arith(101,1),in_body,'gets the choice point as an integer').
bu0(override(_A,_B,_C,_D),arith(102,0),in_body,'overrides a compiled predicate - to be used with care').
bu0(random_seed(_A,_B),arith(103,0),in_body,'initializes random/1 with an integer, uses clock if 0').
bu0(member_scan(_A,_B,_C,_D),arith(104,1),in_body,'finds first element without unifying to it'-member_scan(s(_E),[1,s(s(_F)),2],_G)).
bu0(cmember_scan(_A,_B,_C,_D),arith(105,1),in_body,'').
bu0(cdel_scan(_A,_B,_C,_D),arith(106,1),in_body,'').
bu0(push_code(_A,_B),arith(107,1),in_body,'moves code compiled in workspace to kernel and returns top of code area - used by pc/0').
bu0(system0(_A,_B,_C),arith(108,1),in_body,'').
bu0(new_name(_A,_B,_C),arith(109,1),in_body,'returns a new name based on arg 1').
bu0(new_client(_A,_B,_C,_D),arith(110,1),in_body,'from a (host,port) to a client').
bu0(new_server(_A,_B,_C),arith(111,1),in_body,'opens on a port a new server').
bu0(new_service(_A,_B,_C,_D),arith(112,1),in_body,'from (server,timeout) to a service').
bu0(peer_addr(_A,_B,_C),arith(113,1),in_body,'gets address of peer connected to socket').
bu0(peer_port(_A,_B,_C),arith(114,1),in_body,'gets port of peer connected to socket').
bu0(close_socket(_A,_B),arith(115,0),in_body,'closes a server, service or client').
bu0(sock_readln(_A,_B,_C,_D),arith(116,1),in_body,'reads from a socket, a line').
bu0(sock_writeln(_A,_B,_C,_D),arith(117,0),in_body,'writes to a socket, a line').
bu0(sock_read(_A,_B,_C,_D),arith(118,1),in_body,'reads from a socket, a string').
bu0(sock_write(_A,_B,_C,_D),arith(119,0),in_body,'writes to a socket, a string').
bu0(sock2file(_A,_B,_C),arith(120,0),in_body,'reads from a socket, to a file').
bu0(file2sock(_A,_B,_C),arith(121,0),in_body,'writes to a socket, from a file').
bu0(sleep(_A,_B),arith(122,0),in_body,'waits arg 1 seconds').
bu0(host2ip(_A,_B,_C),arith(123,1),in_body,'converts a host name to an IP address').
bu0(qprint(_A,_B),arith(124,0),in_body,'prints out a clause such that a variant of it can be always read back').
bu0(term_store_op(_A,_B,_C,_D,_E),arith(125,1),in_body,'API for external term storage').
bu0(new_builtin(_A,_B,_C,_D),arith(126,1),in_body,'sample 3 arg user added builtin - used for the C interface').
bu0(halt(_A,_B),arith(127,0),in_body,'stops Prolog with given return code when used in main thread- or halts current thread').

bu0(true(_A),0,in_head,'always succeeds').
bu0(call(_A,_B),1,in_head,'executes (atomic!) arg 1').
bu0(abort0(_A),2,in_head,'').
bu0(restart0(_A),3,in_head,'').
bu0(functor(_A,_B,_C,_D),4,in_head,'builds or decomposes a coumpound term'-[x(f(a,b),_E,_F),x(_G,f,3),x(f(a),f,1)]).
bu0(name(_A,_B,_C),5,in_head,'bidirectional: converts atomic to/from list of chars'-[x(hello,_D),x(_E,[98,121,101])]).
bu0(load0(_A,_B),6,in_head,'').
bu0(stat0(_A,_B,_C,_D),7,in_head,'').
bu0(list_asm(_A,_B,_C,_D),8,in_head,'').
bu0(bb_reset(_A,_B),9,in_head,'cleans up and resizes to at least arg 1 bytes compound term area of the blackboard').
bu0(garbage_collect(_A),10,in_head,'performs heap gc now').
bu0(profile(_A),11,in_head,'in specialy compiled profiler mode prints out info accumulated so far').
bu0(member_entry(_A,_B,_C),12,in_head,'').
bu0(for_entry(_A,_B,_C,_D),13,in_head,'').
bu0(return0(_A,_B),14,in_head,'returns a term from an engine left in a state ready to resume').
bu0(fcall(_A,_B,_C,_D),15,in_head,'calls a list to list function: to be implemented').
bu0(if0(_A,_B,_C,_D),16,in_head,'').

bu_ctr(n_inline,66).
bu_ctr(n_arith,74).
bu_ctr(n_builtin,202).
bu_ctr(n_nop,219).

bu1(!,'succeeds like true/0, but removes pending choices in calls at its LEFT and makes things look as if this were the LAST clause of the predicate').
bu1(get_deep_cut(_A,_B),'gets a choice point address, used with 1 arg only').
bu1(cut_to(_A),'cuts to an int in arg 1, a choicepoint address').
bu1(translate_clause(_A,_B),'').
bu1(translate_def(_A,_B),'').
bu1([_A|_B],'').
bu1(end_of_file,'Prolog atom returned by read when at the end of a file').
bu1(pc,'pushes code compiled into the workspace to the persistent kernel').
bu1(restart,'cleans up data areas and reinitializes symbol tables').
bu1(abort,'returns to toplevel').
bu1(init_io,'intialises fast C-based IO routines').
bu1(heap_size(_A),'').
bu1(stack_size(_A),'').
bu1(trail_size(_A),'').
bu1(gc,'enables heap gc').
bu1(nogc,'disables heap gc').
bu1(gc_status(_A),'shows if heap gc is enabled or not').
bu1(dynbbgc,'makes blackboard dynamic with gc on').
bu1(bbgc,'enables blackboard gc ').
bu1(nobbgc,'disables blackboard gc').
bu1(bbgc_status(_A),'shows if blackboard gc is enabled or not').
bu1(bb_gc,'performs blackboard gc now, if enabled').
bu1(bb_gc0,'performs blackboard gc now, in this engine').
bu1(quiet(_A),'gets/sets level of "quietness"').
bu1(current_engine_id(_A),'returns a unique id associated to an engine at creation time').
bu1(current_engine(_A),'gets the unique id of the current engine').
bu1(get_engine_id(_A,_B),'(+Engine,-Id) gets the unique id associated to an engine at creation time').
bu1(create_engine(_A),'makes a new engine or reuses a dead one').
bu1(create_new_engine(_A),'creates an engine as big as the current one').
bu1(open_engine(_A,_B,_C),'open_engine(G,X,E) creates an engine E ready to execute goal G with answer X').
bu1(new_engine(_A,_B,_C),'new_engine(X,G,E) creates an engine E ready to execute goal G with answer X').
bu1(reuse_engine(_A,_B,_C),'reuse_engine(X,G,E) initializes engine E with goal G and answer pattern X').
bu1(get(_A,_B),'get(E,A) returns a new answer A=the(...) from engine E or returns no if no (more) answers exist').
bu1(stop(_A),'stops and frees resources held by an engine (may happen automaticaly if an engine fails)').
bu1(return(_A),'returns data from an engine as if it were an answer - such that the engine can be resumed with get/2 to execute the next goal').
bu1(this_engine(_A),'gets a handle to the current engine').
bu1(to_engine(_A,_B),'(E,T) sends to engine E a term T, and fails if E has an empty message box').
bu1(from_engine(_A),'(T) tries to take a term T from the message box of this engine and sets the message box empty').
bu1(element_of(_A,_B),'(Engine,Answer: backtracks over the answers of a fluent (usually an engine)').
bu1(show_engine,'if debugmes/1 is on (as with quiet(1)), shows params of current engine').
bu1(list_engines,'prints out the list of active engines').
bu1(clean_up_engines(_A),'internal predicate').
bu1(clean_up_engines,'frees resources used by all engines except main').
bu1(clean_up_dead_engines,'frees resources used by dead engines').
bu1(has_threads,'succeeds if threads available on this platform').
bu1(thread_exit,'exits a thread').
bu1(lock_thread_guard(_A),'locks thread guard created with new_thread_guard').
bu1(unlock_thread_guard(_A),'unlocks thread guard').
bu1(try_unlock_thread_guard(_A),'try to unlock a thread guard').
bu1(try_unlock_thread_guard(_A,_B),'try to unlock a thread guard for a specified timeout').
bu1(thread_wait(_A),'waits on guard until notified by thread_notify').
bu1(thread_notify(_A),'notifies a thread waiting on guard with thread_wait').
bu1(thread_notify_all(_A),'notifies all threads waiting on guard with thread_wait').
bu1(thread_timed_wait(_A,_B),'waits on first arg Guard second arg msec or until notified').
bu1(thread_cancel(_A),'terminates (cancels) thread given in arg 1').
bu1(thread_resume(_A),'resumes execution of suspended thread').
bu1(thread_suspend(_A),'suspends execution of thread').
bu1(get_engine_thread(_A,_B),'gets from an Engine the thread it is running on').
bu1(current_engine_thread(_A),'gets from the current Engine the thread it is running on').
bu1(bg(_A,_B,_C,_D,_E,_F,_G),'bg(Goal,H,S,T,-Thread,-EngineAddr,-EngineID): runs goal in background on engine with given heap,stack,trail').
bu1(bg(_A,_B,_C,_D),'bg(Goal,Thread,EngineAddr,EngineID): runs goal in background on new engine - with unique EngineID').
bu1(bg(_A,_B,_C),'bg(Goal,Thread,EngineAddr): runs goal in background if threads are available').
bu1(bg(_A,_B),'runs Goal in new background thread, which is returned in second arg ').
bu1(bg(_A),'runs Goal in background thread - you can set engine size like in heap(500)=>bg(...)').
bu1(begin_critical,'begin serialized execution - enters critical region').
bu1(end_critical,'ends serialized execution - exits critical region').
bu1(put_critical(_A,_B),'(Guard,Data): updates mutex Guard-protected Data on blackboard').
bu1(get_critical(_A,_B),'(Guard,Data): accesses mutex Guard protected Data on blackboard').
bu1(synchronize_on(_A,_B,_C),'wraps Goal for sync on given mutex for serialized execution and returns true or fail').
bu1(synchronize_on(_A,_B),'wraps Goal for sync on given mutex for serialized execution').
bu1(synchronize(_A,_B),'wraps Goal for serialized execution and returns true or fail').
bu1(synchronize(_A),'wraps Goal in arg 1 for serialized execution').
bu1(sdebug(_A),'emits mt-safe synchronized debug message').
bu1(new_thread_guard(_A),'returns a new free thread guard from pool').
bu1(free_thread_guard(_A),'gives back to pool thread guard in arg 1').
bu1(local_out(_A),'produces a term and possibly wakes up the thread at a matching local_in/1').
bu1(local_cout(_A),'puts a term on local blackbord unless already there').
bu1(local_in(_A),'waits for a term produced by a matching local_out/1').
bu1(local_rd(_A),'tests if a term is available on the local blackboard').
bu1(local_when(_A),'waits until a term is available on the local blackboard').
bu1(local_cin(_A),'removes a term if available on the local blackboard').
bu1(local_all(_A,_B,_C),'local_all(X,G,Xs) collects facts X such that G on the blackboard').
bu1(local_all(_A,_B),'local_all(X,Xs) collects all facts matching X on the blackboard').
bu1(wait_for(_A,_B),'wait_for(Term,Constraint) waits for a term on the blackboard, such that Constraint holds').
bu1(notify_about(_A),'notifies a suspended matching wait_for(Term,Contraint), if Constraint holds, that Term is available').
bu1(all_for(_A,_B),'all_for(X,Xs) collects all constrained terms X on the blackboard to list Xs').
bu1(out(_A),'puts a term on Linda server or trigers resumption of a matching in/1 waiting for this data').
bu1(in(_A),'waits to remove a term from Linda blackboard').
bu1(all(_A,_B),'gets the list of terms matching arg 1 from Linda blackboard').
bu1(all(_A,_B,_C),'gets a selection arg 1 of terms matching arg 2 from Linda blackboard').
bu1(rd(_A),'reads a term matching arg 1 from Linda blackboard').
bu1(cin(_A),'tries to get and remove a term from Linda blackboard').
bu1(cout(_A),'adds a term to the blackboard, unless already a matching one is there').
bu1(include(_A),'includes/loads a file with current load method, unless it has already been included by the same method').
bu1(load_method(_A,_B),'args: Number, Name'-x(_C,_D)).
bu1(set_load_method(_A),'sets the current load method by name').
bu1(get_load_method(_A),'gets the current load method by name').
bu1(dconsult(_A),'reconsult/1 variant: cleans up data areas, consults/compiles based on db_ratio/1').
bu1(sconsult(_A),'reconsult/1 variant: cleans up data areas consults, makes all static').
bu1(oconsult(_A),'reconsult/1 variant: consults/compiles based on db_ratio and overwrites old clauses').
bu1(scompile(_A),'smart compile/1 variant: if the *.wam file is newer reloads, otherwise fcompiles first').
bu1(mcompile(_A),'compile/1 variant: cleans up data areas and compiles to memory').
bu1(qcompile(_A),'compile/1 variant: compiles a file to memory after pushing current user code to kernel - where it becomes read only').
bu1(compile(_A),'applies current compilation method to the file arg 1').
bu1(load(_A),'clean loads from a bytecode *.wam file').
bu1(fcompile(_A),'compiles a *.pl file to a *.wam bytecode file').
bu1(debug(_A),'').
bu1(consult(_A),'consults with possible duplication of clauses, allows later dynamic recompilation depending on db_ratio/1').
bu1(consult(_A,_B),'consult(File,DB) consults File into DB)').
bu1(reconsult(_A),'applies current consult method to file given as arg 1, set db_ratio/1 for specifying dynamic recompilation of heavily used interpreted code').
bu1(~_A,'short hand for reconsult').
bu1(assert_from_chars(_A),'asserts a program from clauses in list of chars').
bu1(assert_from_chars(_A,_B),'(Db,Cs) asserts to database Db, a set of clauses parsed from list of char codes Cs').
bu1(read_terms_from_chars(_A,_B),'(Chars,Clause): backtracks over Clause(s) parsed from a list of char codes').
bu1(read_terms_from_chars(_A,_B,_C),'(Chars,Clause,VarsInClause): backtracks over Clause(s) parsed from a list of char codes'-[x([98,40,88,44,89,41,58,45,97,40,89,44,88,41,46,32,97,40,49,44,49,41,46,32,97,40,95,44,50,41,46,32],_D,_E)]).
bu1(get_lineno(_A),'gets line number counter in current file').
bu1(set_lineno(_A),'sets line number counter in current file - use with care').
bu1(consult_cmd(_A),'').
bu1(consult_cmd(_A,_B),'').
bu1(file_cmd_hook(_A,_B),'file_cmd_hook(Cmd,Db): allows defining user actions on commands read from files').
bu1(consult_included(_A,_B),'').
bu1(consult0(_A),'').
bu1(consult0(_A,_B),'').
bu1(terminate_load,'').
bu1(terminate_file(_A,_B),'').
bu1(load_file(_A),'').
bu1(fcompile_file(_A),'').
bu1(scompile_file(_A),'').
bu1(mcompile_file(_A),'').
bu1(translate_all(_A,_B),'').
bu1(get_a_predicate(_A,_B,_C),'').
bu1(or(_A,_B),'').
bu1(if(_A,_B,_C),'').
bu1(termcat(_A,_B,_C),'').
bu1(asm,'shows transformations and readable BinWAM assembler for Prolog code entered at terminal').
bu1(asm(_A),'generates readable binarized form and BinWAM assembler to a file').
bu1(_A =.. _B,'called univ -this is bidirectional- it converts between a term and its view as a alist of components'-[x(f(a,b),_C),x(_D,[f,a,b])]).
bu1((_A ::- _B),'variant of :- for hand transformed binary clauses - use with care').
bu1(##_A,'executes arg 1 at compile time - make sure the executed code terminates').
bu1(proto_append(_A,_B),'').
bu1(proto_member(_A,_B),'').
bu1(member3(_A,_B,_C),'').
bu1(n_inline(_A),'').
bu1(n_arith(_A),'').
bu1(n_builtin(_A),'').
bu1(n_nop(_A),'').
bu1(statistics,'shows info about data areas'-x).
bu1(stat,'short hand for statistics').
bu1(interactive(_A),'toggles interactive query answering/tracing with arg 1 = yes or no').
bu1(def_to_mbin(_A,_B),'').
bu1(bincall(_A,_B),'call binary predicate with given continuation').
bu1(metacall(_A),'calls the interpreter').
bu1(on_undefined(_A),'(Goal): defines handler for undefined predicates matching Goal').
bu1(metatrue,'calls the interpreter on current continuation').
bu1(call(_A,_B),'efficient call/N variant').
bu1(call(_A,_B,_C),'efficient call/N variant').
bu1(call(_A,_B,_C,_D),'efficient call/N variant').
bu1(call(_A,_B,_C,_D,_E),'efficient call/N variant').
bu1(call(_A,_B,_C,_D,_E,_F),'efficient call/N variant').
bu1(call(_A,_B,_C,_D,_E,_F,_G),'efficient call/N variant').
bu1(override_call(_A,_B,_C),'').
bu1(safe_override_call(_A,_B,_C),'').
bu1(once(_A),'executes once, with no backtracking').
bu1(map(_A,_B),'maps a predicate with 1 arg to a list'-x(println,[10,20,30])).
bu1(map(_A,_B,_C),'maps a predicate with 2 args to a list'-x(+1,[10,20],_D)).
bu1(maplist(_A,_B),'maps a predicate with 1 arg to a list'-x(println,[10,20,30])).
bu1(maplist(_A,_B,_C),'maps a predicate with 2 args to a list'-x(+1,[10,20],_D)).
bu1(foldl(_A,_B,_C,_D),'(Op,InitialValue,List,?Result) accumulates values interating over List with binary Op'-x(+,0,[10,20,30],_E)).
bu1(foldr(_A,_B,_C,_D),'(Op,InitialValue,List,?Result) accumulates values interating over List with binary Op'-x(+,0,[10,20,30],_E)).
bu1(sum(_A,_B),'(List,?Result): sum of a list'-x([10,20],_C)).
bu1(prod(_A,_B),'(List, ?Result): product of a list'-x([10,20],_C)).
bu1(expand_term(_A,_B),'expands a term according to DCG expansion rules').
bu1(term_expansion(_A,_B),'can be used to define a hook into the default DCG expansion mechanism').
bu1(_A = _B,'(X,Y) true if (possibly cyclic) terms X and Y unify - cyclic terms can result from =/2, as occur check is not performed'-x(f(_C,s(a)),f(_D,_D))).
bu1((_A -> _B),'Cond->Then executes Cond once; if it succeeds it also executes Then').
bu1(if_any(_A,_B,_C),'(Cond,Then,Else): executes Cond; each time when Cond succeeds it also executes Then; if Cond never succeds it executes Else').
bu1((_A ; _B),'A;B succeeds if A succeeds or B, called after A, succeeds').
bu1((_A,_B),'A,B succeeds if A suceeds and B, called after A, succeeds').
bu1(repeat,'backtracks until its continuation succeeds; defined as repeat. repeat:-repeat. ').
bu1(foreach(_A),'foreach(G) backtracks over all answers to G and succeeds').
bu1(foreach(_A,_B),'foreach(G,D) executes D once for each answer of generator G').
bu1(forall(_A),'forall(G) backtracks over all answers to G and succeeds').
bu1(forall(_A,_B),'forall(G,D) executes D once for each answer of generator G').
bu1(for_all(_A,_B),'foreach(A,B) fails for all cases when A succeeds and B fails').
bu1(\+_A,'succeeds if its argument is executed and fails').
bu1(_A \= _B,'true if args fail to unify').
bu1(findall(_A,_B,_C),'findall(X,G,Xs) collects copies of all answers X of G to Xs. If less then half of the heap is free, it allocates new engine for running G'-x(s(_D),(member(_D,[1,2,3]),_D > 1),_E)).
bu1(findall(_A,_B,_C,_D),'findall(X,G,Xs,Ys) appends the list of answers X of G to Ys to obtain Xs'-x(s(_E),(_E = 1 ; _E = 2),_F,[3,4])).
bu1(qfindall(_A,_B,_C,_D),'qfindall(X,G,Xs,Ys): queues based, slightly faster findall/4, not MT-safe'-x(s(_E),(_E = 1 ; _E = 2),_F,[3,4])).
bu1(all_answers(_A,_B,_C),'(X,G,Xs): like findall/3, but such that if V is not common to X and G then V cannot be bound by execution of G'-x(_D,member(s(_D),[_E,_F,_F,_G]),_H)).
bu1(while(_A,_B),'(Cond,Goal): findall variant which explores alternative answers for Goal, while Cond holds, ').
bu1(skip_until(_A,_B),'findall variant').
bu1(skip_when(_A,_B),'findall variant').
bu1(find_while(_A,_B,_C,_D),'findall variant').
bu1(nth_answer(_A,_B),'(N,Goal) returns only the nth answer of Goal, if such an anser exists').
bu1(take_at_most(_A,_B),'(N,Goal) computes at most N answers of Goal').
bu1(drop_at_least(_A,_B),'(N,Goal) drops at least N answers of Goal G').
bu1(has_fuel(_A),'').
bu1(find_at_most(_A,_B,_C,_D),'(N,X,G,Xs) findall variant, computing at most N answers X of G').
bu1(all_but_at_least(_A,_B,_C,_D),'(N,X,G,Xs) findall variant, computing all but the first N answers X of G').
bu1(det_call(_A),'calls a Goal and warns if it was not deterministic').
bu1(for(_A,_B,_C),'generates an integer in a range'-x(_D,1,3)).
bu1(between(_A,_B,_C),'generates an integer between Min and Max'-x(1,3,_D)).
bu1(argn(_A,_B,_C),'generates all n args of term'-x(_D,f(a,b),_E)).
bu1(append(_A,_B,_C),'concatenates/decomposes lists'-[x([1,2],[3,4],_D),x(_E,_F,[1,2])]).
bu1(member(_A,_B),'(X,Xs): checks if an element X unifies with an element on a list Xs or generates sucessively longer lists if Xs is unbound or open ended'-[x(2,[1,2]),x(_C,[1,2])]).
bu1(memberchk(_A,_B),'(X,Xs) checks if an X is a the list Xs').
bu1(member_conj(_A,_B),'like member/2, for a comma separated conjunction, ending with true'-x(_C,(a,b,true))).
bu1(det_append(_A,_B,_C),'').
bu1('.'(_A,_B,_C),'(Head,Tail,List) builds List=[Head|Tail]').
bu1(++(_A,_B,_C),'concatenates N lists, usable in is/2').
bu1(length(_A,_B),'generates/mesures length of a list').
bu1(make_cmd0(_A,_B),'concatenates a list of strings and atomic elements into a string').
bu1(make_cmd(_A,_B),'concatenates a list of strings and atomic elements into an atom').
bu1(make_spaced_cmd(_A,_B),'concatenates a list of elements with inserted space separtors into an atom').
bu1(listify(_A,_B),'(T,Cs): transforms T, unless it is already such, to list of chars').
bu1(is_spec(_A),'true if a spacial character code').
bu1(is_terminator(_A),'true if a terminator character code').
bu1(is_maj(_A),'true if is an upper case char code').
bu1(is_min(_A),'true if a lower case char code').
bu1(is_an(_A),'true if an alphanumerical char code').
bu1(is_num(_A),'true if a digit char code').
bu1(numbervars(_A,_B,_C),'binds to $VAR(I) with I over distinct integers variables in a term').
bu1(numbervar_name(_A,_B),'').
bu1(answer_of(_A,_B),'(X,G): X is an answer for G, after finding all, sorting and removing duplicates').
bu1(solutions(_A,_B),'(GX,Xs): adds (last) output arg X to closure G then works like findall(X,GX,Xs)'-x(argn(_C,f(a,b,c)),_D)).
bu1(gc_call(_A),'G: executes G and ensures that no more space is consumed than the total size of the terms bound to its variables').
bu1(ground(_A),'true if arg has no free variables'-x(f(a,b))).
bu1(atom(_A),'true if symbol (functor of arity 0)'-x(a)).
bu1(float(_A),'true if represented as a 64 bit float number (C-double)'-x(3.14)).
bu1(number(_A),'true if integer or float').
bu1(compound(_A),'true if it has arity > 0'-x(f(a))).
bu1(appendN(_A,_B),'concatenates N lists'-x([[a,b],[],[c]],_C)).
bu1(reverse(_A,_B),'reverses a list'-x([a,b,c],_C)).
bu1(append_conj(_A,_B,_C),'concatenates 2 conjunctions').
bu1(append_disj(_A,_B,_C),'concatenates 2 disjunctions').
bu1(subsumes_chk(_A,_B),'checks if arg 1 is subsumed by arg 2, after renaming vars').
bu1(variant_of(_A,_B),'checks if args are the same up to a renaming of vars').
bu1(tab(_A),'outputs N blanks').
bu1(get(_A),'inputs the next char code after skiping over white space').
bu1(_A is _B,'calls the function evaluator, mostly for arithmetics'-x(_C,3+4*2)).
bu1(expr(_A,_B),'').
bu1(compare(_A,_B,_C),'returns <,=,> in arg 1 after comparing arg 2 with arg 3'-[x(_D,1,2),x(_E,f(b),f(a)),x(_F,s(_G),s(_G))]).
bu1(_A == _B,'true if args are identical terms').
bu1(_A \== _B,'true if arg 1 is not identical to arg 2').
bu1(_A @< _B,'instance of compare/3 with arg 1: <').
bu1(_A @> _B,'instance of compare/3 with arg 1: >').
bu1(_A @=< _B,'instance of compare/3 with arg 1: = or <').
bu1(_A @>= _B,'instance of compare/3 with arg 1: > or =').
bu1(_A < _B,'numeric comparison').
bu1(_A > _B,'numeric comparison').
bu1(_A =< _B,'numeric comparison').
bu1(_A >= _B,'numeric comparison').
bu1(_A =:= _B,'numeric comparison').
bu1(_A =\= _B,'numeric comparison').
bu1(_A+_B,'returns 0 + arg 1').
bu1(_A-_B,'returns 0 - arg 1').
bu1(_A\_B,complement).
bu1(max(_A,_B,_C),'(X,Y,Max): Max is the max of 2 numbers X, Y').
bu1(min(_A,_B,_C),'(X,Y,Min): Min is the min of 2 numbers X, Y').
bu1(bb_def(_A,_B,_C),'bb_def(K1,K2,T) associates to K1 and K2 (a copy of) T on the blackboard').
bu1(bb_set(_A,_B,_C),'bb_set(K1,K2,T) updates the term associated with K1 and K2 to be a copy of T').
bu1(bb_let(_A,_B,_C),'bb_let(K1,K2,T) updates or defines the term associated with K1 and K2 to be T').
bu1(bb_get(_A,_B,_C),'bb_get(K1,K2,T) consumes the term T associated with K1 and K2').
bu1(bb_val(_A,_B,_C),'bb_val(K1,K2,T) T is (a copy of) the term associated with keys K1 and K2').
bu1(bb_rm(_A,_B),'removes the term associated with K1 and K2 from the blackboard').
bu1(let(_A,_B,_C),'').
bu1(let(_A,_B),'').
bu1(def(_A,_B),'').
bu1(set(_A,_B),'').
bu1(val(_A,_B),'').
bu1(rm(_A),'').
bu1(bb_def(_A,_B),'').
bu1(bb_set(_A,_B),'').
bu1(bb_val(_A,_B),'').
bu1(bb_rm(_A),'').
bu1(bb_let(_A,_B),'').
bu1(bb_get(_A,_B),'').
bu1(nb_delete(_A),'').
bu1(nb_setval(_A,_B),'').
bu1(nb_getval(_A,_B),'').
bu1(prolog_flag(_A,_B),'(Flag,Value): retrieves the value of a Prolog flag').
bu1(set_prolog_flag(_A,_B),'(Flag,Value): sets the value of a Prolog flag').
bu1(static_prolog_flag(_A,_B),'contains read-only Prolog flag values').
bu1(vread(_A,_B),'reads HDEFI or HDEFS defined C constant into a Prolog integer or atom').
bu1(vget_int(_A,_B),'gets a VSHARE defined int C variable to a 28 bit int').
bu1(vset_int(_A,_B),'sets a VSHARE defined int C variable to a 28 bit int').
bu1(vget0(_A,_B),'gets the type and value of a C word').
bu1(vget(_A,_B),'gets a VSHARE declared C data object in a term like int(N),F/N or var(V)').
bu1(vset(_A,_B),'sets a VSHARE declared C data object in a term like int(N),F/N or var(V)').
bu1(gvset(_A,_B),'').
bu1(gvget(_A,_B),'').
bu1(set_bp_error(_A,_B,_C,_D),'(Id,Mes,Arg1,Arg2): notifies emulator about error condition').
bu1(get_bp_error(_A,_B,_C,_D),'(Id,Mes,Arg1,Arg2): gets error sate from emulator').
bu1(clear_bp_error,'clears errors set by various conditions').
bu1(pow(_A,_B,_C),'(Base,Expo,Val) computes power function'-x(2,3,_D)).
bu1(**(_A,_B,_C),'returns arg 1 at power arg 2, a float'-x(2,3,_D)).
bu1(log(_A,_B,_C),'returns log in base arg 1 of arg 2, a float'-x(2,8,_D)).
bu1(atan2(_A,_B,_C),'float function').
bu1(hypot(_A,_B,_C),'float function').
bu1(sqrt(_A,_B),'returns square root of arg 1, a float'-x(2,_C)).
bu1(exp(_A,_B),'float function').
bu1(log(_A,_B),'float function').
bu1(sin(_A,_B),'float function').
bu1(cos(_A,_B),'float function').
bu1(tan(_A,_B),'float function').
bu1(asin(_A,_B),'float function').
bu1(acos(_A,_B),'float function').
bu1(atan(_A,_B),'float function').
bu1(integer(_A,_B),'float to int cast').
bu1(float(_A,_B),'float function').
bu1(sign(_A,_B),'int function').
bu1(abs(_A,_B),'int function').
bu1(floor(_A,_B),'float to int function'-x(1.3,_C)).
bu1(ceiling(_A,_B),'float to int function'-x(1.3,_C)).
bu1(truncate(_A,_B),'float to int function'-x(1.51,_C)).
bu1(round(_A,_B),'float to int function'-x(1.51,_C)).
bu1(xor(_A,_B,_C),'bitwise exclusive or').
bu1(statistics(_A,_B),'returns info about data areas'-x(_C,_D)).
bu1(predicate_property(_A,_B),'returns a property of a predicate'-predicate_property(write(_C),_D)).
bu1(current_predicate(_A,_B),'generates/checks name and head of a currently defined predicate').
bu1(current_predicate(_A),'generates/checks the head of an existing predicate').
bu1(ctime(_A),'gets elapsed cpu time in ms'-x(_B)).
bu1(rtime(_A),'gets elapsed real time in secs'-x(_B)).
bu1(otime(_A),'gets time in secs from arbitrary origin to start'-x(_B)).
bu1(abstime(_A),'gets time in secs since arbitrary origin'-x(_B)).
bu1(help,'generates file help.txt with info and examples').
bu1(help(_A),'same as apropos/1'-x(assert)).
bu1(apropos(_A),'prints names of predicates defined in the system'-x(garbage)).
bu1(apropos(_A,_B),'returns names of predicates defined in the system'-x(retract,_C)).
bu1(unix(_A),'executes various Unix commands').
bu1(unix_argv(_A),'gets the list of cmd line args from 1 to argc'-x(_B)).
bu1(unix_cat(_A),'prints a file to user terminal').
bu1(file2chars(_A,_B),'reads a file to a list of ascii codes').
bu1(char_of(_A,_B),'reads a Prolog file to a set of ascii codes - on backtracking').
bu1(sentence_of(_A,_B),'reads a natural language file to a sentence built as a list of words - on backtracking').
bu1(sentence_of(_A,_B,_C),'(File,Ends,Sent) reads a file to a sentence separated by Ends - on backtracking').
bu1(line_of(_A,_B),'(File,Line) reads a file to lines ending with eol - on backtracking').
bu1(term_of(_A,_B),'reads a Prolog file to a set of terms - on backtracking').
bu1(tokens_of(_A,_B),'reads a Prolog file to a slist of tokens').
bu1(token_of(_A,_B),'reads a Prolog file to a set of tokens - on backtracking').
bu1(clause_of(_A,_B),'reads a Prolog file to a set of clauses - on backtracking').
bu1(shell(_A),'passes a command to the OS').
bu1(system(_A),'passes a command to the OS').
bu1(system(_A,_B),'passes a command to the OS and gets back return code').
bu1(cd(_A),'changes local dir to arg 1').
bu1(cd,'changes local dir to HOME directory or / if no such env var').
bu1(pwd,'shows current dir').
bu1(pwd(_A),'returns current dir as a list of chars').
bu1(trace(_A),'traces execution of a goal').
bu1(dynamic _A,'states that a predicate can be updated').
bu1(multifile _A,'states that clauses of a predicate can be in different files').
bu1(is_multifile(_A),'').
bu1(discontiguous _A,'states that clauses of a predicate can be in different places').
bu1(is_discontiguous(_A),'checks if a predicate has been declared as discontiguous').
bu1(hkey(_A,_B),'computes hash code on atomic argument; fails on variables and compound terms'-[term_hash(t(a,b),_C),term_hash(t(a,c),_D)]).
bu1(term_hash(_A,_B),'computes hash code on terms ground up to depth 64; fails if something is unbound or the limit is reached'-[term_hash(t(a,b),_C),term_hash(t(a,c),_D)]).
bu1(global_set(_A,_B,_C),'(A,B,X): associates X to ground keys A,B').
bu1(global_get(_A,_B,_C),'(A,B,X): retrieves X associated to ground keys A,B').
bu1(global_rm(_A,_B),'(A,B): removes value associated to ground keys A,B').
bu1(set_hash_max(_A),'Set the range of values from -1 to -Max, that should be a prime number, defaults to 1999').
bu1(hash_trace(_A,_B),'tracer for hash maps').
bu1(hash_push(_A,_B),'(GroundKey,Term): attaches a term to a ground key in constant time as a the first element').
bu1(hash_put(_A,_B),'(GroundKey,Term): attaches a term to a ground key in constant time as the last element').
bu1(hash_get(_A,_B),'(GroundKey,Term) retrieves in constant time a term attached to a ground key').
bu1(hash_rm_one(_A,_B),'(GroundKey) removes a term attached to a key and returns it').
bu1(hash_rm(_A,_B),'(GroundKey) removes a term attached to a key and returns it - backtracks').
bu1(hash_clear(_A,_B),'(GroundKey,Term) removes all matching terms attached to a key').
bu1(hash_clear(_A),'(GroundKey) removes all terms attached to a key').
bu1(hash_clear,'removes all terms from all hash keys').
bu1(hash_gc,'frees space used by hash keys and values').
bu1(hash_save(_A,_B),'(File,PredNMame) saves the hashed map to a database in predicate PredName ready for 1-arg indexing if compiled').
bu1(hash_save(_A),'(File) saves the hashed map to a database ready for 1-arg indexing if compiled').
bu1(hash_compile,'compiles the hashed map to a predicate benefiting from 1-arg indexing').
bu1('$hx'(_A,_B,_C),'hidden compiled hashed database predicate').
bu1(hash_load(_A),'(File) loads the hashed map to a database').
bu1(hash_key(_A),'(IntKey) backtrack over hash keys').
bu1(hash_find_unique(_A,_B,_C),'like findall(X,G,Xs) but uses hashing on ground term X to collect unique answers').
bu1(push_term(_A,_B),'(GroundKey,Term): attaches a term to a ground key in constant time as a the first element').
bu1(put_term(_A,_B),'(GroundKey,Term): attaches a term to a ground key in constant time as the last element').
bu1(new_iterator(_A,_B),'(GroundKey,Iterator) gets an iterator to terms attached to ground key').
bu1(close_iterator(_A),'(Iterator) closes an iterator').
bu1(has_terms(_A),'(GroundKey) succeds if at least on term is attached to GroundKey').
bu1(get_next_term(_A,_B),'(Iterator,Term) returns the next term attached to an iterator, fails if no more left').
bu1(remove_current_term(_A),'(Iterator) removes current term attached to an iterator').
bu1(update_current_term(_A,_B),'(Iterator,NewTerm): replaces current term of an iterator').
bu1(delete_all_terms(_A),'(GroundKey) removes all terms attached to a key').
bu1(count_terms(_A,_B),'returns the number of terms attached to a key').
bu1(get_term(_A,_B),'(Iterator,Term): backtracks over terms associated to a key').
bu1(get_all_terms(_A,_B),'(GroundKey,Ts) collects to alist all terms attached to a key').
bu1(new_term(_A,_B),'(Term,Handle): creates a new external Term and returns an integer Handle to it').
bu1(instance_of(_A,_B),'(Handle,Term): creates an internal instance of an external term given as a Handle').
bu1(free_term(_A),'(Handle): frees external term given as a Handle').
bu1(new_key_iterator(_A),'(Iterator): returns an iterator over the set of external keys').
bu1(tstest(_A,_B,_C),'(K,V,R): performs various tests with external terms').
bu1(process_term(_A,_B,_C),'(OpCode,Input,Output): applies various user defined C functions to external terms').
bu1(mmap_new(_A),'(D): returns a new mmap handle D - a mmap holds multiple elements for each key').
bu1(mmaps_iterate(_A),'(D): backtracks over mmap handles created with mmap_new D').
bu1(mmaps_gc,'frees memory for all mmaps created with mmap_new').
bu1(mmaps_clean,'empties and cleans up all mmaps created with mmap_new').
bu1(mmaps_show,'lists the content of all mmaps created with mmap_new').
bu1(mmap_push(_A,_B,_C),'(D,K,V): adds K,V as the first element of mmap D').
bu1(mmap_put(_A,_B,_C),'(D,K,V): adds K,V as the last element of mmap D').
bu1(mmap_get(_A,_B,_C),'(D,K,V): gets V, given K in mmap D,  backtracks over K if K unbound').
bu1(mmap_rm_one(_A,_B,_C),'(D,K,X): removes first term X attached to key K from mmap D').
bu1(mmap_rm(_A,_B,_C),'(D,K,X): removes a term X attached to key K from mmap D and backtracks on each such term').
bu1(mmap_rm_all(_A,_B),'(D,K): removes all terms X attached to key K from mmap D').
bu1(mmap_gc(_A),'(D): frees unused memory in D').
bu1(mmap_clear(_A),'(D): empties and cleans up D').
bu1(mmap_key(_A,_B),'(D,K): succeds if K is known, otherwise finds V backtracks over each key K in D').
bu1(mmap_show(_A),'(D): lists the content of a mmap').
bu1(map_new(_A),'(D): returns a new map handle D').
bu1(maps_iterate(_A),'(D): backtracks over map handles created with mmap_new D').
bu1(maps_gc,'frees memory for all maps created with mmap_new').
bu1(maps_clean,'empties and cleans up all maps created with mmap_new').
bu1(maps_show,'lists the content of all maps created with mmap_new').
bu1(map_put(_A,_B,_C),'(D,K,V): adds K,V to map D').
bu1(map_get(_A,_B,_C),'(D,K,V): gets V, given K in map D, backtracks over K if K unbound').
bu1(map_rm(_A,_B),'(D,K): removes key K from map D').
bu1(map_gc(_A),'(D): frees unused memory in D').
bu1(map_clear(_A),'(D): empties and cleans up D').
bu1(map_key(_A,_B),'(D,K): succeds if K is known, otherwise finds V backtracks over each key K in D').
bu1(map_show(_A),'(D): lists the content of a map').
bu1(addq(_A,_B,_C),'adds to end of persistent queeue'-x(key1,key2,33)).
bu1(pushq(_A,_B,_C),'adds to beginning of persistent queeue'-x(key1,key2,f(_D,_D))).
bu1(cpopq(_A,_B,_C),'pops (copy of) first element of persistent queue'-x(key1,key2,_D)).
bu1(cmembq(_A,_B,_C),'generates (copies of) members of a queue'-x(key1,key2,_D)).
bu1(cdelq(_A,_B,_C,_D),'deletes first matching element from a queue'-x(key1,key2,_E,_F)).
bu1(cdelq_any(_A,_B,_C),'deletes any matching element from a queue'-x(key1,key2,_D)).
bu1(membq(_A,_B,_C),'').
bu1(set_db(_A),'sets the name of active database for dynamic code').
bu1(current_db(_A),'gets the name of currently active database').
bu1(asserted(_A),'runs a predicated if asserted').
bu1(asserta(_A),'adds a clause to be first in a predicate definition').
bu1(assertz(_A),'adds a clause to be last in a predicate definition').
bu1(assert(_A),'adds a clause').
bu1(retract(_A),'backtracks over deleting matching clauses').
bu1(retract1(_A),'deletes first matching clause in the current database').
bu1(retractall(_A),'deletes all matching clauses').
bu1(clause(_A,_B),'clause(H,B) generates a clause with head matching H and body B').
bu1(abolish(_A),'abolish(F/N) deletes predicate F/N').
bu1(abolish(_A,_B),'').
bu1(db_hook,'').
bu1(db_hook_on,'').
bu1(db_hook_off,'').
bu1(x_traced_call(_A),'traces calls if compile-time x_trace triggers it').
bu1(x_trace(_A),'traces and profiles predicates, in combination with x_profile').
bu1(x_trace(_A,_B),'db hook, internal').
bu1(x_profile,'db hook, shows the result of profiling compiled predicates').
bu1(x_dynamic(_A),'db hook, states that a predicate can be updated').
bu1(x_is_dynamic(_A),'db hook, checks if dynamic').
bu1(x_asserta(_A),'db hook, adds a clause to be first in a predicate definition').
bu1(x_assertz(_A),'db hook, adds a clause to be last in a predicate definition').
bu1(x_retract(_A),'db hook, backtracks over deleting matching clauses').
bu1(x_retractall(_A),'db hook, deletes all matching clauses').
bu1(x_abolish(_A),'db hook, deletes all matching clauses and unmarks the clause as dynamic').
bu1(x_clause(_A,_B),'db hook, clause(H,B) generates a clause with head matching H and body B').
bu1(x_consult(_A),'db hook, consults with possible duplication of clauses').
bu1(x_listing,'db hook, lists given predicate if in current database').
bu1(x_listing(_A),'lists predicate F of arity N if in current database').
bu1(x_gc,'db hook, recovers memory used by the database').
bu1(disable_static(_A),'').
bu1(disable_static(_A,_B),'').
bu1(disable_builtins,'').
bu1(db_asserted(_A,_B),'runs predicate arg 2 if asserted in database arg 1').
bu1(db_asserta(_A,_B),'does asserta/1 arg 2 into database given as arg 1').
bu1(db_assertz(_A,_B),'does assertz/1 arg 2 into database given as arg 1').
bu1(db_assert(_A,_B),'does assert/1 arg 2 into database given as arg 1').
bu1(db_retract(_A,_B),'does retract/1 arg 2 from database given as arg 1').
bu1(db_retract(_A,_B,_C),'db_retract(Db,H,B) retracts clause with head H and body B from database Db').
bu1(db_retract1(_A,_B),'deletes from database given as arg 1 a matching clause').
bu1(db_retractall(_A,_B),'removes from database given as arg 1, all matching clauses').
bu1(db_retractall(_A,_B,_C),'removes from database given as arg 1, all matching clauses seen as head + body').
bu1(db_head(_A,_B),'generates/checks a predicate head in database (arg 1)').
bu1(db_clause(_A,_B,_C),'clause(DB,H,B) generates a clause found in database DB with head matching H and body B').
bu1(db_abolish(_A,_B),'db_abolish(DB,F/N) removes predicate F/N from DB').
bu1(db_abolish(_A,_B,_C),'').
bu1(db_clean(_A),'db_clean(DB) abolishes all predicates in DB').
bu1(db_clean,'abolishes all predicates in currently active database').
bu1(db_move(_A,_B),'db_move(FromDB,ToDB) moves the content of database FromDB over database ToDB while replacing similar predicates').
bu1(this_db(_A),'same as current_db, returns the current database').
bu1(db_consult(_A,_B),'db_consult(File,Db) consults a file to a database Db').
bu1(db_save(_A,_B),'db_save(Db,File) saves using qprint/1 all the clauses of Db to File').
bu1(db_save(_A),'db_save(File) saves all the clauses of the current database to File').
bu1(xsave(_A),'xsave(File) saves all the clauses of the current database to a binary File').
bu1(xload(_A),'xload(File) loads clauses to the current database, from a binary File').
bu1(make_compileable(_A,_B),'make_compilable(Files,File) rewrites Files as a canonical File with all predicates contiguous and no operators').
bu1(dynco(_A),'yes/no: activates/desactivates dynamic recompilation').
bu1(db_ratio(_A),'sets/gets call/assert ratio which triggers dynamic recompilation').
bu1(make_all_static,'').
bu1(make_all_dynamic,'').
bu1(make_static(_A),'').
bu1(make_dynamic(_A),'').
bu1(dyn2stat(_A),'dyn2stat(H): compiles at runtime a predicate with head H').
bu1(dyn2stat(_A,_B),'dyn2stat(Db,H): compiles at a predicate from database Db with head H').
bu1(stat2dyn(_A),'stat2dyn(H): reverts to uncompiled representation for a predicate with head H').
bu1(stat2dyn(_A,_B),'stat2dyn(Db,H): reverts to uncompiled representation for a predicate in database Db with head H').
bu1(listing,'lists current database').
bu1(listing(_A),'lists given predicate if in current database').
bu1(listing(_A,_B),'lists predicate F of arity N if in current database').
bu1(db_listing(_A,_B),'lists predicate F/N in given database').
bu1(db_listing(_A),'lists caluses given database').
bu1(topcall(_A),'calls arg 1 as if it were entered by the user at Prolog prompt').
bu1(is_asserted(_A),'checks if currently asserted').
bu1(is_dynamic(_A),'checks if dynamic').
bu1(db_is_dynamic(_A,_B),'checks if dynamic in a given database').
bu1(bb,'lists (bp_long!) content of the blackboard').
bu1(bb0,'').
bu1(bb(_A),'').
bu1(bb_list(_A),'').
bu1(bb_element(_A,_B),'').
bu1(sort(_A,_B),'sorts and removes duplicates'-x([2,1,3,1,4,4,2],_C)).
bu1(merge_sort(_A,_B,_C),'(Order,List,Sorted)'-x(>,[1,3,2,2,4],_D)).
bu1(msort(_A,_B),'sorts and keeps duplicates'-x([2,1,3,1,4,4,2],_C)).
bu1(keysort(_A,_B),'sorts while putting similar keys one after the other in a list'-x([3-a,1-a,2-b,1-c,2-d],_C)).
bu1(keygroup(_A,_B,_C),'sorts while grouping similar keys'-x([3-a,1-a,2-b,1-c,2-d],_D,_E)).
bu1(bagof(_A,_B,_C),'all solutions predicate generating unsorted bags of possibly dupplicated answers'-x(_D,member(_D,[3,2,2,1]),_E)).
bu1(setof(_A,_B,_C),'all solutions predicate generating sorted sets of unduplicated answers'-x(_D,member(_D,[3,2,2,1]),_E)).
bu1(_A^_B,'calls arg 2 and binds arg 1'-x(_C,eq(_C,1))).
bu1(free_variables(_A,_B,_C,_D),'').
bu1(not _A,'sound negation').
bu1(vars_of(_A,_B),'(Term,Vars): lists free vars of a term'-x(f(_C,t(_C,_D,_E),_E),_F)).
bu1(term_codes(_A,_B),'(Term,Chars): converts between a term and its list of char code representation'-[x(f(a,b),_C),x(_D,[102,40,97,44,98,41])]).
bu1(term_chars(_A,_B),'(Term,Chars): converts between a term and its list of char code representation'-[x(f(a,b),_C),x(_D,[102,40,97,44,98,41])]).
bu1(atom_codes(_A,_B),'(Atom,CharCodes): converts between an atom and its list of char code representation'-[x(hello,_C),x(_D,[104,101,108,108,111])]).
bu1(number_codes(_A,_B),'(Number,CharCodes): converts between a number and its list of char code representation'-[x(1999,_C),x(_D,[50,48,48,49])]).
bu1(atom_chars(_A,_B),'(Atom,CharAtoms): converts between an atom and its list of char atoms representation'-[x(hello,_C),x(_D,[104,101,108,108,111])]).
bu1(number_chars(_A,_B),'(Number,CharAtoms): converts between a number and its list of char atoms representation'-[x(1999,_C)]).
bu1(to_string(_A,_B),'converts a term to a string constant (in paricular, converts numbers to strings)'-[x(13,_C),x(3.14,_D),x(f(_E,_E),_F)]).
bu1(read_term_from_chars(_A,_B),'reads a term from a list of char codes'-[x([102,40,88,44,88,44,89,44,89,41],_C)]).
bu1(@(_A,_B,_C),'alternative form for C/3 DCG connect relation').
bu1(star(_A,_B,_C,_D),'star(Recognizer,Result): DCG based star regexp processor for (Recognizer)*').
bu1(plus(_A,_B,_C,_D),'plus(Recognizer,Result): DCG based plus regexp processor for (Recognizer)+').
bu1(one(_A,_B,_C,_D),'one(Recognizer,Result): DCG based regexp processor for exactly one Recognizer').
bu1(dcg_call(_A,_B,_C,_D),'(F,X,I,O): DCG metacall for star,plus,one').
bu1(dcg_call(_A,_B,_C,_D,_E),'(F,X,Y,I,O): DCG metacall for star,plus,one').
bu1(is_space(_A,_B,_C),'').
bu1(is_letter(_A,_B,_C),'').
bu1(is_punct(_A,_B,_C),'').
bu1(is_digit(_A,_B,_C),'').
bu1(word(_A,_B,_C),'').
bu1(nword(_A,_B,_C),'').
bu1(to_tokens(_A,_B),'to_tokens(Codes,PrologTokens)').
bu1(to_words(_A,_B),'to_words(Codes,NaturalLanguageWords').
bu1(to_word_codes(_A,_B),'to_word_codes(Codes,ListOfWordCodes)').
bu1(term(_A,_B,_C,_D),'').
bu1(disj_term(_A,_B,_C,_D),'').
bu1(match_word(_A,_B,_C),'match_word(+Word,I,O): matches/consumes a word, in the conext of DCGs').
bu1(match_before(_A,_B,_C,_D,_E),'match_before(+Stops,-Word,-Stop,I,O): matches a word with DCGs until a given delimiter set is hit, one of which is also returned').
bu1(match_before(_A,_B,_C,_D),'match_before(+Stop,-Word,I,O): matches a word with DCGs until a given delimiter is hit').
bu1(codes_words(_A,_B),'converts a list of character codes to a list of words and back').
bu1(words_code(_A,_B),'generates a code ready to print, one a t a time with put/1 from a list of words').
bu1(write_term_to_chars(_A,_B),'writes a term to a list of char codes'-[x(f(_C,_C,_D,_D),_E)]).
bu1(read_term_from_chars(_A,_B,_C),'reads a term with variable names from a list of char codes'-[x([102,40,88,44,88,44,89,44,89,41],_D,_E)]).
bu1(write_term_to_chars(_A,_B,_C),'writes a term with variables names to a list of char codes'-[x(f(_D,_E),['X' = _D,'Y' = _E],_F)]).
bu1(write_term_to_chars1(_A,_B,_C),'').
bu1(trim_term(_A,_B),'').
bu1(trim_term(_A,_B,_C),'').
bu1(trim_term(_A,_B,_C,_D),'trim_term(D,Filler,T,NewT) replaces subterms of T deeper than D with Filler').
bu1(user_error(_A,_B),'writes basic error message and fail').
bu1(errmes(_A,_B),'writes error message and fails').
bu1(fatal_error(_A,_B),'writes error message and aborts').
bu1(quietmes(_A),'writes message if in low quietness mode').
bu1(quietmes(_A,_B),'writes message if quietness is lower than arg 1)').
bu1(debugmes(_A),'writes message in debug mode (low quietness)').
bu1(main,'user defined optional startup predicate').
bu1(main(_A),'default entry predicate, does both prolog_init/1 and prolog_run/1').
bu1(prolog_load(_A),'loads code and/or executes first command line arg').
bu1(prolog_init(_A),'runs more command line args representing prolog goals').
bu1(prolog_run(_A),'starts main/0 if defined, otherwise toplevel/0').
bu1(toplevel,'interactive toplevel Prolog loop').
bu1(topstep(_A),'interactive toplevel Prolog step').
bu1(is_prolog(_A),'recognizes binprolog - useful for portability').
bu1(bp_only(_A),'runs goal only if is_prolog(binprolog) is true').
bu1(bp_only(_A,_B),'(Goal,Alternative): runs goal only if is_prolog(binprolog) is true otherwise runs Alternative').
bu1(read(_A),'reads a term').
bu1(read_term(_A,_B),'reads a term and also a list of variable-name associations').
bu1(top_read_term(_A,_B),'').
bu1(warn_singletons(_A,_B,_C),'').
bu1(read_with_singletons(_A,_B,_C),'').
bu1(read_clause(_A),'').
bu1(read_tokens(_A,_B),'').
bu1(get_code(_A),'ISO char code reader').
bu1(put_code(_A),'ISO char code writer'-x(99)).
bu1(nl,'writes a new line character').
bu1(fast_write(_A),'').
bu1(write_float(_A),'').
bu1(generic_write(_A),'overridable write/1, style (writeq, write, display) given with write_style/1 assumption').
bu1(generic_write(_A,_B),'').
bu1(write(_A),'writes to current output stream set with tell/1, defaults to <user> - Prolog''s stdio').
bu1(print(_A),'variant of write/1').
bu1(writeq(_A),'variant of write which quotes if needed, so that term is read back correctly/1').
bu1(portable_display(_A),'').
bu1(display(_A),'writes to terminal while ignoring operator definitions').
bu1(portray(_A),'').
bu1(portray_clause(_A),'pretty prints a clause').
bu1(pp_clause(_A),'prints out a clause with some care on how it looks').
bu1(pp_term(_A),'pretty prints a term').
bu1(read_chars(_A),'reads to a list of ascii codes').
bu1(write_chars(_A),'writes a list of ascii codes'-x([104,101,108,108,111])).
bu1(get_code(_A,_B),'inputs a char code from a stream - ISO').
bu1(put_code(_A,_B),'outputs a char code to a stream - ISO').
bu1(get_char(_A,_B),'(Stream,CharAsOneLetterConstant): inputs a char from a stream -ISO Prolog').
bu1(put_char(_A,_B),'(Stream,CharAsOneLetterConstant): outputs a char to a stream -ISO Prolog').
bu1(flush_output,'flushes current output stream').
bu1(flush_output(_A),'flushes a stream').
bu1(file_size(_A,_B),'returns the size of a file, in bytes').
bu1(ttyin(_A),'').
bu1(ttyout(_A),'').
bu1(ttyput(_A),'').
bu1(ttynl,'').
bu1(ttyprin(_A),'writes to terminal').
bu1(ttyprint(_A),'writes to terminal with a new line').
bu1(ttycwrite(_A),'').
bu1(ttycwriteln(_A),'').
bu1(popen(_A,_B,_C),'popen(Cmd,read/write,Stream) opens Stream using a pipe from/to process executing Cmd').
bu1(pclose(_A),'closes a pipe generated stream').
bu1(pcollect(_A,_B),'collects output from a command to a list of char codes').
bu1(ls2list(_A,_B),'(Dir,Files): converts ls cmd output to list of files and dirs').
bu1(dir2list(_A,_B,_C),'(DirListerCmd,DirName,Files): converts OS specific DirLister output to list of files and/or directories').
bu1(dir2dirs(_A,_B),'(Dir,Dirs): converts dir cmd output to list of sub directories of Dir').
bu1(dir2files(_A,_B),'(Dir,Files): converts dir cmd output to list of files (which are not dirs) contained in Dir').
bu1(fopen(_A,_B,_C),'Prolog equivalent of C-function: opens a stream in a given mode and returns an integer handle to it').
bu1(fclose(_A),'closes the C-stream specifiend as an integer handle').
bu1(open(_A,_B,_C),'returns a stream (arg 3) on a file (arg 1) in read/write/append mode (arg 2)').
bu1(close(_A),'closes a stream opened by open/3').
bu1(set_input(_A),'sets current input stream').
bu1(set_output(_A),'sets current output stream').
bu1(current_input(_A),'gets current input stream').
bu1(current_output(_A),'gets current output stream').
bu1(see(_A),'focuses input on a file').
bu1(seeing(_A),'gets file name opened and set by see/1').
bu1(seen,'close file opened by see/1').
bu1(tell(_A),'focuses output on a file').
bu1(telling(_A),'gets file name opened and set by tell/1').
bu1(told,'closes file opened by tell/1').
bu1(tell_at_end(_A),'focuses output on file opened in append mode').
bu1(see_at(_A),'seeks a seekable file at a give offset (in bytes)').
bu1(seeing_at(_A),'retrieves position in current file opened by see/1').
bu1(tell_at(_A),'moves output file pointer to a given offset (in bytes)').
bu1(telling_at(_A),'retrieves output file position (in bytes)').
bu1(file_search_path(_A),'defines search path relative to BP_PATH (home of BinProlog) and PROLOG_PATH (home of user files)').
bu1(file_extension_list(_A),'defines default file extensions for find_file').
bu1(file_library(_A,_B),'').
bu1(make_file_name(_A,_B,_C,_D),'').
bu1(find_file(_A,_B),'finds a file name on search path').
bu1(exists_file(_A),'true if file exists').
bu1(see_or_fail(_A),'opens a file if it exists, otherwise fails').
bu1(flush,'').
bu1(sread(_A,_B,_C),'reads a term and a list of vars from a string (atom)'-x('f(X,Y)',_D,_E)).
bu1(sread(_A,_B),'reads a term from a string (atom)').
bu1(swrite(_A,_B,_C),'writes a term with a liste of vars to a string (atom)').
bu1(swrite(_A,_B),'writes a term to a string (atom)').
bu1(bp_val(_A,_B,_C),'unifies with 2 key indexed global logical variable').
bu1(bu0(_A,_B,_C,_D),'').
bu1(bu1(_A,_B),'').
bu1(bu_ctr(_A,_B),'').
bu1(current_op(_A,_B,_C),'generates/check current op/3 operator definition(s)').
bu1(op(_A,_B,_C),'op(Pri,A,Op) defines an operator Op of priority Pri and associativity A').
bu1(is_builtin(_A),'recognizes a predicate head as a builtin'-x(var(_B))).
bu1(edit(_A,_B),'edit(E,F) edits with editor E, file F').
bu1(my_edit(_A),'').
bu1(edit,'calls DOS editor edit on last compiled file').
bu1(ed,'edits last compiled/consulted file with default editor and refreshes it in memory').
bu1(textedit,'calls texedit editor on last compiled file').
bu1(emacs,'calls emacs editor on last compiled file').
bu1(pico,'calls pico editor on last compiled file').
bu1(notepad,'calls notepad editor on last compiled file').
bu1(vi,'calls vi editor on last compiled file').
bu1(spawn(_A),'spawns Goal in a new bp window on W95/NT PC and Unix/X').
bu1(spawn(_A,_B,_C),'spawns(Goal,Includes,TempFile): spawns a new bp window').
bu1(co,'reconsults/recompiles last file').
bu1(co(_A),'reconsults using fast reader').
bu1(current_user_file(_A),'').
bu1(ls,'list files under Unix').
bu1(dir,'lists files under DOS').
bu1(reboot,'regenerates BinProlog from its sources').
bu1(remake,'').
bu1(make,'').
bu1(make(_A),'').
bu1(make(_A,_B),'').
bu1(make(_A,_B,_C,_D),'').
bu1(make(_A,_B,_C,_D,_E),'').
bu1(make0(_A,_B,_C,_D),'').
bu1(qmake(_A),'compiles Project to fast C code - for packaging as standalone executable').
bu1(qmake(_A,_B),'(Project,Module): compiles to fast C code a project in Module: uses set_threshhold(12,60) before dooing cmake/2, for a good speed/code size ratio').
bu1(cmake,'compiles BinProlog''s Prolog components to compact C code - for packaging as standalone executable').
bu1(cmake(_A),'compiles a Project to compact C code - for packaging as standalone executable').
bu1(cmake(_A,_B),'(Project,Module): compiles to C a project with all clauses belonging to Module').
bu1(kmake,'').
bu1(kcmake,'').
bu1(rmake,'').
bu1(crmake,'').
bu1(tboot,'').
bu1(tboot(_A),'').
bu1(tboot(_A,_B),'').
bu1(tmake,'').
bu1(tmake(_A),'').
bu1(tmake(_A,_B),'').
bu1(cboot,'').
bu1(boot,'regenerates file wam.bp in BinProlog src directory').
bu1(add_true(_A,_B),'').
bu1(gc_read(_A),'').
bu1(gc_read_clause(_A),'').
bu1(call_body(_A),'').
bu1(expand_call_body(_A,_B),'').
bu1(tr_body(_A,_B),'').
bu1(char_in_cmd(_A,_B),'').
bu1(show_code0(_A,_B),'').
bu1(patch_it(_A,_B,_C,_D),'').
bu1(lwrite(_A),'').
bu1(is_engine(_A),'recognizes and integer as an engine handle').
bu1(mode _A,'accepts mode declarations although we are not using them currently').
bu1(module_call(_A,_B),'').
bu1(module _A,'starts a module').
bu1(begin_module(_A),'').
bu1(end_module,'ends current module').
bu1(end_module(_A),'ends module if current, signals erro if not').
bu1(module(_A,_B),'starts a module specifying a list of visible predicates').
bu1(is_module(_A),'recognizes/generates a module name').
bu1(modules(_A),'returns a list of existing modules').
bu1(current_module(_A),'gets name of current module').
bu1(module_predicate(_A,_B,_C),'').
bu1(module_name(_A,_B,_C),'').
bu1(public _A,'declares globally visible predicate Name/Arity').
bu1(is_public(_A),'checks predicate head if globally visible').
bu1(gensym(_A,_B),'generates a new name based on arg 1').
bu1(gensym_no(_A,_B),'generates a new number based on arg 1').
bu1(init_gensym(_A),'resets gensym for names based on arg 1').
bu1(spy_goal(_A),'').
bu1(spy _A,'set spy point on goal, triggering trace when interpreted').
bu1(spying(_A),'checks what we are spying').
bu1(nospy _A,'do not spy on Pred/Arity anymore').
bu1(trace,'trace all predicates when interpreted').
bu1(notrace,'do not trace predicates when interpreted').
bu1(otherwise,'always succeeds').
bu1(false,'always fails').
bu1(call_ifdef(_A,_B),'calls if predicate head is defined, calls arg 2 if not').
bu1(callable(_A),'checks if predicate head is defined (callable)').
bu1(default(_A,_B),'default that can be overriden if asserted'-x(host(_C),_C = localhost)).
bu1(set_default(_A),'asserts arg 1 as default state for use by default/2').
bu1(default_host(_A),'returns default host for remote Linda server').
bu1(set_host(_A),'asserts IP adress or name of server host we want to talk to').
bu1(host(_A),'assumes default host for Linda server').
bu1(default_this_host(_A),'returns default IP address or hostname this machine').
bu1(set_this_host(_A),'asserts IP adress or name of this machine').
bu1(this_host(_A),'assumes default IP adress or name this machiner').
bu1(default_port(_A),'returns default port for remote Linda server').
bu1(set_port(_A),'asserts port number of the server we want to talk to').
bu1(port(_A),'assumes default port for Linda server').
bu1(default_this_port(_A),'returns default port to work as a server on').
bu1(this_port(_A),'assumes default port for to work as a server on').
bu1(set_this_port(_A),'asserts default port for to work as a server on').
bu1(default_timeout(_A),'returns default timeout').
bu1(set_timeout(_A),'asserts default timeout').
bu1(timeout(_A),'assumes default timeout').
bu1(default_login(_A),'returns default (nick)name for user').
bu1(set_login(_A),'asserts default (nick)name for user').
bu1(login(_A),'assumes default (nick)name for user').
bu1(default_password(_A),'returns default password for user').
bu1(set_password(_A),'sets default password for user').
bu1(get_password(_A),'gets default password for user').
bu1(check_password(_A),'checks that password matches default password').
bu1(password(_A),'assumes default password for user').
bu1(copy_term(_A,_B),'returns a copy of arg 1 with fresh variables'-x(f(_C,_C,_D,_D),_E)).
bu1(clone_term(_A,_B,_C),'clone_term(Vs,T,CT) does copy_term(T,C) while keeping unchanged variables Vs - useful if doing things like setarg/3 on the new copy'-x([_D,_E],f(_D,_F,_F,_E),_G)).
bu1(phrase(_A,_B,_C),'(Axiom, ?InputChars, ?OutputChars): DCG evaluator, staring from Axiom'-x(([a],[b]),[a,b|_D],_D)).
bu1(phrase(_A,_B),'(Axiom, ?InputChars): DCG evaluator, starting from Axiom, consuming/producing InputChars').
bu1(nth_member(_A,_B,_C),'retrieves N-th element of a list'-x(_D,[a,b,c],_E)).
bu1(member_i(_A,_B,_C,_D),'').
bu1(saved(_A,_B),'').
bu1(stat_dict(_A,_B),'').
bu1(c_threshold(_A),'').
bu1(c_threshold(_A,_B),'').
bu1(set_c_threshold(_A),'related to C generator: sets length K of WAM instruction block such that block larger than K will get compiled to C code').
bu1(set_c_threshold(_A,_B),'(Min,Max): related to C generator: sets Min,Max length of WAM instruction block such that blocks between Min and Max size will get compiled to C code').
bu1(set_c_trace(_A),'').
bu1('C'(_A,_B,_C),'DCG connect predicate').
bu1(halt,'stops BinProlog').
bu1(quit,'same as halt').
bu1(exit,'same as halt').
bu1(stop,'exits thread or process').
bu1(_A:_B,'M:P calls predicate P hidden in module M').
bu1(sock_read(_A,_B),'reads from a socket when size of the data is described by int before chars to be read').
bu1(sock_readln(_A,_B),'reads from a socket until an end of line LF (ascii 10) or char 0 is found and discards possible previous CR (ascii13)').
bu1(sock_write(_A,_B),'writes a string to a socket prefixed by its lenght').
bu1(sock_writeln(_A,_B),'writes a string to a socket and adds an ascii 10 to the end').
bu1(rpc_test,'tests rpc server and client with socket reuse').
bu1(rpc_test(_A),'tests rpc client with socket reuse for a given number of operations').
bu1(rpc_handler(_A),'(Goal): user-defined rpc handler - filters/calls Goal received on server').
bu1(rpc_server(_A,_B,_C),'Port,Password,Timout: runs Jinni compatible server with socket reuse').
bu1(rpc_server(_A,_B),'Port,Password: runs Jinni compatible server with socket reuse').
bu1(rpc_server,'runs Jinni compatible server with socket reuse on default port').
bu1(service_loop(_A,_B),'ServiceSocket,Password:starts service loop on reusable ServiceSocket - works on server side with server/2').
bu1(start_rpc,'starts rpc client on default local reusable socket and port').
bu1(start_rpc(_A,_B,_C),'(Host,Port,Password): starts rpc client on local reusable socket').
bu1(stop_rpc,'stops rpc client on local reusable socket').
bu1(rpc(_A),'(Query): calls server on current local reusable socket').
bu1(rpc(_A,_B,_C),'(Answer,Goal,Result): calls server on local reusable socket and gets back Result as the(Answer) or no').
bu1(ask(_A,_B,_C,_D,_E),'ask(ClientSocket,X,G,W,R): calls rpc server on on ClientSocket with query X goal G password W and gets result R back - supports socket reuse').
bu1(stop_service,'stops server with socket reuse - acts on the server side').
bu1(show_defaults,'show default values of some system variables').
bu1(hide_default(_A),'makes unavailable a default value').
bu1(show_default(_A),'makes available a default value').
bu1(hostname(_A),'the name of current host, if detected, localhost if not').
bu1(detect_ip_addr(_A),'the ip address of current host, if detected, that of localhost if not').
bu1(detect_user(_A),'guesses the user from environment information').
bu1(run_server,'runs foreground server on localhost (default port) for Jinni clients').
bu1(run_server(_A),'runs foreground server on Port to provide services to Jinni clients').
bu1(run_server(_A,_B),'(Port,Password): runs server on Port, Password required from clients').
bu1(run_server(_A,_B,_C,_D,_E,_F),'(Port,Password,Heap,Stack,Trail,Timeout): runs server with specified service parameters').
bu1(remote_run(_A),'runs Goal on remote server using default password').
bu1(remote_run(_A,_B,_C),'(Host,Port,Goal): runs Goal on rmote server at Host, Port with default password').
bu1(remote_run(_A,_B,_C,_D,_E,_F),'(Host,Port,Answer,Goal,Password,Reply): runs Goal on server at Host, Port with given Password and returns Reply. However, if you do let(where,here) before calling it, a local goal is called instead.').
bu1(handle_service(_A,_B),'handles a Jinni service S with password P - always succedes').
bu1(answer_one_query(_A,_B),'handles a Jinni service S with password P').
bu1(term_decoder(_A,_B),'(Encrypted,Plain): user provided encoder for secure communications - works on lists of ascii codes').
bu1(term_encoder(_A,_B),'(Plain,Encrypted): user provided decoder for secure communications - works on list of ascii codes').
bu1(is_interactive,'checks if toplevel is interactive - use interactive/1 with yes/no to set it the way you want it').
bu1(eq(_A,_B),'unifies arg 1 and arg 2, like =').
bu1(and(_A,_B),'conjunction, like comma').
bu1(compute(_A,_B,_C,_D),'applies Op to arg 2 and arg 3 giving a result').
bu1(println(_A),'synchronized printing of a term on a line').
bu1(read_line(_A),'reads a line into a constant').
bu1(read_words(_A),'reads a line into a list of words').
bu1(write_words(_A),'write list of words to a space separated line').
bu1(near_match(_A,_B),'matches 2 lists of chars').
bu1(to_lower_char(_A,_B),'(Upper,Lower): converts a char to lower case').
bu1(to_upper_char(_A,_B),'(Lower,Upper): converts a char to upper case').
bu1(to_lower_chars(_A,_B),'converts a list of chars to lower case').
bu1(to_upper_chars(_A,_B),'converts a list of chars to upper case').
bu1(char_type(_A,_B),'returns the type of char code: upper,lower,digit, etc. ').
bu1(about_to_bp_comment_3(_A),'').
bu1(to_bp_comment(_A,_B,_C),'gets a predicate of form p_n_info("...") and its first arg, holding comments for p').
bu1(bp_info(_A,_B),'keeps basic help info on BinProlog builtins').
bu1(bp_info(_A,_B,_C),'').
bu1(extract_ex(_A,_B,_C),'').
bu1(info,'generates info on predicates with examples').
bu1(info(_A),'generates info and examples of use for predicate Pred/Arity').
bu1(show_info(_A,_B),'generates components of info/1 output').
bu1(has_info(_A),'checks/generates predicates Pred/Arity for which info is available').
bu1(timed_call(_A,_B,_C,_D),'(Answer,Goal,Timeout,Result) - calls and possibly stops Goal after Timout secs').
bu1(#<(_A,_B,_C),'(Xs): sets the dcg token list to be Xs').
bu1(#>(_A,_B,_C),'(Xs): unifies current dcg token list with Xs').
bu1(#:(_A,_B,_C),'(X): matches X against current dcg token').
bu1(#+(_A,_B,_C),'(X): adds linear assumption +(X) to be consumed at most once, by a #- operation').
bu1(#*(_A,_B,_C),'(X): adds intuitionisic assumption *(X) to be used indefinitely by #- operation').
bu1(#=(_A,_B,_C),'(X): unifies X with any matching existing or future +(X) linear assumptions').
bu1(#-(_A,_B,_C),'(X): consumes +(X) linear assumption or matches *(X) intuitionistic assumption').
bu1(#?(_A,_B,_C),'(X): matches +(X) or *(X) assumptions without any binding').
bu1(the(_A,_B,_C),'the(X,G,R) first answer R=the(X) or R=no, without binding G').
bu1(the(_A,_B),'defined as the(X,G):-the(X,G,the(X))').
bu1(the(_A),'defined as the(G):-the(G,G)').
bu1(strip_cont(_A,_B,_C),'').
bu1(add_cont(_A,_B,_C),'').
bu1(throw(_A),'ISO Prolog exception operator: throws a term to be caught by a matching catch').
bu1(catch(_A,_B,_C),'ISO Prolog exception operator: executes arg 1 and if it catches arg 2, it executes arg 3').
bu1(catch0(_A,_B,_C,_D),'').
bu1(get_cont(_A),'captures current continuation, usually an cyclic term').
bu1(call_cont(_A),'calls arg 1 as current continuation').
bu1(swap_cont(_A,_B),'calls arg 1 as cc and returns cc in arg 2').
bu1('$catch_looking_for_throw'(_A),'(CatchThrowData): continuation marker used by catch/throw').
bu1('$process_catch'(_A,_B,_C),'processes data sent by catch for throw').
bu1(throw_with_cont(_A,_B),'works for throw/1: used to rethrow with new continuation').
bu1(callj(_A,_B,_C),'callj(X,G,R) calls Jinni in Twin Prolog with first answer R=the(X) or R=no, without binding G').
bu1(callj(_A,_B),'callj(G,R) calls Jinni in Twin Prolog with first answer R=the(G) or R=no, without binding G').
bu1(callj(_A),'callj(G) calls Jinni in Twin Prolog with first answer binding G and fails if no answers are found').
bu1(_A@_B,'Hilog call predicate').
bu1(?_A,'simple call tracer').
bu1(count_answers(_A,_B),'counts answers to a goal').
bu1(pushTerm(_A,_B),'writes term to array of ints').
bu1(popTerm(_A,_B),'reads term from array of ints').
bu1(cserver(_A),'runs simple remote server at given port').
bu1(cserver,'runs simple remot server at default port').
bu1(shell_server(_A),'runs simple shell server at given port').
bu1(shell_server,'runs simple shell server at default port').
bu1(traceln(_A),'prints messages on default console').
bu1(hi,'prints hi on default console').



is_maj(65).
is_maj(66).
is_maj(67).
is_maj(68).
is_maj(69).
is_maj(70).
is_maj(71).
is_maj(72).
is_maj(73).
is_maj(74).
is_maj(75).
is_maj(76).
is_maj(77).
is_maj(78).
is_maj(79).
is_maj(80).
is_maj(81).
is_maj(82).
is_maj(83).
is_maj(84).
is_maj(85).
is_maj(86).
is_maj(87).
is_maj(88).
is_maj(89).
is_maj(90).
is_maj(95).
is_maj(192).
is_maj(193).
is_maj(194).
is_maj(195).
is_maj(196).
is_maj(197).
is_maj(198).
is_maj(199).
is_maj(200).
is_maj(201).
is_maj(202).
is_maj(203).
is_maj(204).
is_maj(205).
is_maj(206).
is_maj(207).
is_maj(208).
is_maj(209).
is_maj(210).
is_maj(211).
is_maj(212).
is_maj(213).
is_maj(214).
is_maj(216).
is_maj(217).
is_maj(218).
is_maj(219).
is_maj(220).
is_maj(221).
is_maj(222).

is_min(97).
is_min(98).
is_min(99).
is_min(100).
is_min(101).
is_min(102).
is_min(103).
is_min(104).
is_min(105).
is_min(106).
is_min(107).
is_min(108).
is_min(109).
is_min(110).
is_min(111).
is_min(112).
is_min(113).
is_min(114).
is_min(115).
is_min(116).
is_min(117).
is_min(118).
is_min(119).
is_min(120).
is_min(121).
is_min(122).
is_min(223).
is_min(224).
is_min(225).
is_min(226).
is_min(227).
is_min(228).
is_min(229).
is_min(230).
is_min(231).
is_min(232).
is_min(233).
is_min(234).
is_min(235).
is_min(236).
is_min(237).
is_min(238).
is_min(239).
is_min(240).
is_min(241).
is_min(242).
is_min(243).
is_min(244).
is_min(245).
is_min(246).
is_min(248).
is_min(249).
is_min(250).
is_min(251).
is_min(252).
is_min(253).
is_min(254).

is_num(48).
is_num(49).
is_num(50).
is_num(51).
is_num(52).
is_num(53).
is_num(54).
is_num(55).
is_num(56).
is_num(57).

is_an(48).
is_an(49).
is_an(50).
is_an(51).
is_an(52).
is_an(53).
is_an(54).
is_an(55).
is_an(56).
is_an(57).
is_an(65).
is_an(66).
is_an(67).
is_an(68).
is_an(69).
is_an(70).
is_an(71).
is_an(72).
is_an(73).
is_an(74).
is_an(75).
is_an(76).
is_an(77).
is_an(78).
is_an(79).
is_an(80).
is_an(81).
is_an(82).
is_an(83).
is_an(84).
is_an(85).
is_an(86).
is_an(87).
is_an(88).
is_an(89).
is_an(90).
is_an(95).
is_an(97).
is_an(98).
is_an(99).
is_an(100).
is_an(101).
is_an(102).
is_an(103).
is_an(104).
is_an(105).
is_an(106).
is_an(107).
is_an(108).
is_an(109).
is_an(110).
is_an(111).
is_an(112).
is_an(113).
is_an(114).
is_an(115).
is_an(116).
is_an(117).
is_an(118).
is_an(119).
is_an(120).
is_an(121).
is_an(122).
is_an(192).
is_an(193).
is_an(194).
is_an(195).
is_an(196).
is_an(197).
is_an(198).
is_an(199).
is_an(200).
is_an(201).
is_an(202).
is_an(203).
is_an(204).
is_an(205).
is_an(206).
is_an(207).
is_an(208).
is_an(209).
is_an(210).
is_an(211).
is_an(212).
is_an(213).
is_an(214).
is_an(216).
is_an(217).
is_an(218).
is_an(219).
is_an(220).
is_an(221).
is_an(222).
is_an(223).
is_an(224).
is_an(225).
is_an(226).
is_an(227).
is_an(228).
is_an(229).
is_an(230).
is_an(231).
is_an(232).
is_an(233).
is_an(234).
is_an(235).
is_an(236).
is_an(237).
is_an(238).
is_an(239).
is_an(240).
is_an(241).
is_an(242).
is_an(243).
is_an(244).
is_an(245).
is_an(246).
is_an(248).
is_an(249).
is_an(250).
is_an(251).
is_an(252).
is_an(253).
is_an(254).

is_spec(35).
is_spec(36).
is_spec(38).
is_spec(42).
is_spec(43).
is_spec(45).
is_spec(46).
is_spec(47).
is_spec(58).
is_spec(60).
is_spec(61).
is_spec(62).
is_spec(63).
is_spec(64).
is_spec(92).
is_spec(94).
is_spec(96).
is_spec(126).

is_terminator(-1).
is_terminator(10).
is_terminator(13).

