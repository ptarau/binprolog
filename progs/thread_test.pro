go :- bg(say_hello, Thread, EngineAddr, EngineId),
      println(thread(Thread)),
        println(engine(Thread,EngineAddr,same(EngineId))),
        !.

say_hello :-
        println('hello from background thread'),
        current_engine_addr(Addr),
        current_engine_id(Id),
        current_thread(T),
        println(bg_engine(T,Addr,same(Id))),
        !.

go1:-
  bg(bg_process,Thread),
  fg_process(Thread),
  println(threads_joined).

bg_process:-
  println(starting(bg_process)),
  local_in(a(X)),
  println(bg_process_got(a(X))),
  sleep(10),
  println(bg_meeting_point_1).
  
fg_process(Thread):-
  sleep(5),
  println(starting(fg_process)),
  local_out(a(1)),
  thread_join(Thread),
  println(fg_meeting_point_1_with(Thread)).

 