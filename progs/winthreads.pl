lout(X):-
  synchronize_on(1,(
    retract1(waiting(X,T)),assert(X)
  ))
  ,!,
  thread_resume(T).
lout(X):-
  synchronize_on(1,assert(X)).

lcin(X):-synchronize_on(1,retract1(X)).

lin(X):-lcin(X),!.
lin(X):-
  current_engine_thread(T),
  synchronize_on(1,assert(waiting(X,T))),
  thread_suspend(T),
  println(here),
  lcin(X).


rtest1:-rtest(a,1000).
rtest2:-rtest(b,1000).
rtest3:-rtest(c,1000).
mtest:-there,move,for(I,1,5000),println(I),fail.

rtest(F,N):-
  G=..[F,I],
  for(I,1,N),
  println(G),
  remote_run(println(G)),
  fail
; true.


go4:-rtest(10).


rtest(N):-
  retractall(b(_)),
  bg((
    for(I,1,N),
	   wait_for(a(X),(X mod 3 =:= 0; X mod 5 =:= 0)),
	   local_out(b(X)),
	fail
  )),
  ( for(I,1,N), 
      notify_about(a(I)),
    fail
  ; sleep(5),
    listing(available_for),
	listing(waiting_for),
	listing(holds_for)
  ).

go1:-for(N,1,5),bg(goal(N),Thread),write(launching(task(N,Thread))),nl,fail.
goal(N):-for(I,1,5),write(task(N)),write(I),nl,sleep(1),fail.

go2:-bg(goal1,Thread),write(Thread),nl.

goal1:-for(I,1,5),write(I),nl,sleep(1),fail.

test(N):-
  retractall(b(_)),
  bg((
    for(I,1,N),local_in(a(I,J)),local_out(b(J)),fail
  )),
  ( for(I,1,N),
      local_out(a(X,X)),
    fail
  ; sleep(5),
    listing(waiting),
	listing(b)
  ).

go3:-test(100).

/*
prime_before(Max,I):-
  for(I,1,Max),
  not(divisible(I)).

divisible(I):-Limit is integer(sqrt(I)),for(J,2,Limit),I mod J=:=0,!.
*/

go:-go1,fail;go2,fail;go3,fail;true.
