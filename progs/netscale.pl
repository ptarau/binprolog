/* run this on the server side */
light_server:-
  heap(500)=>trail(100)=>stack(100)=>trust.

jserver:-
  run_server(7001,none,400,100,100).
  
one_task(Id,I,IdleTime):-
  println(task_for(Id,I)),
  sleep(IdleTime).

one_client(Id,Times,IdleTime):-
  for(I,1,Times),
    println(start_task(Id,I)),
    remote_run(one_task(Id,I,IdleTime)),
    println(end_task(Id,I)),
    sleep(IdleTime),
  fail.
one_client(Id,_,_):-
  remote_run(println(finished(Id))),
  halt.

/* run this, with various parameters on a W2000 or XP system */
rruns(Clients,Times,IdleTime):-
  for(I,1,Clients),
    swrite(one_client(I,Times,IdleTime),Goal),
    make_cmd(['START bp netscale ',Goal],Cmd),
    println(Cmd),
    bg(system(Cmd)),
    sleep(IdleTime),
  fail
; 
  println(started(all)).

/* example of small test which works fine */
go:-
  G=rruns(20,3,2),
  println(G),
  G.

test:-
  G=rruns(3,2,1),
  println(G),
  G.
    