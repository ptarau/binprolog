server:-
  bg(run_server).

cons:-
  bg(start_consumer),
  wait_for_consumer(X),
  println(redy_to_use=X).

start_consumer:-
  remote_run(consumer(X)),
  println(received=X),
  db_assert(data,range(X)).

wait_for_consumer(X):-
   for(_,1,20),
   sleep(1),
   db_asserted(data,range(X)),
   !.
wait_for_consumer(no_range_found).


consumer(R):-
  wait_for(in_range(ship,R),R<10),
  println(consumed(R)).
  
prod:-
  Bad=11,
  remote_run(producer(Bad)),
  println(produced=Bad),
  sleep(3),
  Good=9,
  remote_run(producer(Good)).

producer(X):-
  notify_about(in_range(ship,X)),
  println(produced(X)).  

/*
server window
?-server.

consumer window
?- cons.
got = 9
yes

producer window

?- prod.
produced = 11
produced = 9
yes
*/
  