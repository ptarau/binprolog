init:-
  bb_let(server,host,localhost),
  bb_let(server,port,7001),
  bb_let(server,password,eureka).

server:-
  bg(run_server).

cons:-
  remote_run(consumer(X)),
  println(got=X).

consumer(R):-
  wait_for(in_range(ship,R),R<10).
  
prod:-
  Bad=11,
  remote_run(producer(Bad)),
  println(produced=Bad),
  sleep(3),
  Good=9,
  remote_run(producer(Good)),
  println(produced=Good).


producer(X):-
  notify_about(in_range(ship,X)).  

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



  