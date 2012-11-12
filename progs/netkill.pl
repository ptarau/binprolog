% this is a killer networking test for various BinProlog
% communication agents: master_servers, server, servants, chat, talk
% listeners

test:-
   spawn((for(I,1,100),write(I),nl,fail;sleep(5))).

spanwait(Goal):-spawn(Goal,[netkill],'temp.pro'),sleep(10).

password(none).

go:-
   detect_ip_addr(H),P1=9100,P2=9200,
   spanwait(master_server(H,7000)=>>run_master_server),
   spanwait(master_server(H,7000)=>>port(P1)=>run_server),   
   spanwait(master_server(H,7000)=>>port(P2)=>run_server),
   spanwait(port(P1)=>>run_servant),
   spanwait(port(P2)=>>run_servant),
   spanwait(port(P1)=>>do_in),
   spanwait(port(P2)=>>do_in),
   spanwait(port(P1)=>>do_out),
   spanwait(port(P2)=>>do_out),
   spanwait(master_server(H,7000)=>>do_yell),
   spanwait(port(P1)=>>do_out),
   spanwait(port(P2)=>>do_out)
   %,spanwait(mix(H))
   .

mix(H):-
   do(host(P1)=>>all(X,Xs1)),write(P1-Xs1),nl,
   do(host(P2)=>>all(X,Xs2)),write(P2-Xs2),nl,
   show_servers,
   show_servants,
   spanwait(master_server(H,7000)=>>do_yell),
   sleep(20),
   host(P1)=>>stop_server,
   sleep(20),
   host(P2)=>>stop_server,
   do(show_servers),
   do(show_servants),
   sleep(10),
   host(7000)=>>stop_server.

do_yell:-
   sleep(5),
   for(I,1,100),symcat(a,I,M),
      do(yell(M)),
   fail
;  halt(0).


do_out:-   
    sleep(5),
    for(I,1,100),
      do(out(a(s(I)))),
    fail
;   halt(0).


do_in:-    
    sleep(5),
    for(_,1,200),
      do(in(a(_))),
    fail
;   halt(0).

do(G):-write('***==>'),
       ( G->write(G),nl
       ; errmes(unexpected_failure,G)
       ).

/* run this on the server side */
light_server:-
  heap(500)=>trail(100)=>stack(100)=>trust.

one_task(I,IdleTime):-
  println(task_for(I)),
  sleep(IdleTime).

one_client(_,Times,IdleTime):-
  for(I,1,Times),
    remote_run(one_task(I,IdleTime)),
  fail.
one_client(Id,_,_):-
  remote_run(println(finished(Id))),
  halt.

/* run this, with vrious parameters on a W2000 or XP system */
rruns(Clients,Times,IdleTime):-
  for(I,1,Clients),
    swrite(one_client(I,Times,IdleTime),Goal),
    make_cmd(['START bp netkill ',Goal],Cmd),
    println(Cmd),
    system(Cmd),
  fail
; 
  println(started(all)).

/* example of small test which works fine */
rtest:-
  G=rruns(20,5,1),
  println(G),
  G.
  