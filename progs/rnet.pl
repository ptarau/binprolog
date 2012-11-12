/*
% new networking

% server side

server:-
  server(7001,none).

server(Port,Password):-
  new_server(Port,Server),
  serve(Server,Password).
  
serve(Server,Password):-
  repeat,
    ( new_service(Server,Service)->
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
  'prolog:disconnect'(Service).
  

% client side


ask(Client,X,G,R):-
  'prolog:ask_service'(Client,X,G,none,R).
 
ask(Client,G):-ask(Client,G,G,the(G)).

stop_service(Client):-
  ask(Client,stop_service),
  'prolog:disconnect'(Client).
*/
  
ctest1:-
  ctest(1000,_,true).

ctest2:-
  ctest(100,I,println(I)).
  
ctest3:-
  ctest(100,_,findall(K,for(K,1,10),Ks)),
  println(Ks).
    
ctest(N,I,G):-
  println(ctest(N,G)),
  ctest(N,I,G,T),
  println(time=T).
      
ctest(N,I,G,T):-
  new_client(C),
  ctime(T1),
  for(I,1,N),
     ( ask(C,G)->true
       ; !,println(unexpected_failure(I,G))
     ),
  I=N,
  stop_service(C),
  ctime(T2),
  T is T2-T1.

