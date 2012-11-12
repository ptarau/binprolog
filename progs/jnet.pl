% :-[netscale].

% Jinni compatibility mappings

write_to(Socket,Codes):-
  sock_write(Socket,Codes).

read_from(Socket,Codes):-
  sock_read(Socket,Codes).
  
decode_term(Codes,Term):-call_ifdef(term_decoder(Codes,Term),term_chars(Term,Codes)).
encode_term(Term,Codes):-call_ifdef(term_encoder(Term,Codes),term_chars(Term,Codes)).

% client side RPCs

disconnect(ServiceOrClient):-
  close_socket(ServiceOrClient).

ask_query_get_answer(Client,QW,A):-
  write_to(Client,QW),
  read_from(Client,A).

ask_service(Client,X,G,W,R):-
  encode_term(the(X,G,W),QW),
  ask_query_get_answer(Client,QW,A),
  decode_term(A,T),
  !,
  R=T.
ask_service(_,_,_,_,no).
    
ask_jserver(H,P,X,G,W,R):-
  new_client(H,P,Client),      
  ask_service(Client,X,G,W,R),
  disconnect(Client).

jremote_run(G):-jremote_run(localhost,7001,G).
jremote_run(H,P,G):-jremote_run(H,P,G,G,the(G)).
jremote_run(H,P,X,G,R):-remote_run(H,P,X,G,none,R).

% API element
remote_run(H,P,X,G,W,R):-ask_jserver(H,P,X,G,W,R).


% server side RPC handling API
   
run_jserver:-run_jserver(7001).

run_jserver(Port):-
  run_server(Port,none).

run_server(Port,Password):-
  % can be set with set_default(heap(...)) etc.
  heap_size(Heap),
  stack_size(Stack),
  trail_size(Trail),
  run_server(Port,Password,Heap,Stack,Trail).
   
run_server(Port,Password,Heap,Stack,Trail):-
  new_server(Port,Server),
  repeat,
    new_service(Server,Service),
    %bg(handle_service(Service,Password)),
    bg(handle_service(Service,Password),Heap,Stack,Trail,  _Thread,_Engine,_Id),
  fail.
 
% END RPC API
  
handle_service(Service,Password):-
  if(answer_one_query(Service,Password),true,true),
  disconnect_service(Service).
 
answer_one_query(Service,Password):-
  read_from(Service,QString),
  decode_term(QString,QTerm),
  react_to(QTerm,Password,ATerm),
  encode_term(ATerm,AString),
  write_to(Service,AString).
  
disconnect_service(Service):-
  disconnect(Service).
  
% defines how RPC requests are handled

react_to(Term,Password,Answer):-
   run_query_term(Term,Result,Password),
   !,
   Answer=the(Result).
react_to(_,_,Answer):-
   Answer=no.

run_query_term(the(Answer,Goal,CPassword),Answer,SPassword):-call_here(CPassword,SPassword,Goal).
run_query_term(run(CPassword,Answer,Goal),Answer,SPassword):-call_here(CPassword,SPassword,Goal). % BinProlog compatibility?

call_here(CP,SP,G):-CP==SP,!,topcall(G).
call_here(CP,_,G):-println(warning(wrong_password_for_query(G),passwd(CP))).

