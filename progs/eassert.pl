% emulating assert with backtrackable assumptions 
% on top of an extra engine in BinProlog 4.xx

new_engine(E):-
    engine_params(H,S,T),
    new_engine(H,S,T,make_worker(X),X,E).

new_engine(H,S,T,Goal,Answer,Handle):-
  create_engine(H,S,T,Handle),
  load_engine(Handle,Goal,Answer).

send_engine(E,Goal):-assumel(todo(Goal)),ask_engine(E,_).

make_worker(E):-current_engine(E),worker.

worker:-
  todo(X),X\==done,
  copy_term(X,G),
  (G->true;true),
  co_worker.

co_worker.
co_worker:-worker.

eassert(E,A):-send_engine(E,assumei(A)).

% query

go:-
  engine_params(200,100,100)=>
  new_engine(E),
     send_engine(E,ttyprint(hello)),
     eassert(E,a(1)),
     eassert(E,a(2)),
     listing,
  send_engine(E,done),
  destroy_engine(E).

/*
?-go.

hello
% assumed a/1:
a(1).
a(2).

% assumed todo/1:

% assumed engine_params/3:
*/
