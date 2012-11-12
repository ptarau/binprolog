%  Hor Vert Size  - starts at 0

:-[tetris].

% defines actions as (local) predicate calls
   
scr_dir(D):-remote_run(scr_dir(D)).

scr_init(N):-remote_run(scr_init(N)).
  
scr_end :- remote_run(scr_end).

scr_send(Position,Code):-remote_run(scr_send(Position,Code)).

scr_score(Score):-remote_run(scr_score(Score)).
 
scr_stat(Val):-remote_run(scr_stat(Val)).
