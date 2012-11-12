bm:-random_seed(13),go,show_game.

:-[tetris].

scr_init(N):-
  max(MaxL,MaxC),
  [Board]="#",    
  (for(L,6,MaxL),scr_send(p(L,MaxC),Board),fail; true),
  (for(C,0,MaxC),scr_send(p(MaxL,C),Board),fail; true),
  N=0,
  scr_score(N),
  !.
  
scr_end :- 
   println(end),
   abort.

scr_send(p(L0,C0),Char):-let(L0,C0,Char).
  
scr_score(Score):-
  show_game,        
  println('Score:'(Score)),
  nl.

% show_game.
  
show_game_:-
   max(MaxL,MaxC),
   for(L,1,MaxL),
     nl,
     for(C,1,MaxC),
       val(L,C,Char),
       put(Char),
   fail.
show_game_:-
   nl,nl.

show_game.

scr_stat(Val):-
    statistics(global_stack,HStat),
    show_game,
    % println([energie(Val),heap(HStat)]),
    nl.
              
% reading a direction: default falling direction
scr_dir(1).
