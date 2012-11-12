scr_init(N):-
  dims(MaxL,MaxC,_),
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
  
show_game:-
   dims(MaxL,MaxC,_),
   for(L,0,MaxL),
     nl,
     for(C,0,MaxC),
       get_val(L,C,Char),
       put(Char),
   fail.
show_game:- 
   sleep(1),
   nl,nl.

get_val(L,C,Char):- val(L,C,X),!,Char=X.
get_val(_,_,Space):- [Space]=" ".
     
scr_stat(Val):-
    statistics(global_stack,HStat),
    show_game,
    println([energie(Val),heap(HStat)]),
    nl.
              
% reading a direction: default falling direction
scr_dir(1).

