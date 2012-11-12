:-[tetris].

scr_init(N):-
  max(MaxL,MaxC),
  (for(_,1,60),nl,fail; true),
  [Board]="#",    
  (for(L,6,MaxL),scr_send(p(L,MaxC),Board),fail; true),
  (for(C,0,MaxC),scr_send(p(MaxL,C),Board),fail; true),
  N=0,
  scr_score(N),
  !.
  
scr_end :- 
   max(L,_),L1 is L+3,
   scr_send(p(L1,0),32),nl,
   abort.

scr_send(p(L0,C0),Char):-
  L is L0+1, C is C0+1,
  put(27),
  cwrite('['),cwrite(L),
  cwrite(';'),cwrite(C),
  cwrite('H'),
  put(Char).

scr_score(Score):-
        max(MaxL,_),        MesL is MaxL+1,
		scr_score0(MesL,Score).
		
scr_score0(MesL,Score):-
  scr_send(p(MesL,0),32),
  cwrite('Score:'),cwrite(Score).

scr_stat(Val):-
    max(MaxL,_),L is MaxL+2,
	[Prompt]=">",
    scr_send(p(L,0),Prompt),
    statistics(global_stack,HStat),
    statistics(trail,TStat),
    statistics(bboard,BBStat),
    write('Energie'(Val)),
    write(' Heap'(HStat)),
    write(' Trail'(TStat)),
    write(' BBoard'(BBStat)).
            
% reading a direction: default falling direction
scr_dir(1).

/*
scr_rec(_):-fail.

scr_dir(D):-
        ctime(T0),
        repeat,
        ( scr_rec(C)->usr_dir(C,D)
        ; ctime(T1), DeltaT is T1-T0,DeltaT>0.20,D is 1
        ).
        usr_dir(-77,0).  % right -77
usr_dir(-80,1).  % down -80
usr_dir(-75,2).  % left -75
usr_dir(-72,3).  % up -72
usr_dir(10,-1).  % enter -1
usr_dir(27,0):-scr_end. % escape
*/