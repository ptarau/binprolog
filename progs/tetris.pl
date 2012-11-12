% Tetris in Prolog (C) Paul Tarau 1989-2001

% ALGORITHM:
% minimize the energy of the surface of the block profile
% = the sum of the heights of the blocs - empty spaces included
% (after a hypothetical fall)
 
% Tetris 10.1

dims(20,10,32).

max(L,C):-dims(L,C,_).

g:-go(game).

go:-go(demo).

go(Mode):-
   init(State),
   play(Mode,State),
   scr_end.

init(surface(N,[])):-
   init_best,
   scr_init(N).
    
% logic
    
rtest:-for(_,1,50),random(20,R),println(R),fail.

random(Max,R):-
  random(N),
  R is N mod Max.  

cputime(X):-statistics(runtime,[X,_]).

% directions and axes: Dir -> p(DeltaL,DeltaC)
dir_move(0,p( 0, 1)). % right *---> p(_,C)
dir_move(1,p( 1, 0)). % down  |
dir_move(2,p( 0,-1)). % left  |
dir_move(3,p(-1, 0)). % right \/
% ---------------------------- %p(L,_) 


% movements

next(Dir,p(L1,C1),p(L2,C2)):-
  max(MaxL,MaxC),
  dir_move(Dir,p(DL,DC)),
  L2 is L1+DL,C2 is C1+DC,
  L2>=0,L2<MaxL,C2>=0,C2<MaxC.

select_block(-1,block(T,O1,P),block(T,O2,P)):-!,
  O2 is (O1+1) mod 4.
select_block(Dir,block(T,O,P1),block(T,O,P2)):-
  next(Dir,P1,P2).


% objects

/* image(Type,[Dir|Ds]) */
image(0,[0,1,0]).     % z.
image(1,[1,0,1]).     % -z.
image(2,[0,1,1]).     % l.
image(3,[0,0,1]).     % -l.
image(4,[1,1,1]).     % i.
image(5,[0,1,2]).   % square.
image(6,[0,0,2,1]).   % t.

max_image(7).

block2parts(B,Qs):-
  B=block(Type,_,_),
  image(Type,Dirs),
  block2parts(Dirs,B,Ps),
  sort(Ps,Qs).

block2parts([],block(_,_,P),[P]):-!.
block2parts([D|Ds],block(T,O,P1),[P1|Ps]):-
  Dir is (O+D) mod 4,
  next(Dir,P1,P2),
  !,
  block2parts(Ds,block(T,O,P2),Ps).

display_parts([],_):-!.
display_parts([P|Ps],Image):-
  scr_send(P,Image),
  !,
  display_parts(Ps,Image).

/* Part and Empty are ascii codes for '*' and ' '
   Sending ' ' for blocks in Old hides them on the display
   Sending '*' for blocks in New shows them on the display
*/
move_parts(Old,New):- 
  "* "=[Part,Empty],
  display_parts(Old,Empty),
  display_parts(New,Part).   

test_parts(Parts,Surface):-
  member(X,Parts),
  member(X,Surface),
  !,
  fail.
test_parts(_,_).

/* moves a shape unless it colides with surface */
move_block(B1,B2,R):-
    block2parts(B2,New),
    test_parts(New,R),
    block2parts(B1,Old),
    move_parts(Old,New).

/* removes a full row */
compress_surface(surface(N1,R1),surface(N2,R2)):-
    max(_,MaxC),
    bagof(surface(Full,L-Cs),
      Len^(
        bagof(C,member(p(L,C),R1),Cs),
        length(Cs,Len),
        ( Len=MaxC->Full=1
        ; Full=0
        )
      ),
    BLCs),
    !,
    eliminate(BLCs,LCs,N),
    !,
    N>0,N2 is N1+N,
    elements(LCs,R2).

element(LCs,p(L,C)):-member(L-Cs,LCs),member(C,Cs).

elements(LCs,Ps):-findall(P,element(LCs,P),Ps).

eliminate([],[],0):-!.
eliminate([L|Ls],Rs2,N2):-
    eliminate(Ls,Rs1,N1),
    remove(L,Rs1,Rs2,N1,N2).

remove(surface(1,_),Rs,Rs,N1,N2):-N2 is N1+1,!.
remove(surface(0,L-Cs),Rs1,[L1-Cs|Rs1],N,N):-L1 is L+N.

touch(p(L,_),_):-max(M,_),L>=M,!. % no free space down
touch(P,Ps):-member(P,Ps),!.


% try

try_animate_block(B,surface(N,R1),surface(N,R2)):-
	block2parts(B,Ps),
	try_let_fall_block(Ps,R1,R2).

try_let_fall_block(Ps1,R1,R2):-
    try_change_block(Ps1,Ps2,R1),
    !,
    try_let_fall_block(Ps2,R1,R2).
try_let_fall_block(Ps,R1,R3):-     
    det_append(Ps,R1,R2),
    !,
    try_reduce(R2,R3).

try_reduce(R1,R2):-
    compress_surface(surface(0,R1),surface(_,R2)),
    !.
try_reduce(R,R).

let_fall([],[],_):-!.
let_fall([p(L1,C)|Ps1],[p(L2,C)|Ps2],MaxL):-
	L2 is L1+1,
	L2<MaxL,
	!,
	let_fall(Ps1,Ps2,MaxL).

try_change_block(Bs1,Bs2,R):-
  max(MaxL,_),
  let_fall(Bs1,Bs2,MaxL),
  ( member(X,Bs2),member(X,R)->fail
  ; true
  ),
  !.

% evaluer

minimize_surface_energy(B0,_,surface(N,R1)):-
  B0=block(Type,_,_),
  re_init_best(B0),
  generate_block(Type,B),
  try_animate_block(B,surface(N,R1),surface(_,R2)),
  energy(R2,Val),
  the_best(OldB,OldVal),
  Val<OldVal,
  set_best(B,Val),
  move_block(OldB,B,R1),
  scr_stat(Val),
  fail.
minimize_surface_energy(_,B,_):-
  the_best(B,_).
    
generate_block(Type,block(Type,Orientation,p(3,C))):-
    max(_,MaxC),
    MaxC1 is MaxC-1,
    for(C,0,MaxC1),
    for(Orientation,0,3).

surface(R1,S):-
    findall(C-Ls,
        setof(L,member(p(L,C),R1),Ls),
    CLs),
    findall(p(L1,C0),
       (member(C0-[L0|_],CLs),L1 is L0-1),
    S).

energy(Parts,G):-
    surface(Parts,S),
    energy(S,0,G).

energy([],G,G):-!.
energy([p(L,_)|Ps],G1,G3):-
    max(MaxL,_),
    G2 is G1+((MaxL-L)*(MaxL-L+1) // 2),
    !,
    energy(Ps,G2,G3).


init_best:- re_init_best(best(block(3,0,p(1,1)))).

re_init_best(B):- set_best(B,99999).

set_best(B,V):- retractall(best(_,_)), assert(best(B,V)).

the_best(Bloc,V):- is_dynamic(best(_,_)),best(Bloc,V).

% animate

impact(B,surface(N,R1),surface(N,R2)):-
  block2parts(B,Ps),     
  member(p(L,C),Ps),L1 is L+1,
  touch(p(L1,C),R1),
  det_append(Ps,R1,R2),
  !.

reduce(surface(N1,R1),surface(N2,R2)):-
  max(_,MaxC),
  compress_surface(surface(N1,R1),surface(N2,R2)),
  move_parts(R1,R2),
  S is MaxC*N2,
  scr_score(S), % computes score
  !.
reduce(R,R).

change_block(B1,B2,surface(_,R)):-
    scr_dir(Dir),
    select_block(Dir,B1,B2),
    move_block(B1,B2,R),
    !.
change_block(B,B,_).

animate_block(B,R1,R3):-impact(B,R1,R2),!,
  reduce(R2,R3).
animate_block(B1,R1,R2):-
  change_block(B1,B2,R1),
  !,
  animate_block(B2,R1,R2).

% play

full(Surface):-member(p(L,_),Surface), L=<5. % no free space up

create_block(block(Type,0,p(5,MidC))):-!,
  max(_,MaxC),MidC is MaxC // 2,
  max_image(Ctr),
  random(Ctr,Type).

best_block(game,B0,B0,_):-!.
best_block(demo,B0,B,NR):-
  minimize_surface_energy(B0,B,NR).

play(_,surface(_,Surface)):-full(Surface),!.
play(Mode,Surface1):-
  create_block(B0),
  best_block(Mode,B0,B,Surface1),
  !,
  animate_block(B,Surface1,Surface2)
  ,!,
  play(Mode,Surface2).
