% PROGRAM: executable specification of a 
%          container/component coordination logic
% AUTHOR:  Paul Tarau
% UPDATED: Mon Nov 25 08:42:52 AST 1996

% system level

:-op(400,fx,(*)).
:-op(400,fx,(@)).
:-op(400,xfx,(@)).
:-op(400,fx,(?)).
:-op(400,xfx,(?)).

:-[coco_data].

+X:-assumel(X).      % linear assumption
*X:-assumei(X).      % intuitionistic assumption

?X:- assumed(X), (\+ assumed(X)->assumel(X);true).

@X:-assumed(X).

X?Y:- ?contains(X,Y).
X@Y:- @contains(X,Y).

if_assumed(X,How):- 
  \+ (assumed(X),assumed(X))->How=linear
; \+ \+ assumed(X) -> How=intuitionistic
; fail.

assumed(X,How):- 
   assumed(X), 
   (\+ assumed(X)->How=linear
   ;How=intuitionistic
   ).

assume(linear,X):- +X.
assume(intuitionistic,X):- *X.

make_new(Container,Component,How):-
  if_assumed(contains(_,Container),How),
  make_name(Container,Component).

  
make_name(_,Component):-nonvar(Component),!.
make_name(Container,Component):-var(Component),
  ( @counter(Container,Ctr)->NewCtr is Ctr+1
  ; NewCtr is 1
  ),
  + counter(Container,NewCtr),
  termcat(Container,NewCtr,Component).

% user level

% creates Component as required by Container
Container<<Component :-
  make_new(Container,Component,How),
  put(Component,Container,How),!
; errmes(failing,Container<<Component).

% move Component to Container, when allowed 
Component>>Container :- 
  get(Component,_,How),
  put(Component,Container,How),!
; errmes(failing,Component>>Container).

get(Component,From,How):-  
   assumed(contains(From,Component),How).

put(Component,Container,How):-
   nonvar(How),nonvar(Component),nonvar(Container),
   assume(How,contains(Container,Component)),!
;  errmes(failing,put(Component,Container,How)).


init:- 
  % creates a prototype `cloneable' container 
  *contains(cloneable,cloneable),
  % creates a prototype `unique' container 
  +contains(unique,unique).

% application: world with Avatars, Places and Objects

prelude:-
  unique << place,
  unique << avatar,
  unique << goal,
  unique << object,
  unique << money,
  dig(lobby).

is_avatar(X):-avatar?avatar(X).
is_object(X):-object?object(X).
is_money(X):-money?money(X).
is_place(X):-place?X.

new(Kind,X):-termcat(Kind,X,KindX),Kind<<KindX.

login(Who):-
  new(avatar,Who), % adds avatar information
  lobby<<Who,
  +whoami(Who).
  
logout:- @whoami(_).

dig(P) :- 
   place<<P.

whereami(P):-
   ?whoami(Me),
   P?Me,
   is_place(P).
  
craft(Component) :- 
   new(object,Component),
   whereami(Where),
   Where<<Component.

go(To):-
   ?whoami(Me),
   ( is_place(To),
     Me>>To,
     !
   ; errmes(unable(Me),go(To))
   ).
   
clone(O):-
   whereami(P),
   *contains(P,O),
   cloneable << O.

take(O,How):-
   whereami(Where),
   get(O,Where,How).

drop(O,How):-
   whereami(Where),
   put(O,Where,How).

move(What,Where):-
   whereami(Here),
   take(What,How),
   go(Where),
   drop(What,How),
   go(Here).

has(A,O):- avatar(A),object(O),A?O.

