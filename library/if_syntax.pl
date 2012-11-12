:-op(970,fx,if).
:-op(980,xfy,then).
:-op(990,xfy,else).

else(then(if(If),Then),Else):-If->Then;Else.

then(if(If),Then):-If->Then.

if_example(if 1>0 then if 2>0 then (write(yes),nl) else (write(no),nl)).
if_example(
  if_max(1,2,3) then write(1) 
  else if 2<0 then write(2) 
  else write(3)
).

if_test:-if_example(T),write(T),nl,display(T),nl,T,nl,nl,fail;nl.

if_max(X,Y,Z) :- if X>Y then Z is X else Z is Y.
