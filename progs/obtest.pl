:-[library(objects)].

% tests

'b::plus'(A,B,C):-C is (A+B) mod 10.

'a::inc':-
  x>>X,
  dx>>DX,
  NewX is X+DX,
  x<<NewX.

go:- a with (
         x:=0,
         dx:=1,
         ::inc, 
         ( *(X,Y,Z):- 
             write(here(X,Y,Z)),nl,
             ^ *(X,Y,R), 
             write(there),nl, Z is R mod 2
        )
     ),

     X extends a with true,
  
     b extends X with ::plus/3,

  b ::  
    ( dx>>DX,write(dx=DX),nl,
      inc,inc,x>>AA,
      plus(15,14,BB),
      *(15,14,CC)
    ),

  d with a:=44,

  write([X,AA,BB,CC]),nl,

  oblist.




