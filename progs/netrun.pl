go:-go(10).

go(N):-go(N,_).
 
go(0,_):-!.
go(N,Old):-
  N>0,
  N1 is N-1,
  println(before(N)),
  remote_run(eq(New,c(Old,Old))),
  println(after_(N)),
  go(N1,New).
  
