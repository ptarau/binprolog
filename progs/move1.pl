fg(Goal):-end_cont-::capture_cont_for(Goal).
  
move:-
  % assumes fg(...) has been executed before
  call_with_cont(run_and_return).

run_and_return(Gs):-
  println(Gs),
  the(Gs,Gs,R),
  eq(the(Gs),R).

go:-fg(test).

test:-eq(X,1),there,move,and(member(Y,[X,2,X]),println(remote_values(X,Y))).
