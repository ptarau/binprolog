/*
% wrap_thread(Goal):-capture_cont_for(Goal).
wrap_thread(Goal,LeftOver):-left_over_cont(LeftOver)-::capture_cont_for(Goal).
  
move:-
  % assumes wrap_thread(...) has been executed before
  call_with_cont(run_and_return).

run_and_return(Gs):-
  println(Gs),
  the(
    from_to(Gs,Cont),
    wrap_thread(Gs,Cont), % this send left over
    Result
  ),
  println(Result),
  eq(the(from_to(Gs,NewGs)),Result),
  ( var(NewGs)->true
  ; NewGs
  ).

return:-
  call_with_cont(collect_left_over),
  true. % should be here: simplifies cont. cutting algo

collect_left_over(Gs):- assumed(left_over_cont(Gs)).
*/  

% should send canonical terms over sockets !!!

go:-wrap_thread(go1).

go1:-
 eq(X,1),
 there,move,
   member(Y,[X,2,X]),println(remote_values(X,Y)),return,
 println(back(X,Y)).


go2:-
  there,move,println(there),return.
