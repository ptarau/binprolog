go1:-bg(listen1),talk1.
go2:-bg(listen2),talk2.

listen1:-
  set_this_port(5001),
  trust.

talk1:-
  set_that_port(5002),
  for(I,1,5),
    remote_run(println(one(I))),
  fail.

listen2:-
  set_this_port(5002),
  trust.

talk2:-
  set_that_port(5001),
  for(I,1,5),
    remote_run(println(two(I))),
  fail.

one:-
  set_that_port(5002),
  remote_run(
    and(
      set_that_port(5003),
      remote_run(println(hello))
    )
  ).

two:-
  set_this_port(5002),
  trust.

three:-
  set_this_port(5003),
  trust.
