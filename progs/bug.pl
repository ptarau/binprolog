go:-
  quiet(0),
  timed_call(ok,(list_engines,repeat,sleep(1),fail),3,_),
  list_engines,
  fail.

fixed1:-   
   create_engine(256,64,64,E),
   list_engines,
/*
   load_engine(E,append(As,Bs,[A,B,B,A]),As+Bs),
   ask_engine(E,R1),write(R1),nl,
   ask_engine(E,R2),write(R2),nl,
   load_engine(E,member(X,[1,2,3]),X),
   ask_engine(E,R3),write(R3),nl,
   ask_engine(E,R4),write(R4),nl,
*/
   destroy_engine(E).

/*


*/
