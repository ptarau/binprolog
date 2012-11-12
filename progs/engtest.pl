:-[allperms].

go1:-
  write(starting),nl,
  create_engine(256,64,64,E1),
  write(create=E1),nl,
  create_engine(128,64,64,E2),
  write(create=E2),nl,
  destroy_engine(E1),
  destroy_engine(E2),
  write(kill=E1+E2),nl.

new_engine(E):-create_engine(256,64,64,E).

go2:-create_engine(256,64,64,E1),create_engine(100,32,32,E2),
    load_engine(E1, append([1,2],[3,4],Xs), Xs),
    ask_engine(E1,R1),write(R1),nl,
    load_engine(E2, (statistics,append(As,Bs,[A,B,B,A])), As+Bs),
    ask_engine(E2,R2),write(R2),nl,
    destroy_engine(E2),
    destroy_engine(E1).

go3:-
   create_engine(256,64,64,E),
   G=append(As,Bs,[A,B,B,A]),
   load_engine(E,G,As+Bs),
   ask_engine(E,R1),copy_term(R1,C1),
   write(G=>R1),nl,
   ask_engine(E,R2),copy_term(R2,C2),
   write(C1),nl,
   write(C2),nl,
   ask_engine(E,R),write(R),nl,
   ask_engine(E,R),write(R),nl,
   load_engine(E,member(X,[1,2,3]),X),
   ask_engine(E,S),write(S),nl,
   ask_engine(E,S1),write(S1),nl,
   ask_engine(E,S2),write(S2),nl,
   (ask_engine(E,R)->write(R),nl;true),
   destroy_engine(E).

go4:-
  create_engine(256,64,64,E), 
  load_engine(E,(go;statistics),true),
  ask_engine(E,true).

go5:-   
   create_engine(256,64,64,E),
   load_engine(E,append(As,Bs,[A,B,B,A]),As+Bs),
   ask_engine(E,R1),write(R1),nl,
   ask_engine(E,R2),write(R2),nl,
   load_engine(E,member(X,[1,2,3]),X),
   ask_engine(E,R3),write(R3),nl,
   ask_engine(E,R4),write(R4),nl,
   destroy_engine(E).

make_engine(Goal,Answer,E):-
   new_engine(E),
   load_engine(E,Goal,Answer).

suspend:-suspend_engine(0).

consume(X):-repeat,suspend,nonvar(X),assert(answer(X)),!.

produce(X):-perm([1,2,3],X),suspend,write(after),nl.
   
go6:-
  make_engine(produce(X),X,P),write(prolog_engine=P),nl,
  make_engine(consume(X),X,C),write(prolog_engine=C),nl,
  multitask_engines(5000),
%  ask_engine(P,R),
%  ask_engine(C,R),
  listing.
   

