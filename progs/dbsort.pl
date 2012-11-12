go:- dynbbgc,create_engine(8000,500,2000,Engine),
     load_engine(Engine,test(X),X),
     ask_engine(Engine,Answer),
     write(Answer),nl,
     destroy_engine(Engine).

test(working):-
  abolish(i/4),
  consult('wam.txt'),
  functor(G,ii,4),
  findall(G,G,Gs),
  (length(Gs,L1),write(unsorted_facts(L1)),nl,fail;true),
  sort(Gs,Sorted),
  length(Sorted,L),
  statistics,
  write(sorted_facts(L)),nl,
  abolish(ii/4),
  member(X,Sorted),
  assert(X),
  fail.
test(done):-
  functor(G,ii,4),
  findall(G,G,Gs),
  length(Gs,L),
  write(checked_facts(L)),nl.
