

% API

tstest(K,V,R):-term_store_op(0,K,V,R).
push_term(K,T):-term_store_op(1,K,T,_).
put_term(K,T):-term_store_op(2,K,T,_).
new_iterator(K,I):-term_store_op(3,K,0,I).
close_iterator(I):-term_store_op(4,I,0,_).
has_terms(K):-term_store_op(5,K,0,YN), YN=1.
get_next_term(I,T):-term_store_op(6,I,0,T).
remove_current_term(I):-term_store_op(7,I,0,_).
update_current_term(I,T):-term_store_op(8,I,0,T).
delete_all_terms(K):-term_store_op(9,K,0,_).
count_terms(K,N):-term_store_op(10,K,0,N).

new_term(K,R):-term_store_op(11,K,0,R).
instance_of(K,R):-term_store_op(12,K,0,R).
free_term(K):-term_store_op(13,K,0,_).

new_key_iterator(I):-term_store_op(14,0,0,I).

process_term(OpCode,Input,Output):-
  term_store_op(15,OpCode,Input,Output).

% derived operations

get_term(K,T):-get_term(K,0,T).

remove_term(K,T):-get_term(K,1,T).

get_term(K,Remove,T):-
  count_terms(K,N),
  N>0,
  new_iterator(K,Iter),
  for(I,1,N),
  get_next_term(Iter,T),
  (0=:=Remove->true;remove_current_term(Iter)),
  ( I=:=N->
    close_iterator(Iter)
  ; true
  ).

/* similar to findall(Term,get_hash(Key,Term),Terms) */

get_all_terms(Key,Ts):-
  new_iterator(Key,Iterator),
  get_one_more_term(Iterator,Ts),
  close_iterator(Iterator).

/* collects on term at a time from a new iterator */
get_one_more_term(Iterator,[T|Ts]):-
  get_next_term(Iterator,T),
  !,
  get_one_more_term(Iterator,Ts).
get_one_more_term(_Iterator,[]).

