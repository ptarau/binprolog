% fairly efficient legacy recorda-recordz familly
% still useful for running old programs

% sorry for explicit Refs, but as they are not present in ISO prolog 
% anymore, and are unsafe with gc, they are discontinued...
% they will be emulated by returning the terms themselves
% but this will be inefficient

record(Key,Term,Term):-record(Key,Term).
recorda(Key,Term,Term):-recorda(Key,Term).
recordz(Key,Term,Term):-recordz(Key,Term).
recorded(Key,Term,Term):-recorded(Key,Term).
erase(Term):-recorded_key(Key),erase(Key,Term),!.

% end of */3 emultaion

record(Key,Term):-recordz(Key,Term).

recorda(Key,Term):-
  pushq('$recorded',Key,Term),
  record_key(Key).

recordz(Key,Term):-
  addq('$recorded',Key,Term),
  record_key(Key).

recorded(Key,Term):-var(Key),!,
   recorded_key(Key),
   get_recorded(Key,Term).
recorded(Key,Term):-
   get_recorded(Key,Term).

erase(Key,Term):-
  cdelq('$recorded',Key,Term,_).

current_key(Name,Key):-
  recorded_key(Key),
  functor(Key,Name,_).

% hidden tools

record_key(Key):-
  functor(Key,F1,N1),
  rmemoq('$keys','$recorded',F1/N1).

recorded_key(Key):-
  cmembq('$keys','$recorded',F1/N1),
  functor(Key,F1,N1).

get_recorded(Key,Term):-
   cmembq('$recorded',Key,Term).


% adds a (assumed ground) object unless it is there
rmemoq(Key,Name,X):-
  cmembq(Key,Name,X),!.
rmemoq(Key,Name,X):-
  addq(Key,Name,X).
