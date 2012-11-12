% The Simpsons and the Royals

parent_of(homer, abe).
parent_of(maggie,homer).
parent_of(bart,homer).
parent_of(lisa,homer).
parent_of(maggie, marge).
parent_of(lisa, marge).
parent_of(bart, marge).
parent_of(homer,liz).
parent_of(charles,liz).
parent_of(charles,abe).
parent_of(william,charles).

male(bart).
male(homer).
male(abe).
male(charles).
male(william).

female(maggie).
female(marge).
female(lisa).
female(liz).

% rules

child_of(Parent,Child):-
  parent_of(Child,Parent).

father_of(Child,Father):-
  parent_of(Child,Father),
  male(Father).

mother_of(Child,Mother):-
  parent_of(Child,Mother),
  female(Mother).

sibling_of(Child,Other):-
  parent_of(Child,Parent),
  parent_of(Other,Parent),
  not(Child=Other).

sister_of(Child,Sister):-
  sibling_of(Child,Sister),
  female(Sister).

brother_of(Child,Brother):-
  sibling_of(Child,Brother),
  male(Brother).

aunt_or_uncle_of(Child,Person):-
   parent_of(Child,Parent),
   sibling_of(Parent,Person).

aunt_of(Child,Aunt):-
  aunt_or_uncle_of(Child,Aunt),
  female(Aunt).

uncle_of(Child,Uncle):-
  aunt_or_uncle_of(Child,Uncle),
  male(Uncle).

cousin_of( Child, Cousin):- 
  aunt_or_uncle_of(Child, Person), 
  child_of(Person,Cousin).

grand_parent_of(Child,GP):-
  parent_of(Child,P),
  parent_of(P,GP).

grand_father_of(Child,GP):-
  grand_parent_of(Child,GP),
  male(GP).

unique_call(Goal):-
  findall(Goal,Goal,Instances),
  sort(Instances,Sorted),
  member(Goal,Sorted).

ancestor_of(Child,Parent):-
  parent_of(Child,Parent).

ancestor_of(Child,Parent):-
   parent_of(Child,Person),
   ancestor_of(Person,Parent).

  
