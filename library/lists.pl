% some Sicstus/Quintus compatible predicates should go here

%   Adapted from shared code written by Lawrence Byrd & Richard A O'Keefe      

:- module(lists, [
%        append/3,
        delete/3,
        is_list/1,
        last/2,
%        member/2,
%        memberchk/2,
        nextto/3,
        no_doubles/1,
        non_member/2,
        nth/3,
        nth/4,
        nth0/3,
        nth0/4,
        permutation/2,
        prefix/2,
        remove_duplicates/2,
        % reverse/2,
        same_length/2,
        same_length/3,
        select/3,
        sublist/2,
        substitute/4,
        suffix/2,
        max_list/2,
        min_list/2,
        sum_list/2
	]).




%   delete(+List, +Element, ?Residue)
%   is true when all *identical* occurences of Element in List are removed 
%   and the result is Residue.  

delete([], _, []).
delete([Head|Tail], Element, Rest) :-
	Head==Element, !,
	delete(Tail, Element, Rest).
delete([Head|Tail], Element, [Head|Rest]) :-
	delete(Tail, Element, Rest).


%   is_list(+List)
%   is true when List is a proper list.

is_list(X) :- var(X), !, fail.
is_list([]).
is_list([_|Tail]) :- is_list(Tail).


%   last(?List, ?Element)
%   is true when Element is the last element in List.

last([Head|Tail], Element) :- last(Tail, Head, Element).

last([], Element, Element).
last([Head|Tail], _, Element) :- last(Tail, Head, Element).


%   nextto(?X, ?Y, +List)
%   is true when X and Y appears side-by-side in List.

nextto(X, Y, [X,Y|_]).
nextto(X, Y, [_|Rest]) :-
	nextto(X, Y, Rest).


%   no_doubles(+List) 
%   is true when the List contains no duplicate elements.

no_doubles([]).
no_doubles([Head|Tail]) :-
	non_member_(Tail, Head),
	no_doubles(Tail).


%   non_member(+Element, +List)
%   non_member is true when Element does not exist in List.

non_member(Element, List) :-
	non_member_(List, Element).


non_member_([], _).
non_member_([Head|Tail], Element) :-
	dif(Head, Element),
	non_member_(Tail, Element).

%   nth(?N, +List, ?Element)
%   nth/3 is true when Element is the Nth element of List, counting the first
%   element as 1.


nth(N, List, Element) :-
	integer(N), !,
	N >= 1,
	N1 is N-1,
	nth0i(N1, List, Element).
nth(N, List, Element) :-
	var(N),
	nth0v(List, Element, 1, N).


%   nth(?N, +List, ?Element, ?Rest)
%   nth is true when Element is the N:th element in List and Rest is all the
%   elements in the List except Element.

nth(N, List, Element, Rest) :-
	integer(N), !,
	N >= 1,
	N1 is N-1,
	nth0i(N1, List, Element, Rest).
nth(N, List, Element, Rest) :-
	var(N),
	nth0v(List, Element, 1, N, Rest).


%   nth0(?N, +List, ?Element)
%   nth0/3 is true when Element is the Nth element of List, counting the first
%   element as 0.


nth0(N, List, Element) :-
	integer(N), !,
	N >= 0,
	nth0i(N, List, Element).
nth0(N, List, Element) :-
	var(N),
	nth0v(List, Element, 0, N).

nth0v([Element|_], Element, Index, Index).
nth0v([_|Tail], Element, M, Index) :-
	N is M + 1,
	nth0v(Tail, Element, N, Index).

nth0i(0, [Head|_], Head) :- !.
nth0i(N, [_|Tail], Element) :-
	M is N - 1,
	nth0i(M, Tail, Element).


%   nth0(?N, +List, ?Element, ?Rest)
%   nth0/4 unifies Element with the nth element in List, counting the
%   first element as 0 and Rest with rest of the elements.

nth0(N, List, Element, Rest) :-
	integer(N), !,
	N >= 0,
	nth0i(N, List, Element, Rest).
nth0(N, List, Element, Rest) :-
	var(N),
	nth0v(List, Element, 0, N, Rest).

nth0v([Element|Tail], Element, Index, Index, Tail).
nth0v([Head|Tail], Element, M, Index, [Head|Rest]) :-
	N is M + 1,
	nth0v(Tail, Element, N, Index, Rest).

nth0i(0, [Head|Tail], Head, Tail) :- !.
nth0i(N, [Head|Tail], Element, [Head|Rest]) :-
	M is N - 1,
	nth0i(M, Tail, Element, Rest).


%   permutation(?List, ?Perm)
%   is true when Perm is a permutation of List.

permutation([], []).
permutation(List, [First|Perm]) :- 
	select(First, List, Rest),
	permutation(Rest, Perm).


%   prefix(?Prefix, +List)
%   is true when Prefix is a prefix of List.

prefix([], _).
prefix([X|PreTail], [X|Tail]) :-
	prefix(PreTail, Tail).


%   remove_duplicates(+List, ?Pruned)
%   is true when Pruned is like List but with all *identical* duplicate 
%   elements removed.

remove_duplicates([], []).
remove_duplicates([Head|Tail1], [Head|Tail2]) :- 
	delete(Tail1, Head, Residue),
        remove_duplicates(Residue, Tail2).

/*
%   reverse(?List, ?Reversed)
%   is true when Reversed is has the same element as List but in a reversed 
%   order. List must be a proper list.

reverse(List, Reversed) :-
	reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([Head|Tail], SoFar, Reversed) :-
	reverse(Tail, [Head|SoFar], Reversed).
*/

%   same_length(?List1, ?List2)
%   is true when List1 and List2 have the same number of elements.

same_length([], []).
same_length([_|L1], [_|L2]) :-
	same_length(L1, L2).


%   same_length(?List1, ?List2, ?Length)
%   is true when List1 and List2 have the same number of elements and
%   the length is Length.

same_length(L1, L2, Length) :-
	nonvar(Length), !,
	length(L1, Length),
	length(L2, Length).
same_length(L1, L2, Length) :-
	same_length(L1, L2, 0, Length).

same_length([], [], N, N).
same_length([_|L1], [_|L2], N0, N) :-
	N1 is N0+1,
	same_length(L1, L2, N1, N).



%   select(?Element, ?List, ?List2)
%   is true when the result of removing an occurrence of Element in List
%   is List2.

select(Element, [Element|Tail], Tail).
select(Element, [Head|Tail1], [Head|Tail2]) :- 
	select(Element, Tail1, Tail2).


%   sublist(?Sub, +List)
%   is true when all members of Sub are members of List

sublist(List, List).
sublist(Sub, [Head|Tail]) :- sublist_(Tail, Head, Sub).

sublist_(Sub, _, Sub).
sublist_([Head|Tail], _, Sub) :- sublist_(Tail, Head, Sub).
sublist_([Head|Tail], X, [X|Sub]) :- sublist_(Tail, Head, Sub).


%   substitute(?X, ?Xlist, ?Y, ?Ylist)
%   is true when Xlist and Ylist are identical lists except for the 
%   corresponding elements X and Y.


substitute(_, [], _, []) :- !.
substitute(OldElem, [OldHead|OldRest], NewElem, [NewElem|NewRest]) :-
	OldElem==OldHead, !,
	substitute(OldElem, OldRest, NewElem, NewRest).
substitute(OldElem, [NotElem|OldRest], NewElem, [NotElem|NewRest]) :-
	substitute(OldElem, OldRest, NewElem, NewRest).


%   suffix(?Suffix, +List)
%   is true Suffix is an ending part of List.

suffix(Suffix, Suffix).
suffix(X, [_|Tail]) :-
	suffix(X, Tail).

%   max_list(+ListOfNumbers, ?Max)
%   is true when Max is the greatest of the numbers in the ListOfNumbers.


max_list([Head|Tail], Max) :- 
	max_list(Tail, Head, Max).

max_list([], Max, Max).
max_list([Head|Tail], Element, Max) :-
	Head =< Element, !,
	max_list(Tail, Element, Max).
max_list([Head|Tail], _, Max) :-
	max_list(Tail, Head, Max).


%   min_list(+ListOfNumbers, ?Min)
%   is true when Min is the smallest of the numbers in ListOfNumbers.

min_list([Head|Tail], Min) :- 
	min_list(Tail, Head, Min).

min_list([], Min, Min).
min_list([Head|Tail], Element, Min) :-
	Head >= Element, !,
	min_list(Tail, Element, Min).
min_list([Head|Tail], _, Min) :-
	min_list(Tail, Head, Min).


%   sum_list(+ListOfNumbers, ?Sum)
%   is true when the sum of ListOfNumbers is Sum. It can be used to check
%   a sum or to calculate the sum.

sum_list(List, Sum) :-
	sum_list(List, 0, Sum).

sum_list([], Sum, Sum).
sum_list([Head|Tail], Sum0, Sum) :-
	Sum1 is Head+Sum0,
	sum_list(Tail, Sum1, Sum).

:-end_module.
