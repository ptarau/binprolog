p:-[lsort].

% modified version of a Lolli program (sort.ll) by J. Hodas
% I have added \+ hyp and got rid of original explicit continuation passing
% as in BinProlog -<> is already defined in terms of assumel/1

collect([]).
collect([X]) :- hyp(X).
collect([X,Y|L]) :- hyp(X), collect([Y|L]), X @=< Y.

unpack([]).
unpack([X|L]) :- assumel(hyp(X)), unpack(L).

lsort(L,K) :- unpack(L), collect(K), \+ hyp(X).

go:-lsort([10,2,33,10,24,2],R),write(R),nl,fail.
go.
