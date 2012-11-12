
% DCGs with no preprocessor
% expects: Sicstus-style backtrackable setarg +
% BinProlog's backtrackable global variables 

% tools

begin_dcg(Name,Xs):-lval(dcg,Name,Xs-Xs).

end_dcg(Name,Xs):-lval(dcg,Name,Xs-[]).

w(Word,Name):-
  lval(dcg,Name,State),
  State=_-[Word|Xs2],
  setarg(2,State,Xs2).

begin_dcg(Xs):-begin_dcg(default,Xs).
end_dcg(Xs):-end_dcg(default,Xs).
w(Word):-w(Word,default).

% grammar
x:-ng,v.

ng:-a,n.

a:-w(the).
a:-w(a).

n:-w(cat).
n:-w(dog).

v:-w(walks).
v:-w(sleeps).

% test
go:-begin_dcg(Xs),x,end_dcg(Ys),write(Ys),nl,fail.

p:-[setarg_dcg].

/*

?- [setarg_dcg].
compiling(to(mem),myprogs/setarg_dcg.pl,...)
compile_time(134)
?- go.
[the,cat,walks]
[the,cat,sleeps]
[the,dog,walks]
[the,dog,sleeps]
[a,cat,walks]
[a,cat,sleeps]
[a,dog,walks]
[a,dog,sleeps]

*/
