% xref based program checking tools

:-[library(xref2latex)].

% tests on BinProlog, retunrns full latex document
bptest:-xref(wam).

% tests itself
selftest:-xref(bpxref).

% filters out unwanted predicates

non_trivial(FN):-FN \== true/0,FN\==(!)/0.  % FN\==fail/0,

%filter(FN):-non_trivial(FN).
%filter(F/N):-functor(P,F,N), \+ is_builtin(P).
%filter(F/N):-non_trivial(F/N), functor(P,F,N), is_builtin(P).

filter(F/N):-has_info(F/N),non_trivial(F/N).

% adds some predicates that the system cannot see
generator(F/N):-
  db_clause(wam,bu0(P1,_,_,_),_),
  functor(P1,F,N1),
  N is N1-1.
  
go:-
    dynbbgc,
    create_engine(2000,1000,1000,E),
    load_engine(E,xref(wam,0),_), % \input this into ../doc/art.tex
    ask_engine(E,_),
    destroy_engine(E),
    system('mv wam.tex ../doc/preds.tex').

