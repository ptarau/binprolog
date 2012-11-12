% xref based program checking tools

% :-write('use -h2000 -t1000 -b0'),nl.

:-[library(xref2txt)].


% tests BinProlog
wtest:-plain_xref(wam).

% tests itself
test:-plain_xref(library(xref)).

%filter(FN):-FN == true/0.
filter(F/N):-functor(P,F,N), \+ is_builtin(P).

go:-
    dynbbgc,
    create_engine(2000,500,1000,E),
    load_engine(E,wtest,_),
    ask_engine(E,_),
    destroy_engine(E).

