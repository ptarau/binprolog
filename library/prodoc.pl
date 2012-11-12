% xref based program documentation tool

:-[library(xref2latex)].

% tests itself
selftest:-xref(prodoc).

% filters out unwanted predicates

non_trivial(FN):-FN \== true/0,FN\==(!)/0.  % FN\==fail/0,

filter(F/N):-functor(P,F,N), \+ is_builtin(P).

% adds some predicates that the system cannot see
generator(_F/_N):-fail.

