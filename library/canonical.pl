% file: canonical.pro

% converts a term to parser independent representation

to_canonical(T,CT):-if(atomic_to_canonical(T,X),eq(X,CT),compound_to_canonical(T,CT)).

% converts a term back to normal Prolog representation


from_canonical(CT,T):-if(canonical_to_atomic(CT,X),eq(X,T),from_compound_canonical(CT,T)).

atomic_to_canonical(T,v(T)):-var(T).
atomic_to_canonical(T,s(Cs)):-atom(T),atom_codes(T,Cs).
atomic_to_canonical(T,i(T)):-integer(T).
atomic_to_canonical(T,r(Cs)):-float(T),number_codes(T,Cs).

canonical_to_atomic(v(T),T).
canonical_to_atomic(s(Cs),T):-atom_codes(T,Cs).
canonical_to_atomic(i(T),T).
canonical_to_atomic(r(Cs),T):-number_codes(T,Cs).

compound_to_canonical(T,f(CTs)):-'=..'(T,Ts),map(to_canonical,Ts,CTs).

from_compound_canonical(f(CTs),T):-map(from_canonical,CTs,Ts),'=..'(T,Ts).
