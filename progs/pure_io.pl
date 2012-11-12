:-[library(string_io)].

go:-T0=f(X,X,g(a,[X,Y,Y],13)),t2s(T0,S),write(S),nl,
    s2t(S,T),write(T0+T),nl,
    name(N,S),write(N),nl.
