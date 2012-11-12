go:-go('BMARK_differen:').

go(Mes):-differen(Mes,600).

/* Common functions...       */

print_times(Mes,T1,T2,T3,L0) :- L is 1000*L0,
        TT1 is T2 - T1,
        TT2 is T3 - T2,
        TT is TT1 - TT2,
        nl, write(Mes='Net Time'(TT)), nl,
        Lips is L // TT,
        Klips is Lips // 1000,
        write('KLips are: '), write(Klips), nl.

compens_loop(0).
compens_loop(X) :- Y is X - 1, compens_loop(Y).

/* --------------------------------------------------*/
/*       differen (times10,divide10,log10,ops8)      */
/*       These 4 examples are from Warren's thesis   */
/*       differen(150) will do.                      */

differen(Mes,N) :- statistics(runtime,[X|_]),
          differenloop(N),
          statistics(runtime,[Now|_]),
          compens_loop(N),
          statistics(runtime,[M|_]),
          Li is 71 * N,
          print_times(Mes,X,Now,M,Li).

differenloop(0).
differenloop(X) :- \+ \+(differen_top), Y is X - 1, differenloop(Y).

ds:-statistics,differen_top,statistics.

differen_top:-
        times10(I1),
        d(I1,x,_D1),
        divide10(I2),
        d(I2,x,_D2),
        log10(I3),
        d(I3,x,_D3),
        ops8(I4),
        d(I4,x,_D4).

d(F,X,DF):-df(F,X,DF),!.
d(X,X,1).
d(_C,_X,0).

df(U+V,X,DU+DV) :- d(U,X,DU), d(V,X,DV).
df(U-V,X,DU-DV) :- d(U,X,DU), d(V,X,DV).
df(U*V,X,DU*V+U*DV) :- d(U,X,DU), d(V,X,DV).
df(U/V,X,(DU*V-U*DV)/(^(V,2))) :- d(U,X,DU), d(V,X,DV).
df(^(U,N),X,DU*N*(^(U,N1))) :- N1 is N - 1, d(U,X,DU).
df(-U,X,-DU) :- d(U,X,DU).
df(exp(U),X,exp(U)*DU) :- d(U,X,DU).
df(log(U),X,DU/U) :- d(U,X,DU).


times10( ((((((((x*x)*x)*x)*x)*x)*x)*x)*x)*x ).
divide10( ((((((((x/x)/x)/x)/x)/x)/x)/x)/x)/x ).
log10( log(log(log(log(log(log(log(log(log(log(x)))))))))) ).
ops8( (x+1)*((^(x,2)+2)*(^(x,3)+3)) ).
