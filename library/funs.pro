% translator for functional syntax with guards
/*
syntax:
  Head:-Guards,!,Body.
  where Head is f(X1...Xn)
        Body is a function expression, to be expanded
        Guards is a conjunction of Prolog predicates
  Any function defined with this syntax is directly usable in
  is/2.
        
*/

:-op((:=),xfx,1195). % to be used instead of :- when integrated

funs2prolog(InFile,OutFile):-
  tell(OutFile),
    ( term_of(InFile,Fun),
      fun2cls(Fun,Cls),
      portray_clause(Cls),
      fail
    ; nl
    ),
  told.
  
fun2cls(F,(D:-Bs)):- 
  fun2cls(F,D,Gs,Fs,[]),
  append(Gs,Fs,Rs),
  l2conj(Rs,Bs).
  
fun2cls(':-'(H,GB),D,Gs)-->
  {precut(GB,([!]),Gs,B)},
  {H=..[F|Xs]},
  {append(Xs,[Y],Ys)},
  {D=..[F|Ys]},
  fun2term(B,Y).

fun2term(A,T)-->{var(A)},!,{T=A}.  
fun2term(A,T)-->{number(A)},!,{T=A}.
fun2term(A,Y)-->{A=..[F|Xs]},funs2terms(Xs,Y,Ts),{T=..[F|Ts]},[T].

funs2terms([],Y,[Y])-->[].
funs2terms([A|As],Y,[T|Ts])-->
  fun2term(A,T),
  funs2terms(As,Y,Ts).

precut((!,Bs),End,End,Bs):-!.
precut((A,Bs),End,[A|Cs],Ds):-!,precut(Bs,End,Cs,Ds).
precut(A,End,End,A).

l2conj([],true):-!.
l2conj([X],X):-!.
l2conj([X|Xs],(X,Cs)):-l2conj(Xs,Cs).

% example of translation
ftest:-
  F=(
    d(X,Y) :- X<5,Y>0,!,
      f(g(3.14,h(b,X,10)),c(X),Y)
  ),
  fun2cls(F,R),
  portray_clause(R).

/*
?- ftest.
d(_A,_B,_C) :-
        _A < 5,
        _B > 0,  !,
        b(_D),
        h(_D,_A,10,_E),
        g(3.14,_E,_F),
        c(_A,_G),
        f(_F,_G,_B,_C).
yes

?- fun2cls((h(X):-g1,g2,!,b(c(d(X)))),R).
X=_x2298,
R=h(_x2298,_x3124) :- 
  g1,g2,
  !,
  d(_x2298,_x3224),
  c(_x3224,_x3189),
  b(_x3189,_x3124)

?- fun2cls((d(X,Y):-sqrt(pow(X,2)+pow(Y,2))),R).
X=_x2298,
Y=_x2358,
R=d(_x2298,_x2358,_x3373) :- 
  !
  ,pow(_x2298,2,_x3475),
  pow(_x2358,2,_x3551),
  +(_x3475,_x3551,_x3438),
  sqrt(_x3438,_x3373)

yes
*/  

% simple translator from Haskell - TODO

x-->[X],{write(X),nl,fail}.
x-->[].

hdef((H:-G,!,R))-->left(H,G),[=],!,right(R).

left(H,G)-->head(H),maybe_guard(G).

maybe_guard(Gs)-->['|'],!,guard(Gs).
maybe_guard([])-->[].

head([T|Ts])-->term(T),terms(Ts).

terms([T|Ts])-->term(T),terms(Ts).
terms([])-->[].

guard([T|Ts])-->term(T),terms(Ts).

term([T|Ts])-->['('],!,term(T),terms(Ts),[')'].
term(T)-->[T].

right([T|Ts])-->term(T),terms(Ts).

htest(R):-
  hdefs(Ts),
  hdef(R,Ts,[]).
  
hdefs(Ts):-member(S,[
    "f n | n<2 = 0",
    "f n = f (n-1) + f(n-2)"
  ]),
  to_words(S,Ts).
  
    