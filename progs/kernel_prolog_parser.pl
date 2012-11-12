:-[library(deprecated)].

% ?-list2clause("f(X):-g(X),h(X).",Term).

list2clause(Chars,Clause):-
   gc_call((
     tokenizer(Chars,Tokens), parser(Tokens,Clause)
   )).

% Horn clause + disj + if-then-else parser

parser(Tokens,Term):-
  #<Tokens,
    top_term(eoc,Term),
  #>[].

top_term(End,HB) :- term(H), body(End,H,HB).

body(End,H,':-'(H,Bs)) :- #iff,conj_seq(Bs),#End.
body(End,H,H) :- #End.

term(V) :- #var(T),lval(a_var,T,V).
term(N) :- #num(N).
term(T) :- #const(F),args(Xs),T=..[F|Xs].
term(L) :- #lbra,term_list(L).
term(S) :- #lpar,spec_term(S).

args([T|Ts]) :- #lpar,term(T),arglist(Ts).
args([]).

arglist([]) :- #rpar.
arglist([T|Ts]) :- #comma,term(T),arglist(Ts).

term_list([]):- #rbra,!.
term_list([T|Ts]) :- term(T),term_list_cont(Ts).

term_list_cont([T|Ts]):- #comma, term(T),term_list_cont(Ts).
term_list_cont(T):- #bar, term(T), #rbra.
term_list_cont([]):- #rbra.

conj_seq(Xs):-seq(comma,',',term,Xs,_).

seq(InCons,OutCons,Inner,Bs,End):- 
  call(Inner,B),
  cons_args(InCons,OutCons,Inner,B,Bs,End).

cons_args(InCons,OutCons,Inner,G,T,End) :- #InCons,!, 
  T=..[OutCons,G,Gs],
  call(Inner,NewG), 
  cons_args(InCons,OutCons,Inner,NewG,Gs,End).   
cons_args(_,_,_,G,G,End) :- dcg_val([End|_]).

spec_term(Xs):- conj_seq(Xs),#rpar.
spec_term(Xs):- seq(disj,';',disj_term,Xs,_),#rpar.
spec_term(Xs):- top_term(rpar,Xs).

disj_term(T):- seq(comma,',',term,Xs,End),disj_term_cont(End,Xs,T).

disj_term_cont(if,Xs,(Xs->Then)):- #if, seq(comma,',',term,Then,_).
disj_term_cont(disj,Xs,Xs).
disj_term_cont(rpar,Xs,Xs).

% tokenizer

tokenizer(Cs,Ws):- #<[32|Cs],words(Ws),!,#>[].

words(Ws):-star(word,Ws),space.

word(W):-space,token(W).

token(lpar):-c("(").
token(rpar):-c(")").
token(lbra):-c("[").
token(rbra):-c("]").
token(bar):-c("|").
token(comma):-c(",").
token(disj):-c(";").
token(if):-c("->").
token(eoc):-c(".").
token(iff):-c(":-").
token(Token):-token(F,Xs),name(N,Xs),Token=..[F,N].

token(num,Xs) :- plus(is_digit,Xs).
token(const,Xs) :- one(is_punct,Xs).
token(F,Xs) :- #X,sym(X,F,Xs).

sym(X,var,[X|Xs]):-is_maj(X),!,star(is_letter,Xs).
sym(X,const,[X|Xs]):-is_min(X),star(is_letter,Xs).

c([]).
c([X|Xs]) :- #X,c(Xs).

space:-star(is_space,_).

is_space(X):- #X, member(X,[32,7,9,10,13]).

is_letter(X):- #X, is_an(X).

is_punct(X):- #X, (is_spec(X);member(X,"!;`""'[]{}*")).

is_digit(X):- #X, is_num(X).

% regexp tools with  AGs + high order

one(F,[X]):- call(F,X).

star(F,[X|Xs]):- call(F,X),!,star(F,Xs).
star(_,[]).

plus(F,[X|Xs]):- call(F,X),star(F,Xs).


% tests

data(
"f(X,s(X))."
).
data(
"f(X,s(X)):-
   a(Y1,13,2,  Y1 ),!,
   g(X,b).").
data(
"a([X|Xs],Ys,[X|Zs]):-a(Xs,Ys,Zs)."
).
data(
"go(Xs,Ys):-a([1,2,3],[4,5|Xs],Ys)."
).

data(
"a(X):- (a,b(X),c), d(X)."
).

data(
"b(X):- ((a(X);b(X));c(X)), d(X),e(X)."
).

data(
"c(X):- ( a(X) -> b(X) ; c(X) )."
).

data(
"d(X):- 
   ( a(X), b(X) -> c,d,e ; c(X)->d ;  f(X),g,((h)) 
   ).
"
).
data("d((H:-B)):-a(H),d((B->t;f)),show(A,B,(A:-B)).").

test:-data(Cs),writeln(Cs),tokenizer(Cs,Ws),write(Ws),nl,fail.

writeln([]):-nl.
writeln([X|Xs]):-put(X),writeln(Xs).

go:-
   data(Cs),
   writeln(Cs),
   (list2clause(Cs,T)->true;T='no!!!'),
   write(T),nl,nl,
   fail.


