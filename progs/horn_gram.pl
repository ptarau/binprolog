list2clause(Chars,Clause):-
   gc_call((
     tokenizer(Chars,Tokens), parser(Tokens,Clause)
   )).

% Horn clause parser

parser(Tokens,Term):-
  dcg_def(Tokens),
    clause(Term),
  dcg_val([]).

clause(':-'(H,B)) :- head(H), body(B).

head(H) :- term(H).

body(Bs) :- #iff,goal(B),goals(B,Bs).
body(true) :- #eoc.

goals(G,','(G,Gs)) :- #comma, goal(NewG), goals(NewG,Gs).
goals(G,G) :- #eoc.

goal(G) :- term(G).

term(V) :- #var(T),lval(a_var,T,V).
term(N) :- #num(N).
term(T) :- #const(F),args(Xs),T=..[F|Xs].

args([T|Ts]) :- #lpar,term(T),arglist(Ts).
args([]).

arglist([]) :- #rpar.
arglist([T|Ts]) :- #comma,term(T),arglist(Ts).


% tokenizer

tokenizer(Cs,Ws):-dcg_def([32|Cs]),words(Ws),!,dcg_val([]).

words(Ws):-star(word,Ws),space.

word(W):-space,token(W).

token(lpar):-c("(").
token(rpar):-c(")").
token(comma):-c(",").
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
   g(X,b).
").


test:-data(Cs),tokenizer(Cs,Ws),write(Ws),nl,fail.

go:-
   data(Cs),
   list2clause(Cs,T),
   write(T),nl,
   fail.


