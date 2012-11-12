% uses Assumption Grammars
% implements a curried flat syntax for LP, as in:

% append nil Xs Ys.
% append AXs Ys AZs :- AXs = cons A Xs, AZs = cons A Zs, append Xs Ys Zs.
% append AXs Ys AZs :- AXs = A|AZs, AZs = A|Zs, append Xs Ys Zs.
% append AXs Ys AZs :- AXs = A|AZs, AZs = A|Zs, append Xs Ys Zs.


go:-
 % f 12 Xs :- Xs =: cons X Ys, f 11 Ys, !, r 3.14 X.
   Ts=[atom(f),integer(box(12,[46,49,50])),var(_2641,'Xs'),atom((:-)),
     var(_2641,'Xs'),atom(=),atom(.),var(_2915,'X'),var(_2990,'Ys'),(','),
     atom(f),integer(box(11,[46,49,49])),var(_2990,'Ys'),(','),
     atom(!),(','),
     atom(r),atom(3.140000),var(_2915,'X')],
   Vs=[var('Xs',_2641,s(1)),var('X',_2915,s(1)),var('Ys',_2990,s(1))],
   parse_flat(Ts,Vs,C),
 write(C),nl,
 fail.

test:-write('> '),read_flat(C,_),write(C),nl,fail.

test(Cs):-read_tokens_from_chars(Cs,Ts,Vs),write(Ts-Vs),nl.

read_block(XXs):-get0(X),read_block1(X,XXs).

read_block1(-1,"end_of_file"):-!.
read_block1(46,Xs):-get_code(X),!,read_block2(X,Xs).
read_block1(X,[X|Xs]):-get_code(Y),read_block1(Y,Xs).

read_block2(10,[]):-!.
read_block2(32,[]):-!.
read_block2(9,[]):-!.
read_block2(13,[]):-!.
read_block2(X,[X|Xs]):-read_block1(X,Xs).

read_flat(Clause,Vs):-
%   read_tokens(Ts,Vs),!,
  read_block(Cs),read_tokens_from_chars(Cs,Ts,Vs),!,
   parse_flat(Ts,Vs,Clause).

parse_flat(Ts,_Vs,(H:-Bs)):-
   split_rest(Ts,[Xs|Xss]),
   parse_head(Xs,H),
   parse_body_or_true(Xss,Bs).

parse_head(Xs,H):-parse_atom(Xs,H).

parse_body_or_true([],true).
parse_body_or_true([sep((:-)),X|Xs],Bs):-parse_body(Xs,X,Bs).

parse_body([],X,B):-!,parse_atom(X,B).
parse_body([sep(','),Y|Xs],X,BBs):-
  parse_body_atom(X,Bs,BBs),
  parse_body(Xs,Y,Bs).

parse_body_atom(X,Bs,BBs):-
  parse_atom(X,B),
  add_body_atom(B,Bs,BBs).

add_body_atom(true,Bs,Bs):-!.
add_body_atom(B,Bs,(B,Bs)).

parse_atom([var(V,_),atom(Eq),atom(X)|Xs],true):-member(Eq,[=,:=,=:]),!,
  parse_args(Xs,As),
  T=..[X|As],V=T.
parse_atom([atom(X)|Xs],A):-
  parse_args(Xs,As),
  A=..[X|As].

parse_args([],[]).
parse_args([T|Ts],[X|Xs]):-parse_arg(T,X),parse_args(Ts,Xs).

parse_arg(var(X,_),X):-!.
parse_arg(atom(X),X):-!.
parse_arg(integer(box(X,_)),X):-!.
%parse_arg(X,X).

split_rest(Xs,[Ys|Yss]):-
   dcg_val([]),dcg_def(Ys),
   split_flat(Xs,Yss),
   dcg_val([]).

split_flat([],[]):-!.
split_flat([X|Xs],[sep(S)|Xss]):-sep(X,S),!,split_rest(Xs,Xss).
split_flat([X|Xs],Xss):-dcg_connect(X),split_flat(Xs,Xss).

sep((','),(',')).
sep(atom((:-)),(:-)).


