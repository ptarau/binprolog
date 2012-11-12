/*

%
% Executable English: an Adventure in Abstract Interpretation
%                         

ABSTRACT:
Two classical operations, abstractization and concretization defining
together the usual a Galois connection are used to interpret an
`executable' subset of English.  The result of the abstraction operator
is used to update a world-model through use of linear and
intuitionistic implication.  The world-model's assumptions are
projected back through the concretization operation to allow fairly
complex natural language processing (anaphora, pronouns, relative
clauses), over a realistic common-sense domain.

KEY IDEA:
Reduced precision syntax analyses combined with high precision
world-model based understanding of semantics and pragmatics.

*/

% THIS IS ABOUT HALF - the abstractization step
% looks like a good staring point for a Prolog-based MUD :-)

:-[library(tree)].

% syntax for free, with operator precedence parsing...

:-op(50,fy,green).
:-op(50,fy,red).
:-op(50,fy,yellow).
:-op(50,fy,blue).

:-op(100,fy,a).
:-op(100,fy,an).
:-op(100,fy,the).
:-op(100,fy,his).
:-op(100,fy,her).
:-op(200,xfy,and).
:-op(300,xfx,to).
:-op(300,fx,to).
:-op(300,xfx,in).
:-op(300,fx,in).
:-op(700,xfx,is).
:-op(700,xfx,gives).
:-op(700,xfx,takes).
:-op(700,xfx,drops).
:-op(700,xfx,goes).
:-op(700,xfx,has).
:-op(970,fx,if).
:-op(980,xfy,then).
:-op(990,xfy,else).

check(X):-assumed(X),assumel(X).

text(there is a book in the green room).
text(mary has a flower).
text(joe is in the green room).
text(he has a diamond).
text(she is in the blue room).
text(he takes the book).
text(he goes to the blue room).
text(he gives it to her).
text(mary drops the book). % it means joe did not give her the diamond
text(he takes it).
text(he goes to the red room).
text(she goes to the red room). % same abstract meaning
text(she gives a flower to him).
text(
  if joe has the moon 
  then joe gives the moon to mary 
  else joe gives his diamond to her
).
text(joe goes to the green room).

show:-text(X),write(X),nl,display(X),nl,nl,ppt(X),nl,nl,fail.

% world-state (context) modelling

go:-
   findall_conj(action(T),text(T),Ts),
   Ts.

action(A):-
   write('READING: '),write(A),nl,
   ppt(A),nl,
   get_action(A,Pred),
   (Pred->write('SUCEEDS: '),write(Pred);ppt('FAILING!!!'(Pred))),
   nl,nl,
   write('ABSTRACT ASSUMPIONS:'),nl,listing,
   nl,write('--------------------------------------'),nl,nl.


get_action(A is B,Action):-!,get_action1(action_is(A,B),Action).
get_action(A has B,Action):-!,get_action1(action_has(A,B),Action).
get_action(Term,Action):-get_action1(Term,Action).

get_action1(Term,Action):-
   Term=..[F|Xs],
   abs_list(Xs,Ys),
   Action=..[F|Ys].

get_constraint(Term,Cond):-get_action1(Term,Cond).

action_is(there,O0 in P0):-!,abs1(O0,O),abs1(P0,P),assumel(P has O).
action_is(S0,O):-p_nom(S0,S),assumel(O has S).

action_has(S0,O0):-p_nom(S0,S),p_acc(O0,O),assumel(S has O).

takes(S0,O0):-
  p_nom(S0,S),p_acc(O0,O),
  check(P has S),
  transfer(O,P,S).

drops(S0,O0):-
  p_nom(S0,S),p_acc(O0,O),
  check(P has S),
  transfer(O,S,P).

gives(S0,O0 to C0):-
  p_nom(S0,S),p_acc(O0,O),p_acc(C0,C),
  transfer(O,S,C).

goes(S0,O0):-
  p_nom(S0,S),p_acc(O0,O),  
  transfer(S,_,O).

% this has ordinary Prolog (closed world) semantics
% handling counterfactuals needs abductive reasoning
else(then(if(If0),Then0),Else0):-
  get_constraint(If0,If),get_action(Then0,Then),get_action(Else0,Else),
  (If->Then;Else).

then(if(If0),Then0):-
  get_constraint(If0,If),get_action(Then0,Then),
  (If->Then).

transfer(O,From,To):-
   assumed(From has O),
   assumel(To has O).

abs_list([],[]).
abs_list([X|Xs],[Y|Ys]):-abs(X,Y),abs_list(Xs,Ys).

abs(X to Y,AX to AY):-!,abs1(X,AX),abs1(Y,AY).
abs(X in Y,AX in AY):-!,abs1(X,AX),abs1(Y,AY).
abs(X,AX):-abs1(X,AX).

abs1(X,Z):-compound(X),abs0(X,Y),!,abs(Y,Z).
abs1(X,X).

abs0(a(X),X).
abs0(an(X),X).
abs0(the(X),X).
abs0(in(X),X).
abs0(his(X),X).
abs0(her(X),X).
abs0(to(X),X).

p_nom(P,X):-find_if(p_nom0(P,Xs),member(X,Xs),P=X).

p_nom0(he,[joe]).
p_nom0(she,[mary]).
p_nom0(it,[book,flower]).

p_acc(P,X):-find_if(p_acc0(P,Xs),member(X,Xs),X=P).

p_acc0(him,Xs):-p_nom0(he,Xs).
p_acc0(her,Xs):-p_nom0(she,Xs).
p_acc0(it,Xs):-p_nom0(it,Xs).

find_if(If,Then,Else):-
  findall(If-Then,If,IfThens),
  ( IfThens=[]->Else
  ; member(If-Then,IfThens),
    Then
  ).


