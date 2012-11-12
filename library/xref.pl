% BinProlog basic cross-reference utility
% Author: Paul Tarau, 1996
% Last revision:Mon Apr  1 09:14:55 AST 1996
/*
       
:-module(xref,
    [xref/1,init_xref/1, ascii_name/2, db_proc/2,
    undefined/2, unused/2, get_callers/3, get_callees/3]
  ).
*/

about_init_xref_1(
  "Loads a file into a database used to track refrences"
).
  
init_xref(File):-
  db_consult(File),
  (apply_generator(F/N),functor(P,F,N),db_assert(File,P),
  quietmes(2,'GENERATING'(P)),
  fail
  ;true),
  db_sort(File).

db_sort(DB):-val('$clauses',DB,XsYs),
  unify_to(XsYs,Cs-Qs),
  copy_term(Cs,Ds),
  sort(Ds,Es),
  bb_set('$clauses',DB,Es-Qs).
  
db_consult(File):-
  consult(File,File).

db_proc(DB,F/N):-
  ( nonvar(F),nonvar(N)->functor(H,F,N),tval(DB,H,_)
  ; cmembq('$clauses',DB,F/N)
  ),
  apply_filter(F/N).

db_filtered_clause(DB,F/N,H,B):-
  db_proc(DB,F/N),
  functor(H,F,N),
  db_clause(DB,H,B).

apply_generator(FN):-is_compiled(generator(_)),generator(FN).

apply_filter(FN):-is_compiled(filter(_)),!,filter(FN).
apply_filter(_).

calles_to(DB,FN,GM):-
  answer_of(db_calls(DB,FN,GM),GM).

called_from(DB,GM,FN):-
  answer_of(db_calls(DB,FN,GM),FN).

db_calls(DB,F/N,Callee/M):-
  db_filtered_clause(DB,F/N,_,B),
  in_body(B,Goal),
  functor(Goal,Callee,M).

in_body(V,_):-var(V),!,fail.
in_body((A,B),X):-!,(in_body(A,X);in_body(B,X)).
in_body((A;B),X):-!,(in_body(A,X);in_body(B,X)).
in_body((A->B),X):-!,(in_body(A,X);in_body(B,X)).
in_body(A,X):-meta_arg(A,_,B,_),!,in_body(B,X).
in_body(A,A):-functor(A,F,N),apply_filter(F/N).

meta_arg(findall(X,G,Xs),findall(X,NewG,Xs),G,NewG).
meta_arg(bagof(X,G,Xs),bagof(X,NewG,Xs),G,NewG).
meta_arg(setof(X,G,Xs),setof(X,NewG,Xs),G,NewG).
meta_arg(not(G),not(NewG),G,NewG).
meta_arg(\+(G), \+(NewG),G,NewG).
meta_arg(call(G),call_body(NewG),G,NewG).
meta_arg(answer_of(G,X),answer_of(NewG,X),G,NewG).


% enumerates predicates with no visible static definitions
undefined(DB,F/N):-
  answer_of((undefined0(DB,X),functor(X,F,N)),F/N).

undefined0(DB,H):-
  db_filtered_clause(DB,_,_,B),
  in_body(B,H),
  \+ tval(DB,H,_).

% enumerates predicates with no visible static uses
unused(DB,F/N):-
  mark_used(DB),
  unused0(DB,F/N).

mark_used(DB):-
  db_filtered_clause(DB,_,_,B),
    in_body(B,H),
    bb_let(H,DB,'$used'),
  fail
; true.

unused0(DB,F/N):-
  db_proc(DB,F/N),
  functor(H,F,N),
  \+ val(H,DB,'$used').
  
get_callees(File,FN,Cs):-
  findall(Callee,calles_to(File,FN,Callee),Cs).

get_callers(File,FN,Cs):-
  findall(Caller,called_from(File,FN,Caller),Cs).

ascii_name(F/N,Name):-
  [U]="_",
  make_cmd0([F,[U],N],Cmd),
  findall(Y,repl_member(Cmd,U,Y),Xs),
  name(Name,Xs).
 

repl_member(Cmd,U,Y):-member(X,Cmd),repl(U,X,Y).
  
repl(U,X,Y):-is_an(X),X=\=U,!,Y=X.
repl(_,X,Y):-([Y]="V";name(X,Cs),member(Y,Cs);[Y]="W").

% :-end_module(xref).

