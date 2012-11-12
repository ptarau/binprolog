% extra predicates - the old ones moved to deprecated.pl

% ISO catch/throw - with continuations

catch(Goal,Ball,Do,Cont) ::- catch0(Goal,Ball,Do,Cont,Cont) .
catch(_,_,_) :- fail .

catch0(Goal,Ball,Do,Cont) :- 
  get_neck_cut(Choice),
  Goal,
  eq(Goal,_),
  '$to_catch'('$catch_looking_for_throw'([Ball,Do,Choice,Cont])).

'$to_catch'(_).

'$process_catch'(Term,[Ball,Do,Choice,Cont],_):-copy_term(Term,Copy),
   % println(found(Term=Ball,Do)),
   untrail_to(Choice),
   do_or_throw_again(Term,Copy,Ball,Do,Cont).


throw(Term):-consume_cont('$process_catch'(Term,X),'$catch_looking_for_throw'(X)).

throw_with_cont(Term,Cont,_)::-throw(Term,Cont).

do_or_throw_again(_Term,Ball,Ball,Do,Cont):- !,Do,call_cont(Cont).
do_or_throw_again(Term,_Ball,_Copied,_Do,Cont):-throw_with_cont(Term,Cont).

% gathers in conjunction goals from the current continuation
% until Marker is reached when it calls Closure ont it
consume_cont(Closure,Marker):-
  get_cont(Cont),
  consume_cont1(Marker,(_,_,_,Cs),Cont,NewCont), % first _
  call(Closure,Cs),                              % second _
  % sets current continuation to leftover NewCont    
  call_cont(NewCont).                            % third _

% gathers goals in Gs until Marker is hit in continuation Cont
% when leftover LastCont continuation (stripped of Gs) is returned
consume_cont1(Marker,Gs,Cont,LastCont):-
   strip_cont(Cont,Goal,NextCont),
   ( nonvar(NextCont),NextCont=true-> !,
     errmes(end_of_continuation_stack_reached,expected_end_marker_not_found(Marker))
   ; arg(1,NextCont,X),nonvar(X),Marker=X->
     Gs=Goal,arg(2,NextCont,LastCont)
   ; Gs=(Goal,OtherGs),
     consume_cont1(Marker,OtherGs,NextCont,LastCont)
   ).

% this `binarized clause' gets the current continuation
get_cont(Cont,Cont)::-true(Cont).

% setes calls NewCont as continuation to be called next
call_cont(NewCont,_) ::- true(NewCont).

% sets NewCont as continuation to be called next 
% instead of OldCont which is returned in arg 2  
swap_cont(NewCont,OldCont,OldCont) ::- true(NewCont).


/**

  ---- Standard DCG based Assummption Grammars ----
  
  attempt to implement at source level some of the functionality
  of assumption grammars - by overloading the plain DCG transform
  
  - this basically provides AG functionlity in any Prolog having DCGs
    in particular in Jinni Prolog
  
  OO + DCGs seem an extremely powerful mechanism
  
  to be extended to use Jinni's backtrackable assumptions - and possibly other
  Java objects implementing the interface Undoable
  
  NOTE: predicates defined here with arity 3 should be used within clause
        having DCG arrows
        
  porting an AG program to this is quite easy:
  
  replace :- by -->
  
  replace #X by '#:'(X)
  
  use phrase/3 to test out AG components without DCG clause - like in:
 

 ?-phrase(('#<'([a,b,c]),'#+'(t(99)),'#*'(p(88)),'#-'(t(A)),'#-'(p(B)),'#:'(X),'#>'(As)),Xs,Ys).
 
 which returns:
 
 A = 99 Ys = (['*'(p(88))|_408] / _408) - [b,c] Xs = _183 As = [b,c] X = a B = 88 
  
*/

/* sets the dcg token list to be Xs */
'#<'(Xs,_,Db-Xs):-new_assumption_db(Db).

/* unifies current dcg token list with Xs */
'#>'(Xs,Db-Xs,Db-Xs).

/* matches X against current dcg token */
'#:'(X,Db-[X|Xs],Db-Xs).

/* adds 'linear' assumption +(X) to be consumed at most once, by a '#-' operation */
'#+'(X,Db1-Xs,Db2-Xs):-add_assumption('+'(X),Db1,Db2).

/* adds 'intuitionisic' assumption *(X) to be used indefinitely by '#-' operation */
'#*'(X,Db1-Xs,Db2-Xs):-add_assumption('*'(X),Db1,Db2).

/* unifies X with any matching existing or future +(X) linear assumptions */
'#='(X,Db1-Xs,Db2-Xs):-equate_assumption('+'(X),Db1,Db2).

/* consumes +(X) linear assumption or matches *(X) intuitionistic assumption */
'#-'(X,Db1-Xs,Db2-Xs):-consume_assumption('+'(X),Db1,Db2).
'#-'(X,Db-Xs,Db-Xs):-match_assumption('*'(X),Db).

/* matches +(X) or *(X) assumptions without any binding */
'#?'(X,Db-Xs,Db-Xs):-match_assumption('+'(X),Db).
'#?'(X,Db-Xs,Db-Xs):-match_assumption('*'(X),Db).

new_assumption_db(Xs/Xs).

add_assumption(X,Xs/[X|Ys],Xs/Ys).

consume_assumption(X,Xs/Ys,Zs/Ys):-nonvar_select(X,Xs,Zs).

match_assumption(X,Xs/_):-nonvar_member(X0,Xs),copy_term(X0,X).

equate_assumption(X,Xs/Ys,XsZs):- \+(nonvar_member(X,Xs)),!,add_assumption(X,Xs/Ys,XsZs).
equate_assumption(X,Xs/Ys,Xs/Ys):-nonvar_member(X,Xs).

% nonvar_member(X,XXs):-println(entering=nonvar_member(X,XXs)),fail.
nonvar_member(X,XXs):-nonvar(XXs),XXs=[X|_].
nonvar_member(X,YXs):-nonvar(YXs),YXs=[_|Xs],nonvar_member(X,Xs).

nonvar_select(X,XXs,Xs):-nonvar(XXs),XXs=[X|Xs].
nonvar_select(X,YXs,[Y|Ys]):-nonvar(YXs),YXs=[Y|Xs],nonvar_select(X,Xs,Ys).

% Jinni interface for use in Twin Prolog

callj(Query):-
  callj(Query,Answer),
  % println(answer=Answer),
  Answer=the(Query).

callj(Goal,Answer):-callj(Goal,Goal,Answer).

callj(Pattern,Goal,Answer):-
  ( call_jinni_term((Pattern:-Goal),R)->Answer=R
  ; Answer=no
  ).

call_jinni_term(Query,Answer):-
  term_codes(Query,Qs),
  call_jinni_string(Qs,As),
  % write_chars(As),nl,
  term_codes(Answer,As).
    
call_jinni_string(Qs,As):-
  vget(callback,int(F)),
  call_external(F,Qs,As),
  !.
call_jinni_string(_,"no").


% haskell interface

% Haskell to prolog
popTerm(Cs,T):-
  newDict(D),
  popTerm(T,D,Cs,[]).
  
popTerm(T,D) --> [X],makeTerm(X,T,D).

makeTerm(-1,V,D)--> [X],{lookUp(X,V,D)}.
makeTerm(-2,T,D)-->
  [SymLen],makeSym(SymLen,Cs),{atom_codes(S,Cs)},
  [Arity],makeArgs(Arity,Ts,D),
  {T=..[S|Ts]}.
makeTerm(-3,S,_)-->
  [SymLen],makeSym(SymLen,Cs),{number_codes(S,Cs)}.

makeSym(0,[])-->[].
makeSym(K,[C|Cs])-->{K>0,K1 is K-1},[C],makeSym(K1,Cs).

makeArgs(0,[],_)-->[].
makeArgs(I,[T|Ts],D)-->{I>0,I1 is I-1},
  popTerm(T,D),
  makeArgs(I1,Ts,D).

newDict(_).

lookUp(I,V,D):-member(I-W,D),!,W=V.


% Prolog to Haskell

pushTerm(T0,Cs):-
  copy_term(T0,T),
  numbervars(T,0,_),
  pushTerm(T,Cs,[]).
  
pushTerm('$VAR'(I))-->!,[-1,I].
pushTerm(T)-->{number(T)},!,pushNumber(T).
pushTerm(T)-->{atom(T)},!,pushAtom(T),[0].
pushTerm(T)-->{T=..[F|Ts],length(Ts,L)},pushAtom(F),[L],pushTerms(Ts).

pushTerms([])-->[].
pushTerms([T|Ts])-->pushTerm(T),pushTerms(Ts).

pushAtom(T)-->{atom_codes(T,Cs),length(Cs,L)},[-2,L],pushEach(Cs).
pushNumber(T)-->{number_codes(T,Cs),length(Cs,L)},[-3,L],pushEach(Cs).

pushEach([])-->[].
pushEach([C|Cs])-->[C],pushEach(Cs).

% test  
httest(T+T1):-
  popTerm(
  [-2,5,108,105,107,101,115,3,-1,0,
   -2,3,74,111,101,2,-2,6,97,112,112,108,101,115,2,
   -3,4,51,46,49,52,-1,1,-3,2,49,48,-1,0]
   ,
   T
  ),
  write(t=T),nl,
  pushTerm(T,Cs),
  popTerm(Cs,T1).
  
fun(F,N,T):-functor(T,F,N).
succ(X,Y):- +(X,1,Y).


