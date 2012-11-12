go:-catch(go2,Any,println(uncought=Any)).

go2:-
  catch(
    ( member(X,[a,f(A,A),c]),
      X=f(_,_),
      println(before_throw=X),
      throw(goooot(X)),
      println(after_trow)
    ),
    got(Y),
    println(cought(Y))
  ),
  println(after(caught(Y))).

ignore(_).

go1:-consume_cont(println,boo),println(one),println(two),ignore(boo),println(hi).

/* implementation of catch/throw - with first order continuations 

catch(Goal,Ball,Do,Cont) ::- catch0(Goal,Ball,Do,Cont,Cont) .
catch(_,_,_) :- fail .

catch0(Goal,Ball,Do,Cont) :- 
  get_neck_cut(Choice),
  Goal,
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
   ( nonvar(NextCont),NextCont=true-> !,errmes(in_consume_cont,expected_marker(Marker))
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
*/
