% CONTINUATIONS ARE FIRST ORDER OBJECTS: some tools based on this

% calls Goal with current continuation available to its inner calls
capture_cont_for(Goal):-
  assumeal(cont_marker(End)),
    Goal,
  end_cont(End).

% passes Closure to be called on accumulated continuation
call_with_cont(Closure):-
  assumed(cont_marker(End)),
  consume_cont(Closure,End).
  
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
   ( NextCont==true-> !,errmes(in_consume_cont,expected_marker(Marker))
   ; arg(1,NextCont,X),Marker==X->
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
