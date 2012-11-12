stat_dict(runtime,0).
stat_dict(global_stack,1).
stat_dict(local_stack,2).
stat_dict(trail,3).
stat_dict(code,4).
stat_dict(strings,5).
stat_dict(symbols,6).
stat_dict(htable,7).
stat_dict(bboard,8).
stat_dict(gctime,9).
stat_dict(realtime,10).

user_error(Mes,Obj):-error_message(basic,Mes,Obj).

errmes(Mes,Obj):-error_message(pretty,Mes,Obj).

fatal_error(Mes,Obj):-error_message(pretty,Mes,Obj,halt(666)).

error_message(Portray,Mes,Obj):-error_message(Portray,Mes,Obj,fail).

error_message(Portray,Mes,Obj,Action):-telling(user),!,
  quiet(Q),Q=<5,portray_error(Portray,Mes,Obj),!,
  Action.
error_message(Portray,Mes,Obj,Action):-
	quiet(Q),Q=<5,
	telling(F),
	tell(user),
	  portray_error(Portray,Mes,Obj),!,
	tell(F),
	Action.

quietmes(At,Mes):-quiet(Q),Q=<At,!,trim_term(Mes,M),ttyprint(M).
quietmes(_,_).

quietmes(Mes):-quietmes(4,Mes).

debugmes(Mes):-quietmes(1,Mes).

portray_error(basic,Mes,Obj):-
  fast_write('>>> '),fast_write(Mes),fast_write(': '),
  trim_term(Obj,T),
  fast_write(T),nl.
portray_error(pretty,Mes,Obj):-
  write('*** '),write(Mes),write(': '),nl,
  trim_term(Obj,T),
  portray_clause(T).

nth_member(X,Xs,N):-member_i(X,Xs,1,N).

member_i(X,[X|_],N,N).
member_i(X,[_|Xs],N1,N3):-
  N2 is N1+1,
  member_i(X,Xs,N2,N3).
	
trim_detail('...').

trim_depth(N):-quiet(Q),trim_depth1(Q,N).

trim_depth1(Q,N):-Q<3,!,N is 6-Q.
trim_depth1(_,3).

trim_term(T,NewT):-trim_depth(N),trim_term(N,T,NewT).

trim_term(N,T,NewT):-trim_detail(D),trim_term(N,D,T,NewT).

trim_term(_,_,T,NewT):-var(T),!,NewT=T.
trim_term(_,_,T,NewT):-atomic(T),!,NewT=T.
trim_term(_,_,T,NewT):-float(T),!,NewT=T.
trim_term(0,NoMore,_,NoMore):-!.
trim_term(N,D,T,NewT):-N1 is N-1,
  T=..[F|Xs],
  trim_args(Xs,Ys,N1,D),
  NewT=..[F|Ys].

trim_args([],[],_,_).
trim_args([X|Xs],[Y|Ys],N,D):-
  trim_term(N,D,X,Y),
  trim_args(Xs,Ys,N,D).

expand_call_body(Body,
  ( 
	do_body(Body, AfterCut, HadCut),
	( HadCut = yes,
		!,
		call_body(AfterCut)
	;   HadCut = no
	)
  )
).


bp_val(bp_virtual,bu0(_,_,_,_),1).
bp_val(bp_virtual,bu1(_,_),1).
bp_val(bp_virtual,bu_ctr(_,_),1).

bp_val(bp_virtual,portray_clause(_),2).
bp_val(bp_virtual,portray(_),2).
bp_val(bp_virtual,term_expansion(_,_),2).
bp_val(bp_virtual,trim_detail(_),2).
bp_val(bp_virtual,trim_depth(_),2).
