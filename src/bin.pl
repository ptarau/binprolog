
% BINARY CLAUSE COMPILER

% EXPORTED 

compile_binary(Mode,BinClause):-cc_bincomp(Mode,BinClause).

compile_builtins(Mode):-cc_builtins(Mode).

% INTERNALS

cc_must_have_builtins(wam).
cc_must_have_builtins(c):-c_chunk_file('wam.h').

cc_builtins(Mode):-
  cc_must_have_builtins(Mode),
  bin_builtin(B),
  cc_bincomp(Mode,B),
  fail.
cc_builtins(_).

cc_bincomp(bin,C):-!,portray_clause(C),fail.
cc_bincomp(asm,C):-write('BINARY:'),nl,portray_clause(C),nl,fail.
cc_bincomp(Mode,C):-
	newpred(Mode,C), 
	cc_compile_clause(C,CodeChunks),
	gg_emit(Mode,CodeChunks),
	!.
cc_bincomp(Mode,C):-
  get_lineno(L),
	errmes(failing_to_compile_clause(mode(Mode),line(L)),C).

bin_builtin(('cut_to'(X,Cont):-true(Cont))):-cutp(X).
bin_builtin(C):-  
	bu0(B,_,Where,_),
	make_builtin(Where,B,C).

make_builtin(in_head,B,(B:-true(Cont))):-
	functor(B,_,N),arg(N,B,Cont).
make_builtin(in_body,B,(B:-B)).

cc_compile_clause(C,[Head,PCode,Tail]):-
  cc_clause(C,PCode,Head,Tail).

firstarg(H,FN):-arg(1,H,A),classif_first(A,FN).

classif_first(A,FN):-atomic(A),!,FN=A/0.
classif_first(A,FN):-var(A),!,FN='_'/0.
classif_first(T,F/N):-functor(T,F,N).

cc_clause(C,OCode,
	[ii(clause,?,F1,N1),ii(firstarg,?,GM,LastN)],
	[ii(execute,?,F2,N2)]):-
	add_true(C,(H:-B)),
	firstarg(H,GM),
	cc_h_b(H,F1/N1,B,F2/N2,'get','put',RCode),
	max(N1,N2,MaxN),FirstN is MaxN+1,
	vars(RCode,OCode),
	functor(Dict,dict,MaxN),
	fill_info(OCode,Dict),
	collapse_args(Dict,1,MaxN), 
	allocate_regs(OCode,FirstN/FirstN-[],FirstN/LastN-_).

cc_h_b(H,F1/N1,B,F2/N2,HM,BM,RCode):-
	cc_h(H,F1/N1,HM/LV-RCode,HM/V2-Rest),
	cc_b(B,F2/N2,BM/V2-Rest,BM/LV-[]),
        !.

begin_chunk-->
	emit(ii(c_chunk,_,len(L),V)), % start C-ified stream
	hidden_var(L+V).

end_chunk-->
	emit(ii(c_chunk,_,len(L),V)), % end C-ified stream
	hidden_var(L+V).

emit(E,ModeLength-[E|Es],ModeLength-Es).

get_mode(Mode,S,S):-S=Mode/_Length-_Es.

set_mode(Mode,_/Length-Es,Mode/Length-Es).

hidden_var(Length,S,S):-S=_Mode/Length-_Es.

has_compress:-
  is_prolog(binprolog),
  ( vread(jump_compress,1)
  ; vread(struct_compress,1)
  ).

begin_chunk_comp-->{has_compress},!,begin_chunk.
begin_chunk_comp-->[].

begin_chunk_nocomp-->{has_compress},!.
begin_chunk_nocomp-->begin_chunk.

cc_h(B,FN)-->
	begin_chunk_nocomp,  % $$$ HERE WHEN JUMP_COMPRESS==0 
 	cc_h0(B,FN).  %        _and_ STRUCT_COMPRESS==0

cc_b(Cont,FN)-->
	cc_b0(Cont,FN)
	,end_chunk		             % $$$
	.

cc_h0(B,F/N)-->{ bu0(B,No,in_head,_) },!,
	{functor(B,F,N),arg(N,B,Cont)},
	emit(ii(builtin,?,No,Cont))
	,begin_chunk_comp       % $$$ HERE WHEN JUMP_COMPRESS==1
	.                       % _or_ STRUCT_COMPRESS==1

cc_h0(T,F/N)-->
	{functor(T,F,N),N>0},!,{functor(CT,F,N)},
	emit_head_top_term(N,T,CT).
cc_h0(T,_)-->{errmes(unexpected_head_atom,T)}.

emit_head_top_term(N,T,CT) --> 
	emit_top_args(1,1,T,CT),
	cc_top_arg(1,1,CT,T),
	begin_chunk_comp,	    % $$$ HERE OTHERWISE
	emit_top_args(2,N,T,CT),
	cc_top_arg(2,N,CT,T).

cc_b0(Cont,FN)-->{var(Cont)},!,cc_b0(true(Cont),FN).
cc_b0(true,true/0)-->!.
cc_b0('cut_to'(_cutp,Cont),FN)-->!,
	emit(ii('put',_,cutarg(1),_cutp)), % 1 is unrelated to temp regs
	cc_b0(Cont,FN).
cc_b0(=(A,B,Cont),FN)-->!,
	cc_eq(A,B),
	cc_b0(Cont,FN).
cc_b0(B,FN)-->{ bu0(B,No,in_body,_) },!,
	cc_builtin(No,B,FN).
cc_b0(T,F/N)-->
	{functor(T,F,N),N>0},!,{functor(CT,F,N)},
	emit_body_top_term(N,T,CT).
cc_b0(T,_)-->{errmes(unexpected_body_atom,T)}.

emit_body_top_term(N,T,CT) -->
	  cc_top_arg(1,N,CT,T),!,
	  emit_top_args(1,N,T,CT).

cc_eq(A,B)-->{var(B),nonvar(A)},!,cc_eq1(B,A).
cc_eq(A,B)-->cc_eq1(A,B).

cc_eq1(A,B)-->
	cc_top_t(V1=A),
	emit(ii('put',_,temp(0),V1)),
	emit(ii('get',_,temp(0),V2)),
	set_mode(get),
	cc_top_t(V2=B),
        set_mode(put).

out_reg(0,_,0).
out_reg(1,Res,Res).

cc_builtin(arith(No,NbOutArgs),OpArgsCont,FN)-->!,
	{ functor(OpArgsCont,Op,N1),arg(N1,OpArgsCont,Cont),
		N is N1-1, arg(N,OpArgsCont,X), out_reg(NbOutArgs,X,Res),
		I is N-NbOutArgs,functor(NewOpArgs,Op,I) % NbOutArgs = 0,1
	},
	handle_constant_res(NbOutArgs,VarRes=Res),
	emit_top_bargs(1,I,OpArgsCont,NewOpArgs),
	emit(ii(arith,_Type,No,VarRes)),
	cc_b0(Cont,FN).
cc_builtin(No,BodyAndCont,FN)-->
	{ functor(BodyAndCont,_,N),N1 is N-1,
		arg(N,BodyAndCont,Cont),
		arg(1,BodyAndCont,Arg)
	},
	cc_b_args(N1,Arg),
% NOGOOD!!! {temparg(0,_Temp)},
	emit(ii(inline,_,No,_Temp)), % void var!!! implicitly temp(0)
	cc_b0(Cont,FN).

handle_constant_res(0,_)-->!.
handle_constant_res(1,X=C)-->{var(C)},!,{X=C}.
handle_constant_res(1,X=C)-->{atomic(C)},!,
	emit(ii('put',_,temp(0),C)),
	emit(ii('get',_,temp(0),X)).
handle_constant_res(1,X=C)-->!,
	cc_top_t(X=C).

% handle_constant_res(1,_=C)-->{errmes(must_be_atomic_or_var,C)}.

classif_load(X,A,_)-->{var(A)},!,{X=A}.
classif_load(X,A,constant)-->{atomic(A)},!,{X=A}.
classif_load(X,A,_)-->cc_top_t(X=A).
	
cc_b_args(0,_)-->[].
cc_b_args(1,Arg)-->
	cc_top_t(V=Arg), % usually in write mode
	emit(ii('put',_,temp(0),V)).

emit_top_bargs(I,N,_,_) --> {I>N},!.
emit_top_bargs(I,N,T,CT) --> {I=<N,I1 is I+1},
	{arg(I,T,A),arg(I,CT,X)},
	classif_load(X,A,Type),
	emit(ii(load,Type,I,X)),
	emit_top_bargs(I1,N,T,CT).

emit_top_bargs(I,N,T,CT) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X)},
	!,
	cc_top_t(X=A),
	{I1 is I+1},
	emit_top_bargs(I1,N,T,CT).

emit_top_args(I,N,_,_) --> {I>N},!.
emit_top_args(I,N,T,CT) --> {I=<N},
  {arg(I,T,A),arg(I,CT,X),classif_arg(X,A,Type)}, % must be int. if const!
  !,
  get_mode(Op),emit(ii(Op,Type,arg(I),X)),
  {I1 is I+1},
  emit_top_args(I1,N,T,CT).

% calls cc_top_t
cc_top_arg(I,N,_,_) -->  {I>N},!.
cc_top_arg(I,N,CT,T) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X)},
	{I1 is I+1},!,
	cc_top_t(X=A),
	cc_top_arg(I1,N,CT,T).

% calls cc_top_arg
cc_top_t(X=T) --> {var(T)},!,{X=T}.
cc_top_t(X=T) --> {atomic(T)},!,{X=T}.
cc_top_t(X=T) --> {functor(T,F,N)},{N>0},!,
	{functor(CT,F,N)},
	emit_top_structure(X,F,N,T,CT),
	cc_args(N,CT,T). % ## sould NOT be cc_top_arg !!!

emit_top_structure(X,F,N,T,CT) --> 
	get_mode(Op),
	emit(ii(Op,structure,F/N,X)),
	emit_args(N,T,CT).

% calls cc_t
cc_arg(I,N,_,_) -->  {I>N},!.
cc_arg(I,N,CT,T) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X)},
	{I1 is I+1},!,
	cc_t(X=A),
	cc_arg(I1,N,CT,T).

% calls cc_arg
cc_t(X=T) --> {var(T)},!,{X=T}.
cc_t(X=T) --> {atomic(T)},!,{X=T}.
cc_t(X=T) --> {functor(T,F,N)},{N>0},!,
	{functor(CT,F,N)},
	emit_structure(X,F,N,T,CT),
	cc_args(N,CT,T).

cc_args(N,CT,T)-->{N>1},!,
	{N1 is N-1},
	{arg(N,T,A),arg(N,CT,X)},
        cc_t(X=A),
	cc_arg(1,N1,CT,T).
cc_args(N,CT,T)-->
	cc_arg(1,N,CT,T).

emit_structure(X,F,N,T,CT) --> 
	get_mode(Op),
	{deep_structure_op(Op,NewOp)},
	emit(ii(NewOp,structure,F/N,X)), % ## 'push' if Op='put'
	emit_args(N,T,CT).

emit_args(N,T,CT) --> emit_args0(1,N,T,CT).

emit_args0(I,N,_,_) --> {I>N},!.
emit_args0(I,N,T,CT) --> {I=<N},
	{arg(I,T,A),arg(I,CT,X),classif_arg(X,A,Type)},
	!,
	get_mode(Op),emit(ii(UnifyOp,Type,Op,X)),
	{unify_op(Op,A,UnifyOp),I1 is I+1},
	emit_args0(I1,N,T,CT).

unify_op('get',_,unify).
unify_op('put',A,VarOp):-deep_var_op(A,VarOp).

deep_var_op(A,push):-compound(A),!.
deep_var_op(_,write).

deep_structure_op('get','get').
deep_structure_op('put','push').

classif_arg(X,A,_):-var(A),!,X=A.
classif_arg(X,A,constant):-atomic(A),!,X=A.
classif_arg(_,_,_).


% VARIABLE OCCURRENCE CLASSIFIER

% vars(T,R) :-
% each (selected) variable V of T gives in R a term
%
%   var(NewVar,OccNo/MaxOccurrences)
%
% and T is subject to (an ugly) side effect as selected
% variables get unified to '$OCC'(NewVar,MaxOccurrences=MaxOccurrences)

vars(T,R):-
	find_occurrences(T,R,Vars,[]),
	count_occurrences(Vars).

find_occurrences([],[])-->[].
find_occurrences([ii(Op,Type,Val,Var)|L],[ii(Op,Type,Val,Occ)|R])-->
	occurrence(Var,Occ),
	find_occurrences(L,R).

occurrence(A,A)-->{atomic(A)},!.
occurrence(V,var(NewV,1/Max))-->{var(V)},!,
	newvar(X=Max),
	{V='$OCC'(NewV,X=Max)}.
occurrence('$OCC'(OldV,X=Max),var(OldV,K/Max))-->!,
	oldvar(X=Max,K).
occurrence(Var,Occ)-->{errmes(bad_occurrence,at(var=Var,occ=Occ))}.

inc(V,K,K):-var(V),!,V=s(_).
inc(s(V),K1,K3):-K2 is K1+1,inc(V,K2,K3).

oldvar(X=_,K,Xs,Xs):-inc(X,2,K).

newvar(E,[E|Es],Es).

count_occurrences([]):-!.
count_occurrences([X=Max|Vs]):-inc(X,1,Max),count_occurrences(Vs).


% ARGUMENT REGISTER OPTIMIZER

% fills Dict and and marks still free slots in variables
% with information on liftime of arguments

fill_info(Is,Dict):-fill_all(Is,Dict,0,_).

tpoint(T2,T1,T2):-T2 is T1+1. % gets a time-point

fill_all([],_)-->[].
fill_all([I|Is],Dict)-->
	{fill_var_type(I)},
	fill_one(I,Dict),
	fill_all(Is,Dict).

% fills in lifetime information using occurrence numbers

fill_var_type(ii(Op,Type,_,var(_,Occ))):-var(Type),!,
  get_var_type(Occ,Op,Type).
fill_var_type(_).

get_var_type(1/1,unify,void):-!.
get_var_type(1/1,write,void):-!.
get_var_type(1/_,_,variable):-!.
get_var_type(K/Max,_,value):-K=<Max,!.

fill_one(ii(Op,constant,arg(An),_),Dict)-->!,tpoint(T),
	{mark_arg(Op,T,An,var(_-T/T,1/1),Dict)}.
fill_one(ii(_,constant,_,_),_)-->!,tpoint(_).
fill_one(ii(Op,_,arg(An),Xn),Dict)-->!,tpoint(T),
	{mark_arg(Op,T,An,Xn,Dict)},
	{mark_var(T,Xn)}.
fill_one(ii(_,_,_,var(Id,Occ)),_)-->!,tpoint(T),
	{mark_var(T,var(Id,Occ))}.

% marks the argument An of Dict with lifetime information
mark_arg('get',From,An,Xn,Dict):-arg(An,Dict,Xn*_-From/_).
mark_arg('put',To  ,An,Xn,Dict):-arg(An,Dict,_*Xn-_/To).

% marks a variable with lifetime information
mark_var(T,var(_-T/T,1/1)):-!.
mark_var(T,var(_-T/_,1/Max)):-1<Max,!.
mark_var(T,var(_-_/T,Max/Max)):-1<Max,!.
mark_var(_,var(_-_/_,_/_)).

% collapses arguments and variables, if possible
collapse_args(_,I,Max):-I>Max,!.
collapse_args(Dict,I,Max):-I=<Max,
	arg(I,Dict,X),
	collapse_them(I,X),
	I1 is I+1,
	collapse_args(Dict,I1,Max).

default_life_time(V1/V2):-set_to(0,V1),set_to(99999,V2).

set_to(Val,Val):-!.
set_to(_,_).


% checks if argument I living ALife can be collapsed with
%   input head variable H living HLife and 
%   output body variable B living BLife

collapse_them(I,var(H-HLife,_)*var(B-BLife,_)-ALife):-
	default_life_time(HLife),
        default_life_time(BLife),
        default_life_time(ALife),
	check_lifetimes(H-HLife,B-BLife,I-ALife).

check_lifetimes(I-HLife,I-BLife,I-ALife):-
	check_var_arg(HLife,ALife),
	check_var_var(HLife,BLife),
	check_var_arg(BLife,ALife),!.
check_lifetimes(I-HLife,_,I-ALife):-
	check_var_arg(HLife,ALife),!.
check_lifetimes(_,I-BLife,I-ALife):-
	check_var_arg(BLife,ALife),!.
check_lifetimes(_,_,_).

check_var_var(_/H2,B1/_):-H2=<B1,!.
check_var_var(H1/_,_/B2):-B2=<H1.

check_var_arg(X1/X2,A1/A2):-
	A1=<X1,X2=<A2.


% TEMPORARY VARIABLE ALOCATOR

allocate_regs([])-->!.
allocate_regs([I|Is])-->
	allocate1(I),
	allocate_regs(Is).

allocate1(ii(_,_,_,var(Reg-_,KMax)))-->allocate_reg(Reg,KMax),!.
allocate1(_)-->[].

allocate_reg(Reg,1/1)-->{var(Reg)},!,
	get_reg(Reg),
	free_reg(Reg).
allocate_reg(Reg,1/Max)-->{var(Reg),1<Max},!,
	get_reg(Reg).
allocate_reg(Reg,Max/Max)-->{1<Max},!,
	free_reg(Reg).

free_reg(Reg,Min/N-Regs,Min/N-[Reg|Regs]):-Reg>=Min,!.
free_reg(_,Rs,Rs).

get_reg(Reg,Min/N-[Reg|Regs],Min/N-Regs):-!.
get_reg(N,Min/N-Regs,Min/N1-Regs):-N1 is N+1.

