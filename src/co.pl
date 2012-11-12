% BinProlog x.xx Copyright (C) 1992 Paul Tarau. All rights reserved.
% COMPILER: dcgs --> prolog --> binary progs --> code

%----------PREPROCESSOR TO BINARY LOGIC PROGRAMS-------------

% Predicate-level pattern directed replacement of some simple
% iterators (think about good chess and go players :-))

repl_pred(FN,Cs,NewCs):-repl_append(FN,Cs,NewCs),!,repmes(FN).
repl_pred(FN,Cs,NewCs):-repl_member(FN,Cs,NewCs),!,repmes(FN).
repl_pred(_,Cs,Cs).

repmes(FN):-debugmes('  replacing_with_builtin'(FN)).

repl_append(App/3,Cs,NewCs):-
  Cs=[H1,(H2:-B2)],
  nonvar(B2),functor(B2,App,3),
  nonvar(H1),functor(H1,App,3),
  arg(1,H1,A1),nonvar(A1),functor(A1,Nil,0),
  arg(1,H2,A2),nonvar(A2),functor(A2,Cons,2),
  ## proto_append(Ts,NewTs),
  Dict=[App/3-_, Nil/0-_, Cons/2-_],
  repl_pred_pattern(Dict,Ts,NewTs,Cs,NewCs).

proto_append(In,Out):-
  Dict=[ lapp/3-_, []/0-_, ('.')/2-_ ],
  l2hl(
    [  lapp([],Ys,Ys),
      (lapp([A|Xs],Ys,[A|Zs]):-
        lapp(Xs,Ys,Zs)
      )
    ],
    In,Dict),
  l2hl(
    [  lapp([],Ys,Ys),
      (lapp([A|Xs],Ys,[A|Zs]):-
           det_append0(Xs,NewXs,EndXs-Zs),
           lapp(EndXs,Ys,NewXs)
      )
    ],
    Out,Dict).

repl_member(Memb/2,Cs0,NewCs):-Cons=('.'),
  Cs0=[C0,(H2:-B2)],
  nonvar(B2),functor(B2,Memb,2),
  add_true(C0,(H1:-Cont)),Cs=[(H1:-Cont),(H2:-B2)],atomic(Cont),
  nonvar(H1),functor(H1,Memb,2),
  arg(2,H1,A1),nonvar(A1),functor(A1,Cons,2),
  arg(2,H2,A2),nonvar(A2),functor(A2,Cons,2),
  ## proto_member(Ts,NewTs),
  Dict=[Memb/2-_,Cons/2-_,Cont/0-_],
  repl_pred_pattern(Dict,Ts,NewTs,Cs,NewCs).

proto_member(In,Out):-
  Dict=[lmemb/2-_,('.')/2-_],
  l2hl(
    [ (lmemb(X,[X|_]):-true),
      (lmemb(X,[_|Xs]):-
         lmemb(X,Xs)
      )
    ],
    In,Dict),
  l2hl(
    [  
      (lmemb(X,Xs):-member_entry(X,Xs)),
      (lmemb(X,[Y|Ys]) :- member3(X,Ys,Y))
    ],
    Out,Dict).

repl_pred_pattern(Dict,Ts,NewTs,Cs,NewCs):-
  l2hl(Cs,ATs,Dict),
  collapse_instances(Ts,ATs),
  collapse_dict(Dict),
  hl2l(NewTs,NewCs).

term2pattern(Xs,Ys,D):-
  findall(X,member(X,Xs),NewXs),
  l2hl(NewXs,Ys,D).

t2ht(V,R,_):-var(V),!,R=V.
t2ht(X,R,D):-atomic(X),!,(dname(X/0,D,VX)->R=(VX=>[]);R=(X=>[])).
t2ht(T,R,D):-
   functor(T,F,N),
   T=..[F|Xs],
   l2hl(Xs,Ys,D),
   (dname(F/N,D,VF)->R=(VF=>Ys);R=(F/N=>Ys)).

l2hl([],[],_).
l2hl([X|Xs],[Y|Ys],D):-t2ht(X,Y,D),l2hl(Xs,Ys,D).

ht2t(V,R):-var(V),!,R=V.
ht2t((F/_ =>Xs),R):-
   hl2l(Xs,Ys),
   R=..[F|Ys].

hl2l([],[]).
hl2l([X|Xs],[Y|Ys]):-ht2t(X,Y),hl2l(Xs,Ys).

dname(X,Dict,VX):-member(X-VX,Dict),!.

collapse_dict([]):-!.
collapse_dict([FN-V|Xs]):-FN=V,collapse_dict(Xs).

%collapse_instances(A,B):-
%  ttyout((portray_clause(A),portray_clause(B))),fail.
collapse_instances([],[]).
collapse_instances([P|Ps],[I|Is]):-
   variant_of(P,I),P=I,
%   subsumes_chk(P,I),P=I,
   collapse_instances(Ps,Is).

% Clause-level replacements

% Transforms definite metaprograms to binary meta-programs
% definite meta clause -> definite clause+ some replacements

mdef_to_def((H:-B),(NewH:-NewB)):-
     hide_atom(H,NewH),
     % debugmes(hidden(H,NewH)),
     repl_body(B,NewB).
mdef_to_def((:-B),(:-NewB)):-
     repl_body(B,NewB),!,
     NewB,  % called here and not propagated to the next step
     fail.

% replaces metavars and some builtins in clause bodies
                         
repl_body(MetaVar,call(MetaVar)):-var(MetaVar),!.
repl_body(M,ExpM):-repl_macro(M,ExpM),!.
repl_body(A,NewA):-split_op(A,L),!,strip_nil(L,NewA).
repl_body(M,ExpM):-is_prolog(binprolog),
  vget0(peval_io,int(1)),
  peval_io(M,ExpM),!.
repl_body(A,NewA):-spying(A),!,NewA=spy_goal(A),debugmes(spying_on(A)).
repl_body(A,NewA):-is_delphi(A,D),!,
  make_delphi_call(A,D,NewA).
repl_body(A,MA):-hide_atom(A,MA).

cutp(X):- name(X,"$cut"). % don't ## this!!!

numbervar_name(VAR_V,N):-functor(VAR_V,'$VAR',1),arg(1,VAR_V,N).

get_deep_cut(X,X).

cut_free(X):-var(X),!.
cut_free(!):-!,fail.
cut_free((A,B)):-!,cut_free(A),cut_free(B).
cut_free((A;B)):-!,cut_free(A),cut_free(B).
cut_free((A->B)):-!,cut_free(A),cut_free(B).
cut_free(_).

avoid_replacing_cut(X):-
   cut_free(X),
   !,fail.
avoid_replacing_cut(_).

repl_macro('!','cut_to'(CUT)):-cutp(CUT).
repl_macro(get_deep_cut(X),get_deep_cut(CUT,X)):-cutp(CUT).
repl_macro(var(X),Known):-nonvar(X),repl_known(var(X),Known).
repl_macro(nonvar(X),Known):-nonvar(X),repl_known(nonvar(X),Known).
repl_macro(atomic(X),Known):-nonvar(X),repl_known(atomic(X),Known).
repl_macro(float(X),Known):-nonvar(X),repl_known(float(X),Known).
repl_macro(atomic(X),Known):-nonvar(X),repl_known(atomic(X),Known).
repl_macro((A,B),NewAB):-traverse_conj(A,B,NewAB).
repl_macro((A;B),NewAB):-repl_disj(A,B,NewAB).
repl_macro((A->B),(NewA->NewB)):-
        repl_body(A,NewA),
        repl_body(B,NewB).
repl_macro(if(C,T,E),if(NewC,NewT,NewE)):-
        repl_body(C,NewC),
        repl_body(T,NewT),
        repl_body(E,NewE).
repl_macro(compare(R,A,B),compare0(A,B,R)).
repl_macro(A==B,compare0(A,B,=)).
repl_macro(A@<B,compare0(A,B,<)).
repl_macro(A@>B,compare0(A,B,>)).
repl_macro(xor(A,B,C),'#'(A,B,C)).

% deprecated - now used in context of dcg expansion
%repl_macro('#'(X),dcg_connect(X)).
%repl_macro('#<'(X),dcg_def(X)).
%repl_macro('#>'(X),dcg_val(X)).
%repl_macro('##<'(X),dcg_tell(X)).
%repl_macro('##>'(X),dcg_telling(X)).

repl_macro(':'(M,X),module_call(M,X)).
repl_macro(findall(X,G,Xs),findall(X,MG,Xs)):-repl_goal(G,MG).
repl_macro(bagof(X,G,Xs),bagof(X,MG,Xs)):-repl_goal(G,MG).
repl_macro(setof(X,G,Xs),setof(X,MG,Xs)):-repl_goal(G,MG).
repl_macro(all_answers(X,G,Xs),all(X,MG,Xs)):-repl_goal(G,MG).
repl_macro(X^G,X^MG):-repl_goal(G,MG).

repl_macro(synchronize(G),synchronize(MG)):-repl_body(G,MG).
repl_macro(call(G),MG):-repl_body(G,MG).
repl_macro(\+(G),\+(MG)):-repl_body(G,MG).
repl_macro(not(G),not(MG)):-repl_body(G,MG).
repl_macro('##'(G),MG):-pexec(G,NewG),repl_body(NewG,MG).
repl_macro(copy_term(A,B),copy_term(0,A,B)).
repl_macro(save_term(A,B),copy_term(1,A,B)).
repl_macro(current_db(DB),vget0(current_db,DB/0)).

% repl_macro(metacall(G),MG):-repl_body(G,MG). % do not do this !!!

bp_delay(G,Mode):-is_prolog(binprolog),!,G,delay_cmd(G,Mode).
bp_delay(_,_).

bp_only(G):-bp_only(G,true).

bp_only(G,_):-is_prolog(binprolog),!,G.
bp_only(_,TrueFail):-TrueFail.

repl_goal(G,NewG):-is_prolog(binprolog),!,repl_goal1(G,NewG).
repl_goal(G,NewG):-repl_body(G,NewG).

repl_goal1(G,NewG):-var(G),!,NewG=call(G).
repl_goal1((A,B),Head):-!,
    traverse_conj(A,B,NewAB),
    make_new_head(NewAB,Head),
    compile_later(Head,NewAB).
repl_goal1((A;B),Head):-!,
    r_disj1(A,B,Head).
repl_goal1(G,NewG):-
    repl_body(G,NewG).

traverse_conj(A,B,NewAB):-nonvar(A),split_op(A,NewA),!,
        repl_body(B,NewB),
        app_body(NewA,NewB,NewAB).
traverse_conj(A,B,(NewA,NewB)):-
        repl_body(A,NewA),
        repl_body(B,NewB).

repl_disj(A,B,NewAB):-is_prolog(binprolog),!,r_disj0(A,B,NewAB).
repl_disj(A,B,NewAB):-traverse_disj(A,B,NewAB).

r_disj0(A,B,NewAB):-
        avoid_replacing_cut((A;B)),!,
        traverse_disj(A,B,NewAB).
r_disj0(A,B,Head):-
        r_disj1(A,B,Head).

r_disj1(A,B,Head):-
	make_new_head(or(A,B),Head),
	r_disj((A;B),Ns,[]),
        delay_disj(Head,Ns).

traverse_disj(If,C,if(NewA,NewB,NewC)):-nonvar(If),If=(A->B),!,
        repl_body(A,NewA),
        repl_body(B,NewB),
        repl_body(C,NewC).
traverse_disj(A,B,or(NewA,NewB)):-
        repl_body(A,NewA),
        repl_body(B,NewB).

delay_disj(Head,Ns):-
        member(N,Ns),
          compile_later(Head,N),
        fail.
delay_disj(_,_).

var2call(X,R):-var(X),!,R=call(X).
var2call(X,X).

r_disj((A;B))-->!,
  {var2call(A,CA)},
  {var2call(B,CB)},
  r_disj(CA),
  r_disj(CB).
r_disj((A->B))-->!,
  { repl_body(A,NA),
    repl_macro('!',CUT),
    repl_body(B,NB)
  },
  [(NA,CUT,NB)].
r_disj(A)-->
  {repl_body(A,NA)},
  [NA].

make_new_head(P,Head):-
   mod_or_file(M),
   new_name(M,Pred),
   vars_of(P,Args),
   Head=..[Pred|Args].

mod_or_file(F):-bb_val(compiler_current_file,F0),!,F=F0.
mod_or_file(M):-current_module(M).

compile_later(H,B):-
%  ttyout(pp_clause((H:-B))),
  assert(
          '$todo'( Mode,
                   translate_def((H:-B),Mode)
                 )
  ).

delay_pred(FN,Cs,Mode):-
  seeing(File),
  debugmes('% delaying multifile predicate'(FN,file=File)),
  assert(
          '$collect_pred'(FN,Cs,File,Mode)
  ).

repl_known(X,true):-X,!.
repl_known(_,fail).
        
split_op(X is B,R):-split_is_rel(X,B,R).
split_op(A < B,R):-split_rel(less,A,B,R).
split_op(A > B,R):-split_rel(greater,A,B,R).
split_op(A =< B,R):-split_rel(less_eq,A,B,R).
split_op(A >= B,R):-split_rel(greater_eq,A,B,R).
split_op(A =:= B,R):-split_rel(arith_eq,A,B,R).
split_op(A =\= B,R):-split_rel(arith_dif,A,B,R).
% split_op(tab(Expr),R):-split_is(X,Expr,R,[tab(Y)]). % faster static tab

split_is_rel(X,B,[expr(B,X)]):-var(B),!.
split_is_rel(X,B,[+(B,0,X)]):-atomic(B),!.
split_is_rel(X,B,[+(B,0,X)]):-float(B),!.
split_is_rel(X,B,[+(R,0,X)]):-is_prolog(binprolog),ground(B),!,expr(B,R). %,ttyprint(ground(B=R)).
split_is_rel(X,B,R):-split_is(X,B,R,[]).

app_body([],Bs,Bs):-!. % do not remove this
app_body([A|As],Bs,(A,Cs)):-app_body(As,Bs,Cs).

strip_nil([A],A):-!.
strip_nil([A|As],(A,Bs)):-strip_nil(As,Bs).

split_rel(Op,A,B,Res):-split_rel_1(Op,A,B,Res,[]).

split_rel_1(Op,A,B)-->
        split_is(X,A),
        split_is(Y,B),
        {OpXY=..[Op,X,Y]},
        emit_is(OpXY).

%split_is(X,A)-->{var(A)},!,emit_is(expr(A,X)). % slow but flexible is/2
split_is(X,A)-->{var(A)},!,{X=A}.               % fast but static is/2
split_is(X,A)-->{atomic(A)},!,{X=A}.
split_is(X,A)-->{float(A)},!,{X=A}.
split_is(R,OpAB)-->
        {OpAB=..[Op,A,B]},!,
        split_is(VA,A),
        split_is(VB,B),
        {OpArgs=..[Op,VA,VB,R]},
        emit_is(OpArgs).
split_is(R,OpA)-->
        {OpA=..[Op,A]},
        split_is(VA,A),
        {OpArgs=..[Op,VA,R]},
        emit_is(OpArgs).

emit_is(X,[X|Xs],Xs).

% compile time execution at user's risk

'##'(G):-is_compiled(G),!,G.
'##'(G):-errmes('not expanded by ##',G).
'##'(G):-arg(2,G,true).

% pexecuted goals should not be local to a module
pexec(G,NewG):-
  debugmes(compile_time_execution_of(G)),
  findall(G,G,Gs),pexec1(Gs,G,NewG).

pexec1([],_,fail).
pexec1([G0|Gs],G,NewG):-pexec2(Gs,G0,G,NewG).

pexec2([],G,G,true).
pexec2([G1|Gs],G0,G,member(G,[G0,G1|Gs])).


% converts a definite clause to a binary metaclause
%    where each metavariable Cont represents a "continuation"
%    and a goal G is represented by a clause :- G.

def_to_mbin((H:-B),M):-!,def_to_mbin0(H,B,M).
def_to_mbin(H,M):-def_to_mbin0(H,true,M).

def_to_mbin0('@@'(H,Upper),B,(HC:-BC)) :- nonvar(H),!,
        term_append(H,cont(ContH),HC),
        add_upper_cont(B,Upper,ContH,BC).
def_to_mbin0(H,B,(HC:-BC)) :-
        term_append(H,cont(Cont),HC),
        add_cont(B,Cont,BC).            

add_upper_cont(B,Upper,ContH,BC):-nonvar(Upper),!,
        add_cont(Upper,ContU,ContH),
        add_cont(B,ContU,BC).
add_upper_cont(B,Upper,ContH,BC):-
        add_cont((strip_cont(ContH,Upper,ContU),B),ContU,BC).

% adds a continuation to a term

add_cont((true,Gs),C,GC):-!,add_cont(Gs,C,GC).
add_cont((fail,_),C,fail(C)):-!.
add_cont((G,Gs1),C,GC):-!,
                 add_cont(Gs1,C,Gs2),
                 term_append(G,cont(Gs2),GC).
add_cont(G,C,GC):-term_append(G,cont(C),GC).

% simple WAM-code lister

show_code(To,IIs):-
        findall(I,show_code0(IIs,I),Is),
        length(Is,Len),
        write('BRUT WAM-ASSEMBLER:'(compilation_mode=To,length=Len)),nl,
        nth_member(I,Is,K),
          show_instr(K,I),
        fail.
show_code(To,IIs):-
        nl,write('POST TERM-COMPRESSION CODE:'(compilation_mode=To)),nl,
        mc_all_instr(IIs,Es),
        nth_member(I,Es,K),
          show_instr(K,I),
        fail.
show_code(To,IIs):-
        nl,write('FINAL CODE:'(compilation_mode=To)),nl,
        gen_code(To,IIs),
        fail.
show_code(_,_):-
        nl.

show_code0(IIs,I):-
        member(Is,IIs),
        member(I,Is),
        show_or_skip(I).

show_or_skip(ii(get,variable,arg(I),var(I-_,_))):-!,fail.
show_or_skip(ii(put,value,arg(I),var(I-_,_))):-!,fail.
show_or_skip(_).

show_instr(K,I):- I=ii(Op,T,X,Y),
        write([K]),write(' '),write(Op),write('_'),
        write(T),write(' '),
        show_fun(X),
        show_info(Y),
        nl.

show_fun(put):-!.
show_fun(get):-!.
show_fun(T):-write(T).

show_info(Var):-Var=var(I-_,_),!,
  write(' '),write(var(I)),
  put_code(9),put_code(9),put_code(9),write('% '),write(Var).
show_info(Var):-write(' '),write(Var).
  
show_steps(asm,M,D):-
  nl,M\==D,write('EXPANDED:'),nl,portray_clause(M),nl,fail
; write('DEFINITE:'),nl,portray_clause(D),nl,fail.
show_steps(_,_,_).

% ------------------------------------------------------------------
% BYTE CODE GENERATOR

bind_c_chunk_length(wam,Es,Es):-!.
bind_c_chunk_length(mem,Es,Es):-!.
bind_c_chunk_length(_,Es,NewEs):-bind_length(Es,_,_,0,NewEs,[]).

bind_length([],_,_,_)-->[].
bind_length([X|Xs],L1,L2,N)-->make_anti_call(X,Xs,L1,L2,N).

% nonvar(L1) is true inside a chunk
make_anti_call(ii(Op,_,Ai),Xs,L1,L2,N)-->{Op=c_chunk_variable},!,
  [ii(Op,len(L2),Ai)],
  {L1=N},
  bind_length(Xs,L1,L2,N).
make_anti_call(ii(Op,_,Ai),Xs,L1,L2,N)-->{Op=c_chunk_value},!,
  [ii(Op,len(L2),Ai)],
  {L2 is N-L1},
  bind_length(Xs,_,_,0).
make_anti_call(II,Xs,L1,L2,N)-->
   { nonvar(L1), % inside a chunk
     II=ii(Name,An,Ai),
     antigenic(Name,An,Ai)
   },!,
   {L2 is N-L1},
   [ii(c_chunk_value,len(L2),0)],
   [II],
%   {expel(II)},
   bind_length([ii(c_chunk_variable,_,0)|Xs],_,_,0).
make_anti_call(II,Xs,L1,L2,N1)-->
  {N2 is N1+1},
  [II],
  bind_length(Xs,L1,L2,N2).

/*  
expel(II):-
  currpred(Pred/Arity-ClauseNo/_Offs),
  ttyprint(expelling(pred(Pred,Arity),clause(ClauseNo),II)), % $$$
  fail.
expel(_).
*/

gen_code(To,IIs):-
  mc_all_instr(IIs,Es),
  bind_c_chunk_length(To,Es,NewEs),
  member(Enc,NewEs),    
    write_instr(Enc,To,Ok),
    Ok=no,
  !,
  fail.
gen_code(_,_).


maxarity(256).

% #if defined TEMPS_AT_END
% temparg(Arg,Temp):-maxarity(Max),Temp is (Max-1)-Arg.

temparg(Arg,Arg).

gen_instr(ii(Op,Type,Arg,Var),Enc):-beautify(Arg,Op,Type,Var,Enc),!.

write_instr(ii(II,X,Y),To,Ok):-encode(II,II,X,Y,To),!,Ok=yes.
write_instr(_,_,no).

mc_all_instr(IIs,Es):-findall(Enc,mc_one_instr(IIs,Enc),Es).

mc_one_instr(IIs,Enc):-
  member(Is,IIs),
  mc_peephole(Is,I),
  instr_cat(I,Enc).

instr_cat(ii(O,T,X,Y),ii(II,X,Y)):-symcat(O,T,II).

  
% eliminates redundancies and beautifies the code
beautify(arg(X),Op,Type,V,Enc):-!,encode_arg(X,Op,Type,V,Enc).
beautify(temp(X),Op,Type,V,Enc):-!,temparg(X,Temp),
   encode_arg(Temp,Op,Type,V,Enc).
beautify(cutarg(X),Op,Type,V,Enc):-!,encode_arg(X,Op,Type,V,Enc).
beautify(Val,Op,Type,var(Xn-_,_),Enc):-!,encode2(Op,Type,Val,Xn,Enc).
beautify(put,write,constant,X,Enc):-cutp(X),!,
  encode2(push,cut,?,?,Enc).
beautify(Y,Op,X,Z,Enc):-encode2(Op,X,Y,Z,Enc).

encode_arg(I,get,variable,var(I-_,_),empty_op):-!.
encode_arg(I,put,value,var(I-_,_),empty_op):-!.
encode_arg(An,Op,Type,var(Xn-_,_),Enc):-!,encode2(Op,Type,Xn,An,Enc).
encode_arg(1,Op,constant,X,Enc):-cutp(X),!,encode2(Op,cut,?,?,Enc).
encode_arg(An,Op,constant,S,Enc):-encode2(Op,constant,S,An,Enc).

encode2(Op,Type,X,Y,ii(Op,Type,X,Y)).

% compacts voids, terms and useless c_chunks
mc_peephole(Xs,X):-mc_top(Xs,X).

mc_input(O,Is,Js):-mc_input0(O,Is,Js). % ,ttyprint(O).

mc_input0(Y,[X|Xs],Ys):-
  gen_instr(X,I),
  mc_empty(I,Y,Xs,Ys).

mc_empty(empty_op,Y,Xs,Ys):-!,mc_input0(Y,Xs,Ys).
mc_empty(I,I,Xs,Xs).

mc_top(Ys,X):-mc_input(X0,Ys,Xs),mc0(Xs,X0,X).

mc0(Xs,X0,X):-op_type(X0,Op,Type),mc1(Op,Type,Xs,X0,X).

mc1(push,variable,Xs,X0,R):-mc_push(Xs,Ys,X0,Y),!,mc5(Ys,Y,R).
mc1(c_chunk,variable,Xs,X0,R):-!,mc_chunk(Xs,Ys,X0,Y),mc5(Ys,Y,R).
mc1(_,void,Xs,X0,R):-!,
 mc_sel(Xs,Ys,1,N),
 mc3(Ys,X0,N,R).
mc1(_,_,Xs,X0,R):-
 mc5(Xs,X0,R).

mc_push(Zs,Xs,X0,Y):-mc_input(End,Zs,Xs),
  op_type(End,push,structure),
  mc_transform_push(X0,End,R),!,Y=R.
mc_push(Xs,Xs,X0,X0).

mc_chunk(Zs,Xs,_,After):-
   mc_input(End,Zs,Ys),
   op_type(End,c_chunk,value),!,
   mc_input(After,Ys,Xs).
mc_chunk(Xs,Xs,X0,X0).

mc_sel(Zs,Ys,N1,N3):-
  mc_input(Y,Zs,Xs),
  op_type(Y,_,void),!,
  N2 is N1+1,
  mc_sel(Xs,Ys,N2,N3).
mc_sel(Xs,Xs,N,N).

mc3(_,X0,N,R):-mc4(N,X0,R).
mc3(Zs,_,_,R):-mc_input(Y,Zs,Ys),mc0(Ys,Y,R).

mc4(N,X0,R):-mc_transform_void(N,X0,R),!.
mc4(1,X0,X0).

mc5(_,R,R).
mc5(Zs,_,R):-mc_input(Y,Zs,Ys),mc0(Ys,Y,R).

op_type(ii(Op,T,_,_),X,Y):-!,X=Op,Y=T.
op_type(Bad,_,_):-errmes(expected(ii/4),Bad).

mc_transform_void(1,ii(unify,void,X,N),ii(unify,variable,X,N)):-!.
mc_transform_void(N,ii(Op,void,X,_),ii(Op,void,X,N)).

mc_transform_push(
  ii(push,variable,_,X),ii(push,structure,FN,X),
  ii(push,constant,FN,X)).

encode(unify_variable,II,get,Ai,To):-        wcode(To,II,1,Ai,?,0).

encode(write_variable,II,put,Ai,To):-        wcode(To,II,2,Ai,?,0).

encode(unify_value,II,get,Ai,To):-           wcode(To,II,3,Ai,?,0).

encode(write_value,II,put,Ai,To):-           wcode(To,II,4,Ai,?,0).

encode(unify_constant,II,get,Const,To):-     wcode(To,II,5,0,Const,0).

encode(write_constant,II,put,Const,To):-     wcode(To,II,6,0,Const,0).
encode(push_constant,II,F/N,Ai,To):-         wcode(To,II,6,Ai,F,N).

encode(get_constant,II,Const,Ai,To):-        wcode(To,II,7,Ai,Const,0).

encode(get_structure,II,Func/Arity,Ai,To):-  wcode(To,II,8,Ai,Func,Arity).

encode(put_constant,II,Const,Ai,To):-        wcode(To,II,9,Ai,Const,0).

encode(put_structure,II,Func/Arity,Ai,To):-  wcode(To,II,10,Ai,Func,Arity).

encode(get_variable,II,Xn,Ai,To):-           wcode(To,II,11,Xn,?,Ai). %move
encode(put_value,II,Xn,Ai,To):-              wcode(To,II,11,Ai,?,Xn). %move

encode(put_variable,II,Xn,Ai,To):-           wcode(To,II,12,Xn,?,Ai).

encode(get_value,II,Xn,Ai,To):-              wcode(To,II,13,Xn,?,Ai).

encode(push_cut,II,?,?,To):-                 wcode(To,II,14,0,?,0).

encode(put_cut,II,?,?,To):-                  wcode(To,II,15,0,?,0).

encode(get_cut,II,?,?,To):-                  wcode(To,II,16,0,?,0).

encode('execute_?',II,Pred,Arity,To):-       wcode(To,II,17,0,Pred,Arity).
       % proceed ==> 18 (by emulator)

encode(load_constant,II,AReg,Const,To):-     wcode(To,II,28,AReg,Const,0).

encode(load_variable,II,AReg,V,To):-         wcode(To,II,50,AReg,?,V).

encode(load_value,II,AReg,V,To):-            wcode(To,II,29,AReg,?,V).

encode(push_structure,II,Func/Arity,Ai,To):-  wcode(To,II,51,Ai,Func,Arity).

encode(push_variable,II,put,Ai,To):- wcode(To,II,52,Ai,?,0).

% encode(push_offset,II,Func/Arity,Ai,To):-  wcode(To,II,52,Ai,Func,Arity).

encode(unify_void,II,_,Ai,To):-        wcode(To,II,61,Ai,?,0).

encode(write_void,II,_,Ai,To):-        wcode(To,II,62,Ai,?,0).

encode(c_chunk_variable,II,Len,Ai,To):- 
  write_or_skip(To,II,63,Ai,?,Len).

encode(c_chunk_value,II,Len,Ai,To):-
        write_or_skip(To,II,64,Ai,?,Len).

encode('clause_?',II,Pred,N,To):- n_nop(X1), X is X1+1,          
        wcode(To,II,X,0,Pred,N).
encode('firstarg_?',II,F/N,MaxRegs,To):-
        n_nop(X1), X is X1+2,
        wcode(To,II,X,MaxRegs,F,N).
encode('end_?',II,Reg,Mode,To):-
   wcode(To,II,0,Reg,Mode,0).
encode(inline_variable,II,No,Arg,To):- n_inline(N),
   wcode(To,II,N,Arg,?,No).
encode(arith_variable,II,No,Arg,To):- n_arith(N),
  wcode(To,II,N,Arg,0,No).
encode(arith_value,II,No,Arg,To):- n_arith(N),
  wcode(To,II,N,Arg,1,No).
encode('builtin_?',II,No,Arg,To):- n_builtin(N),
  wcode(To,II,N,Arg,?,No).
encode_op(P,A,N,To):-
  II=op,
  n_nop(X1),X is X1+3,
  make_cmd([P," ",A," ",N],Name),
  wcode(To,II,X,0,Name,0).

% we put metacall at right to be able to override them
% by reconsulting builtins.pl
n_inline(N):-bu_ctr(n_inline,N).
n_arith(N):-bu_ctr(n_arith,N).
n_builtin(N):-bu_ctr(n_builtin,N).
n_nop(N):-bu_ctr(n_nop,N).

write_or_skip(wam,_,_,_,_,_):-!.
write_or_skip(mem,_,_,_,_,_):-!.
write_or_skip(To,II, Op,Ai,Nop,len(Len)):-
   c_threshold(Min,Max),Len>=Min,Len<Max, % $$$
   !,
   write_or_skip0(To,II,Op,Ai,Nop,Len).
write_or_skip(_,_II,_,_, _,_).

write_or_skip0(wam,II,Op,Ai,Nop,Len):-
   bb_val(c_flag,begin_end,yes),!,
   wcode(wam,II,Op,Ai,Nop,Len).
write_or_skip0(c,II,Op,Ai,Nop,_Len):-!,
   wspec_c(II,Op,Ai,Nop,0). % _Len not used in C.
write_or_skip0(asm,II,Op,Ai,Nop,Len):-!,
   (bb_val(c_flag,begin_end,yes)->CF=c;CF='n'),  
   wcode(asm,II,Op+CF,Ai,Nop,Len).
write_or_skip0(_,_II,_Op,_Ai,_Nop,_Len).

wcode(mem,_II,Op,Reg,F,N):-add_instr(Op,Reg,F,N).
wcode(wam,_II,Op,Reg,F,N):-
  put_code(Op),put_code(Reg),put_code(N),write(F),put_code(0).
wcode(c,  II,Op,Reg,F,N):-wcode_c(II,Op,Reg,F,N). % When compiling to C
wcode(asm,  II,Op,Reg,F,N):-wcode_asm(II,Op,Reg,F,N).

wcode_asm(II,Op,Reg,F,N):-
  Instr=..[II,Reg,F,N],
  write(Op),write('-->'),put_code(9),put_code(9),
  write(Instr),nl. % When compiling to asm


% *.h file we are currently sending C-code
let_c_chunk_file(F_H):-bb_let(c_chunk,file_name,F_H).

c_chunk_file(F_H):-bb_val(c_chunk,file_name,F_H).

% related to newpred

newpred(c,C):-!,C=(H:-_),newpred0(H).
newpred(_,_).

%newpred(_,C):-!,C=(H:-_),newpred0(H).

% deals with info about current predicate
newpred0(H):-
  functor(H,F,N),
  bb_val(predicate,name,F),
  bb_val(predicate,arity,N),!,
  bb_val(predicate,clause,I),
  I1 is I+1,
  bb_set(predicate,clause,I1),
  bb_let(predicate,offset,0).
newpred0(H):-
  functor(H,F,N),
  bb_let(predicate,name,F),
  bb_let(predicate,arity,N),
  bb_let(predicate,clause,1),
  bb_let(predicate,offset,0),
  bb_let(predicate,chunk,0).

currpred(F/N-I/Offs):-
  bb_val(predicate,name,F),
  bb_val(predicate,arity,N),
  bb_val(predicate,clause,I),
  bb_val(predicate,offset,Offs).

% Compiler interface ------------------------------------------------------

% usable from command line, repeatedly
qcompile(File):-
  pc,
  mcompile_file(File).
  
% compile and loads to memory
mcompile(File):-
  mcompile_file(File),
  abort.

fcompile(F):-stat_time(fcompile_file(F)).

scompile(F):- \+ is_prolog(binprolog),mcompile(F).
scompile(F):-
  survive_cleanup(F,NewF),
  init_cmd,
  stat_time(scompile_file(NewF)),
  abort.

mcompile_file(File):-
	find_file(File,F),
	compile_mem(F).

% Compiler implementation ------------------------

% compiles to memory    
compile_mem(InFile):-
        Mode=mem,
        quietmes(compiling(to(Mode),InFile,'...')),
        ctime(T1),
        survive_cleanup(InFile,File),
        init_cmd,
        stat_compile(translate_all(File,Mode)),
        set_current_user_file(File),
        terminate_file(Mode,1),
        !,
        ctime(T2),T is T2-T1,
        quietmes(compile_time(T)).
compile_mem(_):-
        ttyprint('compilation aborted'),
        restart.

fcompile_file(F0):-
  to_list(F0,Fs),
  fcompile_files(Fs),
  add_multifiles(Fs).

add_multifiles(Fs):-
  Mode=wam,
  once(append(_,[Last],Fs)),
  find_file(Last,F), 
  memoable_file(Mode,F,OldF),
  telling(T),
  tell_at_end(OldF),
  translate_delayed_multifile,
  told,
  tell(T).

fcompile_files(Fs):-
  member(F,Fs),
    compile_file0(F),
  fail.
fcompile_files(_).
  
scompile_file(F0):-
  Mode=wam,
  to_list(F0,Fs),
  member(F,Fs),
    find_file(F,IF),
    (  memoable_file(Mode,IF,OldF)->
       ( well_compiled_to(OldF,IF) -> true
       ; compile_file1(Mode,IF,OldF)
       ),
       load0(OldF)
    ;  translate_all(IF,mem)
    ),
  fail.
scompile_file(F0):-
  (  atomic(F0),find_file(F0,F)->set_current_user_file(F)
   ; true
  ),
  terminate_file(mem,1).



% modes: wam,c,asm,bin
compile0(Mode,[F|Fs],OutFile):-!,
  compile1(Mode,[F|Fs],OutFile).
compile0(Mode,File,OutFile):-
  compile1(Mode,[File],OutFile).

compile1(Mode,Fs,OutFile):-!,
  init_cmd,
  ctime(T1),
  xcompile(Mode,Fs,OutFile),
  ctime(T2),
  T is T2-T1,
  quietmes(total_compile_time(T)).

xcompile(Mode,InFiles,OutFile):-
        tell(OutFile),
        decorate_file(header,Mode),
        compile_builtins(Mode),
        member(InFile,InFiles),
        quietmes(compiling(to(Mode),InFile,'...')),
        ctime(T1),
        comp_file(Mode,InFile),
        ctime(T2),T is T2-T1,
        quietmes(compile_time(T)),
        fail.
xcompile(Mode,_,_):-
        terminate_file(Mode,0),
        decorate_file(footer,Mode),
        fail.
xcompile(_,_,_):-
        told.

decorate_file(Where,c):-!,c_decorate_file(Where).
decorate_file(_,_).

terminate_file(Mode,Level):-
  translate_delayed_multifile,
	terminate_file0(Mode,Level).

terminate_file0(Mode,Level):-
        make_dummy_end(Mode),
        gg_emit(Mode,[[ii(end,?,Level,Mode)]]),
        close_c_chunk_file(Mode).

make_dummy_end(c):-c_chunk_file('wam.h'),!.
make_dummy_end(Mode):-
   new_name(Mode,Dummy),
   maincomp(Mode,Dummy).


close_c_chunk_file(c):-!,write_c_chunk(told).
close_c_chunk_file(_).

gg_emit(mem,C):-gen_code(mem,C).
gg_emit(wam,C):-gen_code(wam,C).
gg_emit(c,C):-gen_code(c,C).
gg_emit(asm,C):-show_code(asm,C).
  
% compiles to disk
comp_file(Mode,InFile):-
        member(Mode,[wam,asm,bin,c]),!,
        translate_all(InFile,Mode).

well_compiled_to(OldF,F):-
% works when BinProlog is the actual absolute path
  \+ older_file(OldF,F)
  % ,unix_argv(0,BinProlog), older_file(BinProlog,OldF)
  .

memoing_translate(F,Mode,OldF):-
  Mode=wam,
  is_prolog(binprolog),
  \+ current_module(prolog), % do not comment it out during development!
  memoable_file(Mode,F,OldF), % (unless you know what it implies)
  !,
  ( well_compiled_to(OldF,F) -> true
    ; make_memo_file(F,Mode,OldF)
  ),
  include_memoed_file(OldF).
memoing_translate(F,Mode,'temp.wam'):-
  translate_all(F,Mode).

memoable_file(Mode,F,MemoF):-
  % this makes only *.pl files memoable (not *.pro files...)
  Suf=[Dot,_,_],
    name(F,L),
    append(Inf,Suf,L),
    !,
  Suf=".pl",
  name(Mode,NewSuf),
  append(Inf,[Dot|NewSuf],R),
  name(MemoF,R).

make_memo_file(InputF,Mode,MemoF):-
  Mode=wam,
  telling(CF),
    tell(MemoF),
      translate_all(InputF,Mode),
    told,
  tell(CF).

include_memoed_file(OldF):-
     seeing(CF),
                see(OldF),
                        repeat,
                                get_code(C),
                                put_to_eof(C),
                        !,
                seen,
     see(CF).

put_to_eof(-1):-!.
put_to_eof(C):-put_code(C),fail.

translate_all(F,Mode):-
        seeing(F0),
          translate_file(F,Mode),
        see(F0).

translate_file(F,Mode):-
  bb_let(compiler_current_file,F),
  init_gensym(F),
  see(F),
          rr_read_clause(Mode,C),
            translate_clause(C,Mode),
          !,
	  translate_delayed(Mode),
  seen.

rr_read_clause(Mode,C):-
  get_a_predicate(Mode,FN,Cs),
  %ttyout((write('% '),write(FN),nl)),
  debugmes(compiling(FN)),
  repl_pred(FN,Cs,NewCs),
  ( FN=F/N,functor(P,F,N),is_multifile(P)->delay_pred(FN,NewCs,Mode),fail
  ; member(C,NewCs)
  ).

tth:-cwrite(here),cnl.
ttt:-cwrite(there),cnl.

get_a_predicate(Mode,FN,[C|Cs]):-
  rclause(Mode,FN,C),
  ( C=end_of_file,!,Cs=[]
  ; get_all_clauses(Mode,FN,Cs)
  ).
get_a_predicate(Mode,FN,Cs):-
  get_a_predicate(Mode,FN,Cs).

get_all_clauses(Mode,FN,Cs):-findall(C,get_a_clause(Mode,FN,C),Cs).

get_a_clause(Mode,FN,C):-
   radd(Mode,NewFN,C),
   ( NewFN = FN-> true
   ; !, bb_def(left,over,NewFN-C),fail
   ).
get_a_clause(Mode,FN,C):-
   get_a_clause(Mode,FN,C).

rclause(_,FN,C):-bb_val(left,over,FN-C),bb_rm(left,over),!.
rclause(Mode,FN,C):-radd(Mode,FN,C).

radd(Mode,FN,C):-read_clause(C),get_pred(C,Mode,FN).

get_pred((H:-_),_,F/N):-!,functor(H,F,N).
get_pred((::-(H,_)),_,F/N1):-!,functor(H,F,N),N1 is N-1.
% BUG with end_module: executed before tha last predicate
% which therefore becomes automatically public - noticed in 7.94
get_pred(':-'(H),Mode,_):-!,translate_cmd(H,Mode),fail.
get_pred(H,_,F/N):-functor(H,F,N).

translate_delayed_multifile:-
  debugmes('% starting translate_delayed_multifile'),
  G= '$collect_pred'(FN,Cs,_File,Mode),
  ( is_asserted(G)->
      ( findall(FN-G,metacall(G),FNGs),
	      keysort(FNGs,Sorted),
	      debugmes('% translating multifile predicate'(FN,file=_File)),
	      member(FN-G,Sorted),
	      member(C,Cs),
	      translate_clause(C,Mode),
	      fail
      ; functor(G,F,N),
	      abolish(F,N)
      )
    ; true
  ).

translate_delayed(Mode):-
  Delayed='$todo'(Mode,Goal),
  is_asserted(Delayed),
  debugmes(translating_delayed_to(Mode)),
  !,
  translate_delayed1(Delayed,Goal),
  functor(Delayed,F,N),
  abolish(F,N).
translate_delayed(_).

translate_delayed1(Delayed,Goal):-
  Delayed,
  debugmes(delayed(Goal)),
  Goal,
  fail.
translate_delayed1(_,_).
  
translate_clause(end_of_file,_):-!.
translate_clause(':-'(C),Mode):-!,translate_cmd(C,Mode),fail.
translate_clause('::-'(H,B),Mode):-!,compile_binary(Mode,(H:-B)),fail.
translate_clause(C,Mode):-is_asserted_clause(C),!,
  translate_cmd(assert(C),Mode),fail.
translate_clause(C,Mode):-maincomp(Mode,C),fail.

is_asserted_clause((H:-_)):-!,is_asserted(H).
is_asserted_clause(H):-is_asserted(H).

translate_cmd0([F],Mode):-!,include_file(F,Mode).
translate_cmd0(compile(F),Mode):-!,include_file(F,Mode).
translate_cmd0(ensure_loaded(F),Mode):-!,include_file(F,Mode).
translate_cmd0(consult(F),Mode):-!,current_db(Db),
  delay_cmd(consult_included(F,Db),Mode).   % can be include
translate_cmd0(reconsult(F),Mode):-!,current_db(Db),
  delay_cmd(consult_included(F,Db),Mode).   % can be include
% translate_cmd0(delphi(FND),Mode):-!,delphi(FND),delay_cmd(delphi(FND),Mode).
% translate_cmd0(memo(FND),Mode):-!,memo(FND),delay_cmd(memo(FND),Mode).
translate_cmd0(interactive(On),_):-!,bp_only(interactive(On)).
translate_cmd0(spy(FN),_):-!,bp_only(spy(FN)).
translate_cmd0(nospy(FN),_):-!,bp_only(nospy(FN)).
translate_cmd0(trace,_):-!,bp_only(trace).
translate_cmd0(notrace,_):-!,bp_only(notrace).
translate_cmd0(dynamic(Ps),Mode):-!,bp_delay(dynamic(Ps),Mode).
translate_cmd0(multifile(Ps),_):-!,bp_only(multifile(Ps)).
translate_cmd0(discontiguous(Ps),_):-!,bp_only(discontiguous(Ps)).
translate_cmd0(module(M),_):-!,bp_only(module(M)).
translate_cmd0(begin_module(M),_):-!,bp_only(begin_module(M)).
translate_cmd0(end_module(M),_):-!,bp_only(end_module(M)).
translate_cmd0(end_module,_):-!,bp_only(end_module).
translate_cmd0(module(M,Ps),_):-!,bp_only(module(M,Ps)).
translate_cmd0(public(Ps),_):-!,bp_only(public(Ps)).
translate_cmd0(set_c_threshold(M),_):-!,bp_only(set_c_threshold(M)).
translate_cmd0(set_c_threshold(M,M1),_):-!,bp_only(set_c_threshold(M,M1)).

translate_cmd0(op(X,Y,Z),Mode):-!,
   op(X,Y,Z), 
   ( member(Mode,[wam,c])->encode_op(X,Y,Z,Mode)
   ; true
   ).
translate_cmd0(C,Mode):-
   delay_cmd(C,Mode).

translate_cmd(C,Mode):-
  debugmes(translating_cmd(C,Mode)),
  translate_cmd0(C,Mode).

%delay_cmd(C,_mem):- % used to be mem only  UNSOUND although useful
%  telling(F),
%  tell(user),
%    (metacall(C)->true;true),
%  told,
%  tell(F),
%  fail.
delay_cmd(Body,Mode):- % member(Mode,[wam,c]),!,
  cmd2clause(Body,C),
  maincomp(Mode,C),
  fail.

% execution of `:-' runtime commands
%
% it expects: 
% - init_gensym in decorate_file(header...
% - exec_run_time_commands/0 in main/1

init_cmd:-
  cmd_root(Root),
  get_cmd_no(Root).

get_cmd_no(Root):-
  bb_val(gensym,Root,_),
  bb_set(gensym,Root,0),!.
get_cmd_no(_).

cmd2clause(Body,(Head:-Body)):-
  cmd_root(Root),
  gensym(Root,Head),
  debugmes('% !!! action delayed after compilation:'),debugmes((:-Body)),
  (public(Head/0)->true;true).

include_file(IFile,Mode):-
  db_clause('$include_db','$included'(IFile,Mode),true),!,
  debugmes(already_included_file(IFile,Mode)).
include_file(IFile,Mode):-
  db_assert('$include_db','$included'(IFile,Mode)),
  include_file0(IFile,Mode,_).

compile_file0(InFile):-
   Mode=wam,
   find_file(InFile,IF),
   memoable_file(Mode,IF,OutFile),
   compile_file1(Mode,IF,OutFile).

compile_file1(Mode,IF,OutFile):-
   telling(CF),
   tell(OutFile),
     translate_all(IF,Mode),
   told,
   tell(CF).
  
include_file0(IFile,Mode,MFile):-
  get_lineno(L), set_lineno(0),
  debugmes(lineno_before(L)),
  seeing(CF1),
  find_file(IFile,F),
  quietmes(begin(including(F),in(CF1))),
  memoing_translate(F,Mode,MFile),
  seeing(CF2),
  debugmes(lineno_after(L)),
  set_lineno(L),
  quietmes(end(including(F),in(CF2))).

quickcomp(C):-
   bb_let(is,prolog,quick_bp),
   maincomp(mem,C),
   bb_rm(is,prolog).

maincomp(Mode,C):-
   preprocess(C,M,D,B),
   show_steps(Mode,M,D),
   compile_binary(Mode,B).

% preprocess(Clause,BinClause):-preprocess(Clause,_,_,BinClause).

preprocess(C,M,D,B):-
	preprocess_def(C,M,D),
        def_to_mbin(D,B).

preprocess_def(C,M,D):-
        fact2rule(C,M),    
        mdef_to_def(M,D).

translate_def(D,Mode):-
        def_to_mbin(D,B),
        compile_binary(Mode,B).

fact2rule((:-B),(:-B)):-!.
fact2rule((H:-B),(H:-B)):-!.
fact2rule(H,(H:-true)).

% --------------------------------------------------------------------
% tools

cparser:-bb_def(is,prolog,bin_prolog_with_cparser).

kmake:-cparser,make(wam).

kcmake:-cparser,cmake(wam).


tboot:-tboot(12,60).

tboot(Min):-tboot(Min,10000).

tboot(Min,Max):-integer(Min),integer(Max),set_c_threshold(Min,Max),cboot.


tmake:-tmake(12,60).

tmake(Min):-tmake(Min,10000).

tmake(Min,Max):-integer(Min),integer(Max),set_c_threshold(Min,Max),cboot.


% fast C code generators
 
qmake(Project):-qmake(Project,user).

qmake(Project,Module):-set_c_threshold(12,60),cmake(Project,Module).


% compact C-code generators

cboot:-cmake(binpro,prolog),cmake(wam,prolog).

cmake:-cmake(wam).

cmake(Project):-cmake(Project,user).

cmake(Project,Module):-make(Project,Module,c,'.c').

        
% useful when BinProlog is re-compiled by a version with diffrent builtins

% remake1:-[builtins]=>(disable_builtins,make). % in module user

remake:-reboot.

reboot:-refresh_builtins,boot. % in module prolog

% generate runtime only - uses run.pro. 
% needs linking of binpro.c run.c (instead of wam.c)

rmake:-make(run,prolog).
crmake:-cmake(run,prolog). 

disable_builtins:-
  current_db(DB),
  bp_val(bp_virtual,H,1),
    debugmes(disabling_static(DB,H)),
    disable_static(DB,H),
  fail.
disable_builtins.

refresh_builtins:-
  current_db(DB),
  consult(builtins,in),
  make_new_builtins(in,out),
  set_db(out),
  make_all_static,
  set_db(DB),
  override_builtins,
  db_clean(in),
  db_clean(out).

make_new_builtins(In,Out):-
  db_clause(In,H,B),
  module_predicate(new,H,NewH),
  db_assert(Out,(NewH:-B)),
  fail.
make_new_builtins(_,_).

override_builtins:-
   bp_val(bp_virtual,H,1),
   % db_head'(in,H),
    module_predicate(new,H,NewH),
    functor(H,F,N),
    debugmes(overriding_builtin(F/N)),
    override(3,H,NewH),
  fail.
override_builtins.

% dynamic recompilation

% will inc counter for <DB,H> by 1 until =<0 (when dyn_compiles)

dyn_compile(H):-current_db(DB),dyn_compile(DB,H).

% called by do_goal in lib.pl
dyn_compile(DB,H):- 
   debugmes(dyn_compiling(DB,H)),
   functor(H,F,N),make_static(F/N)
   %,rm(H,DB),
   %,set(H,DB,0)
   .

% on-place static<-->dynamic conversion

make_static(F/N):-
  functor(H,F,N),
  is_asserted(H),
  dyn2stat(H),
  !.
make_static(FN):-
  errmes(unable_to,make_static(FN)).

make_dynamic(F/N):-
  functor(H,F,N),
  is_compiled(H),
  stat2dyn(H),
  !.
make_dynamic(FN):-
  errmes(unable_to,make_dynamic(FN)).

make_all_static:-dyn2stat(_).

make_all_dynamic:-stat2dyn(_).

dyn2stat(H):-
  current_db(DB),
  dyn2stat(DB,H).

dyn2stat(DB,H):-
  disable_static(DB,H),
  vget0(code_top,Top),
  vget0(code_oldtop,OldTop),
  vset(code_oldtop,Top),
    make_static0(DB,H), 
  terminate_file0(mem,1),
  vset(code_oldtop,OldTop).

stat2dyn(H):-
  current_db(DB),
  stat2dyn(DB,H).

stat2dyn(DB,H):-
  db_head(DB,H),
    disable_static(DB,H),
  fail.
stat2dyn(_,_).

make_static0(DB,H):-
  db_clause(DB,H,B),
    quickcomp((H:-B)),
  fail.
make_static0(_,_).

% bootstrapping predicates

boot:-make(wam,prolog).

make:-make(wam).

make(Proj):-make(Proj,user).

make(Project,Module):-make(Project,Module,wam,'.bp').

make(Project,Module,Mode,Suf):-make(Project,Module,Mode,Suf,Project).

make(Project,Module,Mode,Suf,Mes):-
        current_module(M0),
        module(Module),
        ( make0(Project,Mode,Suf,Mes)->true
        ; \+ errmes(unable_to_make,
           [mode(Mode),project(Project,Suf),in_module(Module)])
        ),
        module(M0).

make0(Project,Mode,Suf,Mes):-
        ctime(T1),
        find_file(Project,F),make_cmd([Project,Suf],NewF),
        make_include(Mode,Project),
        compile0(Mode,F,NewF),
        ctime(T2),T is T2-T1,
        write(Mes=time(T)),nl.

make_include(c,Project):-!,
  make_cmd([Project,".h"],F_H),
  let_c_chunk_file(F_H).
make_include(_,_).

asm:-comp_file(asm,user).

asm(Project):-make(Project,user,asm,'.asm','BinWAM intermediate code').

% compiler to C - some constants

set_c_threshold(Min):-set_c_threshold(Min,100000).

set_c_threshold(Min,Max):-
   bb_let(c_flag,c_threshold_min,Min),
   bb_let(c_flag,c_threshold_max,Max).

set_c_trace(X):-bb_let(c_flag,trace,X).

c_threshold(Min):-c_threshold(Min,_).

c_threshold(Min,Max):-
   bb_val(c_flag,c_threshold_min,Min),
   bb_val(c_flag,c_threshold_max,Max),
   !.
c_threshold(5,4):-c_chunk_file('wam.h'),!. % Min > Max means: never
c_threshold(5,500).

