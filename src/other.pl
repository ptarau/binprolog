:-dynamic(std_timer/1).
:-dynamic(try_to_load_file/1).

hide_atom(X,X).

std_expand_term(C,D):-expand_term(C,D).

fast_write(X):-write(X).
ttyprint(X):-telling(F),tell(user),write(X),nl,tell(F).

symcat(Op,Type,OpType):-	
	[Link]="_",
	name(Op,Pref),
	name(Type,Suf),
	append(Pref,[Link|Suf],Chars),
	!,
	name(OpType,Chars).

append([],Ys,Ys).
append([A|Xs],Ys,[A|Zs]):-
	append(Xs,Ys,Zs).

for(Min,Min,Max):-Min=<Max.
for(I,Min,Max):-
        Min<Max,
        Min1 is Min+1,
        for(I,Min1,Max).

% appends a last argument to a term

term_append(T,C,TC):-T=..LT,C=..[_|LC],append(LT,LC,LTC),!,TC=..LTC.

make_fname(Name,Suf,NameSuf):-
  name(Name,L1),
  name(Suf,L2),
  append(L1,L2,L),!,
  name(NameSuf,L).

init_other_sicstus:-
  asserta( (std_timer(T):-!,statistics(runtime,[T,_])) ),
  asserta( (try_to_load_file(F):-!,
     make_fname(F,'.pl',FN),ensure_loaded(FN)) ).

init_other_pl:-
  asserta( (std_timer(T):-!,statistics(cputime,T)) ),
  asserta( (try_to_load_file(F):-!,
    make_fname(F,'.pl',FN),ensure_loaded(FN)) ).

init_other_cprolog:-
  asserta( (findall(X,G,Xs):-!, bagof(X,X^G,Xs) ) ),
  asserta( (std_timer(T):-!, T is cputime ) ),
  asserta( (try_to_load_file(F):-!,
    make_fname(F,'.pl',FN),reconsult(FN)) ).

init_other_bim:-
  asserta( (std_timer(T):-!, cputime(T)) ),
  asserta( (float(T):-!, real(T)) ),
  asserta( (expand_term(T,E):-!,
    portable_expand_term(T,E)) ),
  asserta( (try_to_load_file(F):-!,
    make_fname(F,'.pl',FN),reconsult(FN)) ),
  try_to_load_file(dcg).

init_other_sb:-
  asserta( (std_timer(T):-!,statistics(runtime,[T,_])) ),
  asserta( (try_to_load_file(F):-!,sb_load(F)) ).

sb_load(F):-!,
  make_fname(F,'.pl',Fpl),make_fname(F,'.out',Fout),
  compile(Fpl,Fout),
  load(Fout),
  make_fname('rm ',Fout,Clean),
  system(Clean).

init_other(prolog):-!,init_other_sicstus. 
init_other(sicstus -f):-!,init_other_sicstus. 
init_other(sicstus):-!,init_other_sicstus. % tested with Sicstus 2.1_6
init_other(pl):-!,init_other_pl.  % tested with SWI-Prolog 1.8.5
init_other(sb):-!,init_other_sb. % tested SB-Prolog 3.1 
init_other(cprolog):-!,init_other_cprolog. % tested with Cprolog 1.4a
init_other(api):-!,init_other_sicstus. % with Aquarius 1.0: FAILS
init_other(bim):-!,init_other_bim.

load_bp_files(Prolog):-
  init_other(Prolog),
  try_to_load_file(oper),
  try_to_load_file(builtins),
  try_to_load_file(init),
  try_to_load_file(bin),
  try_to_load_file(co).

boot_to(F):-make_kernel(F,[]).
 
make_kernel(File,Extras):-
	kernel_files(Kernel,Extras),
	compile0(wam,Kernel,File).

kernel_files(['oper.pl','init.pl','builtins.pl','lib.pl','dcg.pl',
   'read.pl','write.pl','bin.pl','co.pl','top.pl','extra.pl'|Fs], Fs).

boot_from_other(Prolog):-
  load_bp_files(Prolog),
  boot_to('otherwam.bp').

call_ifdef(_G,Other):-Other.

% BinProlog compatibilty package for other Prologs.
%
% It allows execution of BinProlog-style bboard operations
% which do not depend on the semantics (logical or immediate udate)
% of the underlying assert/retarct operations: therefore it ensures
% that code using it will be highly portable.
%
% Tested on: Aquarius 1.0 SB-Prolog 3.1, C-Prolog 1.4, SWI-prolog.

:-dynamic(bb/3).
:-G=bb('$empty','$empty','$empty'),assert(G),retract(G),!.

term2key(T,NewT):-nonvar(T),functor(T,Tf,Tn),functor(NewT,Tf,Tn).

bb_def(N,K,_):-bb(N,K,_),!,fail.
bb_def(N,K,X):-bb_assert(N,K,X).

bb_val(N,K,X):-nonvar(N),nonvar(K),bb(N,K,Y),!,X=Y.

bb_set(N,K,X):-nonvar(N),nonvar(K),retract(bb(N,K,_)),!,
  bb_assert(N,K,X).
bb_set(N,_,_):-
  bb_error(N).

bb_rm(N,K):-nonvar(N),nonvar(K),
  ( retract(bb(N,K,_))->true
    ; bb_error(N)
  ).

bb_error(T):-functor(T,F,N),
  write(F/N),
  write(' ??? bb_def/3 expected before bb_set/3, bb_rm/2'),nl,
  fail.

bb_let(N,K,X):-bb_val(N,K,_),!,bb_set(N,K,X).
bb_let(N,K,X):-bb_def(N,K,X).

bb_def(K,X):-bb_def(K,K,X).

bb_set(K,X):-bb_set(K,K,X).

bb_val(K,X):-bb_val(K,K,X).

bb_rm(K):-bb_rm(K,K).

bb_let(K,X):-bb_let(K,K,X).

bb:-
  bb(N,K,X),functor(N,Nf,Nn),functor(K,Kf,Kn),
  write(Nf/Nn+Kf/Kn=X),nl,fail
; nl.

bb_reset:-
  retract(bb(_,_,_)),
  fail
; true.

bb_assert(N,K,X):-
  term2key(N,NewN),
  term2key(K,NewK),
  assert(bb(NewN,NewK,X)).

init_gensym(Root):-bb_let(gensym,Root,0).

gensym(Root,Symbol):-gensym_no(Root,N),symcat(Root,N,Symbol).

gensym_no(Root,N1):-
        bb_val(gensym,Root,N),!,
        N1 is N+1,
        bb_set(gensym,Root,N1).
gensym_no(Root,N):-N=1,
        bb_def(gensym,Root,N).

metacall(X):-X.

spying(_):-fail.

is_asserted(H):-predicate_property(H,(dynamic)),!.

ctime(T):-std_timer(T).

put_code(X) :- put(X).
get_code(X) :- get(X).

peval_io(X,X).

read_clause(EC):-read(C),expand_term(C,EC).

is_prolog(other).

quiet(2). % changed to new 4.61 default !!!

%get_functor(T,F):-functor(T,F,_).
%get_arity(T,N):-functor(T,_,N).
%make_functor(F,N,T):-functor(T,F,N).

member(C,[C|_]).
member(C,[_|L]):- member(C,L).

ttyout(WriteOp):-
  telling(F),
  tell(user),WriteOp,ttyflush,
  % telling(user),
  tell(F).

variant_of(A,B):-subsumes_chk(A,B),subsumes_chk(B,A).

is_multifile(P):-predicate_property(P,multifile).

is_delphi(_,_):-fail.

:-dynamic ctr/1.

ctr(0).

new_name(Base,Name):-
  retract(ctr(N)),!,N1 is N+1,assert(ctr(N1)),
  name(N,Xs),
  name(Base,Bs),
  append("zyx",Bs,NewBs),
  append(NewBs,Xs,Ys),
  name(Name,Ys).

is_prolog(other).

get_lineno('?').
