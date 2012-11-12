is_prolog(X):-bb_val(is,prolog,Other),!,X=Other.
is_prolog(binprolog).

% peval_io(X,X).

peval_io(get_code(X),get0(X)).
peval_io(put_code(X),put(X)).
peval_io(nl,cnl).
peval_io(fast_write(T),cwrite(T)).

peval_io(seeing(F),seeing_telling(0,F)).
peval_io(telling(F),seeing_telling(1,F)).
peval_io(seeing_at(F),seeing_telling_at(0,F)).
peval_io(telling_at(F),seeing_telling_at(1,F)).

get_code(X):-get0(X).
put_code(X) :- put(X).
nl :- cnl.
fast_write(X):-cwrite(X).

read_chars(Cs):-get_code(C),read_more_chars(C,Cs).

read_more_chars(-1,_):-!,fail.
read_more_chars(10,[]):-!. % eol: return
read_more_chars(13,Cs):-!,read_chars(Cs). % ignore PC eol
read_more_chars(C,[C|Cs]):-read_chars(Cs).

write_chars([]).
write_chars([C|Cs]):-put_code(C),write_chars(Cs).

% stream based file and pipe IO

popen(Cmd,read,Stream):-!,
  open_stream(1,Cmd,r,var(CStream)),
  Stream='$stream'(CStream,seemark,Cmd).
popen(Cmd,write,Stream):-
  open_stream(1,Cmd,w,var(CStream)),
  Stream='$stream'(CStream,tellmark,Cmd).

pclose(Stream):-close_f_or_p(1,Stream).

% collects output from a command to a list of ascii codes
pcollect(Cmd,List):-
  popen(Cmd,read,Stream),
  current_input(Old),
  set_input(Stream),
  get_code(X),
  pcollect0(X,List),
  pclose(Stream),
  set_input(Old).


pcollect0(-1,[]):-!.
pcollect0(X,[X|Xs]):-
   get_code(NewX),
   pcollect0(NewX,Xs).

ls2list(DirName,Files):-dir2list('ls -F -1',DirName,Files).

dir2dirs(DirName,Files):-dir2list('dir /B /O:N /A:D',DirName,Files).

dir2files(DirName,Files):-dir2list('dir /B /O:EN /A:-D',DirName,Files).

dir2list(DLister,DirName,Files):-
  namecat(DLister,' ',DirName,Cmd),
  pcollect(Cmd,Cs),
  parse_dir_list(Ds,Cs,[]),
  !,
  Files=Ds.

parse_dir_list([DName|Ds])-->match_before(10,D),{name(DName,D)},parse_dir_list(Ds).
parse_dir_list([])-->[].

% C-style file operations

fopen(File,CMode,StreamId):-
   open_stream(0,File,CMode,var(StreamId)).

fclose(StreamId):-
   close_stream(0,var(StreamId)).
   
% builtin: fgetc(StreamId,CharCode):-...

% builtin: fputc(StreamId,CharCode):-...
% note the reversed order of args w.r.t C, for symettry with fgetc!

get_code('$stream'(S,seemark,_),X):-!,fgetc(S,X).
get_code(S,_):-errmes(bad_stream_in_get_code,S).

put_code('$stream'(S,tellmark,_),X):-!,fputc(S,X).
put_code(S,_):-errmes(bad_stream_in_put_code,S).

get_char(S,C):-get_code(S,X),
  X =:= -1 -> C=end_of_file
; name(C,[X]).

put_char(S,C):-name(C,[X]),!,put_code(S,X).
put_char(S,C):-errmes(error_in_put_char_to(S),C).

flush_output:-flush.

flush_output('$stream'(F,tellmark,_)):-!,fflush(F).
flush_output(S):-errmes(error_in_flush_output,S).

file_size(F,N):-
  exists_file(F),
  fopen(F,'rb',CS),
  fsize(CS,Size),
  fclose(CS),
  N=Size.

% ISO-like stream processing
open(File,Mode,'$stream'(CStream,IOMark,File)):-
  mode2c(Mode,CMode,IOMark,_,_),
  user_file(File,IOMark,UserFile),
  open_stream(0,UserFile,CMode,var(CStream)).

close(user):-!.
close(F):-atomic(F),get_stream(F,S),!,close_f_or_p(0,S).
close(X):-close_f_or_p(0,X).

get_stream(F,'$stream'(Name,IOMark,F)):-
  ( IOMark=seemark
  ; IOMark=tellmark
  ),
  tval(IOMark,F,S),
  (S=var(Name);S=Name/0),
  !.

close_f_or_p(_,'$stream'(_,_,user)):-!.
close_f_or_p(No,Stream):-
  Stream='$stream'(CStream,IOMark,CFile),
  current_stream(IOMark,Stream0),
  ( Stream==Stream0->
    set_default_stream(IOMark)
  ; true
  ),
  tlet(IOMark,CFile,closed_file/0),
  close_stream(No,var(CStream)),
 !.
close_f_or_p(No,Stream):-
  errmes(unable_to_close(No),Stream).

set_default_stream(seemark):-!,
  open(user,read,Stream),set_input(Stream).
set_default_stream(tellmark):-
  open(user,write,Stream),set_output(Stream).

set_stream(IOMark,Stream):-
  Stream='$stream'(CStream,IOMark,CFile),
  mode2c(_,_,IOMark,IOFile,IOFunc),!,
  vset(IOFile,var(CStream)), % follows see_tell convention
  vset(IOFunc,CFile/0),
  ( CFile==user->true
  ; tlet(IOMark,CFile,var(CStream))
  ).

current_stream(IOMark,Stream):-
  mode2c(_,_,IOMark,IOFile,IOFunc),
  vget0(IOFile,var(CStream)),
  vget0(IOFunc,CFile/0),!,
  Stream='$stream'(CStream,IOMark,CFile).


set_input(Stream):-set_stream(seemark,Stream).
set_output(Stream):-set_stream(tellmark,Stream).

current_input(Stream):-current_stream(seemark,Stream).
current_output(Stream):-current_stream(tellmark,Stream).

user_file(user,seemark,F):-!,F=user_input.
user_file(user,tellmark,F):-!,F=user_output.
user_file(F,_,F).

mode2c(read,rb,seemark,seefile,seefunc).
mode2c(write,wb,tellmark,tellfile,tellfunc).
mode2c(append,ab,tellmark,tellfile,tellfunc).



% see-tell based file access

see(File):-see_tell(0,File),!.
see(File):-user_error(unable_to_see,File).

seeing(F):-seeing_telling(0,F).
telling(F):-seeing_telling(1,F).

tell(File):-see_tell(1,File),!.
tell(File):-user_error(unable_to_tell,File).

tell_at_end(File):-see_tell(2,File),!.
tell_at_end(File):-user_error(unable_to_append_to,File).

seen:-seen_told(0).
told:-seen_told(1).

/* interface to fseek-ftell using the basic see-tell metaphor */

see_at(SeekPos):-see_tell_at(0,SeekPos),!.
see_at(SeekPos):-user_error(unable_to_see_at,SeekPos).

tell_at(SeekPos):-see_tell_at(1,SeekPos),!.
tell_at(SeekPos):-user_error(unable_to_tell_at,SeekPos).

seeing_at(FtellPos):-seeing_telling_at(0,FtellPos).
telling_at(FtellPos):-seeing_telling_at(1,FtellPos).

find_file0(File,NewFile):-
  quiet(X),quiet(6),
  (find_file(File,NewFile)->true
  ; NewFile=File
  ),
  quiet(X).

find_file(File,NewFile):-atom(File),!,
        ( file_search_path(Path),file_extension_list(Ext)
        % ; file_library(Path,Ext)
        ),
        find_file1(Path,Ext,File,NewFile).
find_file(library(File),NewFile):-atom(File),!,
        file_library(Path,Ext),
        find_file1(Path,Ext,File,NewFile).
find_file(File,_):-user_error('bad file name',File).

find_file_ext(File,Ext,NewFile):-
        file_search_path(Path),
        find_file1(Path,["",Ext],File,NewFile)->true
;       user_error('bad file name',File).


find_file1(Path,Ext,File,NewFile):-
        seeing(CF),
        find_file2(CF,Path,Ext,File,NewFile).

find_file2(CF,Path,Ext,File,NewFile):-
        see_a_file(Path,File,Ext,NewFile),!,
        see(CF).
find_file2(CF,Path,Ext,File,_):-
        see(CF),
        findall(N,(member(X,Path),name(N,X)),Ds),
        findall(N,(member(X,Ext),name(N,X)),Es),
        errmes(file_not_found(File),Ds+File+Es).

see_a_file(Prefs,File,Sufs,NewFile):-
        make_file_name(Prefs,File,Sufs,NewFile),
        see_or_fail(NewFile),
        seen.

%   File   : READ.PL
%   Author : D.H.D.Warren + Richard O'Keefe
%   Updated: 5 July 1984 
%   Purpose: Read Prolog terms in Dec-10 syntax.
/*
    Modified by Alan Mycroft to regularise the functor modes.
    This is both easier to understand (there are no more '?'s),
    and also fixes bugs concerning the curious interaction of cut with
    the state of parameter instantiation.

    Since this file doesn't provide "metaread", it is considerably
    simplified.  The token list format has been changed somewhat, see
    the comments in the RDTOK file.

    I have added the rule X(...) -> apply(X,[...]) for Alan Mycroft.

    -------------------------------------------------------
    SPURIOUS SYNTAX ERRORS ARE GENERATED WHITH STUFF LIKE:

    :-op(400,xfx,(@)).
    a:- @is,b.

    With the parser as is, the best remedy is to clean them up
    at the next read. They come from bad branches taken before good
    ones in nondeterministic parsing. 
    It is also wise to do simply:

    a:- @(is),b.

    to help the parser avoid spurious nondeterminism. - Paul Tarau

*/

read_clause(EC):-
  read_with_singletons(C,Vs,Ss),
  std_expand_term(C,EC),
  warn_singletons(Ss,Vs,C).

read_with_singletons(X,Vs,Ss):-r_term(X,Vs),singletons(Ss,Vs).

warn_singletons([S|Ss],Vs,C):-quiet(Q),Q=<2,
   warn_singletons1(C,[S|Ss],Vs),
   fail.
warn_singletons(_,_,_).

warn_singletons1(':-'(_),_,_):-!.
warn_singletons1(C,Ss,Vs):-
  add_true(C,(H:-_)),
  seeing_at(Byte),get_lineno(L),
  melt_varnames(Vs),
  ttyout((
    print(Ss),
    print('** warning singleton_variables=>'(line=L,byte=Byte)),nl,
    print('=> '),print(H),fast_write(':-...'),nl,
    nl
  )),
  !.

melt_varnames([]).
melt_varnames([var(N,V,_)|Xs]) :- V=N, melt_varnames(Xs).

read(X):-r_term(X,_).
top_read_term(T,V) :- read_term(T,V).
read_term(T,Vs) :- r_term(T,L),r_vars(L,Vs).

read_or_fail(T,Vs) :- r_term1(T,L),r_vars(L,Vs).

r_vars([],[]).
r_vars([var(N,V,_O)|Vs],[N=V|Es]):-r_vars(Vs,Es).

%  awkward O,I order allowing member(Result,Options),call(Result,Vars)
singletons(ONs,IVs):-findall(N,member(var(N,_V,s(0)),IVs),ONs).

print(X):-portable_print(X).
write(X):-portable_write(X).
writeq(X):-portable_writeq(X).

ttyin(ReadOp):-seeing(user),!,ReadOp.
ttyin(ReadOp):-
  seeing(F),
  see(user),
  ReadOp,
  %seing(user),
  see(F).

ttyout(_):-quiet(Q),Q>9,!. % quiet - suppresses tty output
ttyout(WriteOp):-telling(user),!,WriteOp.
ttyout(WriteOp):-
  telling(F),
  tell(user),WriteOp,flush,
  % telling(user),
  tell(F).

ttyprin(X):-ttyout(write(X)).
ttyprint(X):-ttyout((write(X),nl)).
ttycwrite(X):-ttyout(fast_write(X)).
ttycwriteln(X):-ttyout((fast_write(X),nl)).
ttynl:-ttyout(nl).
ttyput(X):-ttyout(put_code(X)).
display(X):-ttyout(portable_display(X)).
traceln(X):-ttycwriteln(X).

%   r_term(?Answer, ?Variables)
%   reads a term from the current input stream and unifies it with
%   Answer.  Variables is bound to a list of [Atom=Variable] pairs.

r_term(Answer, Variables) :-
	for(_,1,3), % at most 3 times
          r_term1(Term,Vars),
	!,
	Answer = Term,
        Variables = Vars.

r_term1(Term,Variables):-
       r_term2(Term,Variables,Ok),
       clean_up_syntax_errors,
       Ok=true.

r_term2(Term,Variables,Ok):-
	 read_tokens(Tokens, Variables),
	 r_and_check(Tokens,Term),
         !,
         Ok=true.
r_term2(_,_,fail).

clean_up_syntax_errors:-
  K1=syntax_error,K2=length,
  bb_val(K1,K2,_),bb_rm(K1,K2),
  !.
clean_up_syntax_errors.

r_and_check(Tokens,Term):-
	rt(Tokens, 1200, Term, LeftOver),
	all_read(LeftOver).
r_and_check(Tokens,_):-
	syntax_error(Tokens).

%   all_read(+Tokens)
%   checks that there are no unparsed tokens left over.

all_read([]) :- !.
all_read(S) :-
	syntax_error([operator,expected,after,expression], S).


%   expect(Token, TokensIn, TokensOut)
%   reads the next token, checking that it is the one expected, and
%   giving an error message if it is not.  It is used to look for
%   right brackets of various sorts, as they're all we can be sure of.

expect(Token, [Token|Rest], Rest) :- !.
expect(Token, S0, _) :-
	syntax_error([Token,or,operator,expected], S0).


%   I want to experiment with having the operator information held as
%   ordinary Prolog facts.  For the moment the following predicates
%   remain as interfaces to current_op. (i.e unfolded to val/3 - Paul Tarau)
%   prefixop(O -> Self, Rarg)
%   postfixop(O -> Larg, Self)
%   infixop(O -> Larg, Self, Rarg)


prefixop(Op, Prec, Prec) :-
	get_op0(Op, prefixop, fy, Prec), !.
prefixop(Op, Prec, Less) :-
	get_op0(Op, prefixop, fx, Prec),
	Less is Prec-1.

postfixop(Op, Prec, Prec) :-
	get_op0(Op, postfixop, yf, Prec), !.
postfixop(Op, Less, Prec) :-
	get_op0(Op, postfixop, xf, Prec), Less is Prec-1.

infixop(Op, Less, Prec, Less) :-
	get_op0(Op, infixop, xfx, Prec ), !, Less is Prec-1.
infixop(Op, Less, Prec, Prec) :-
	get_op0(Op, infixop, xfy, Prec), !, Less is Prec-1.
infixop(Op, Prec, Prec, Less) :-
	get_op0(Op, infixop, yfx, Prec), Less is Prec-1.

ambigop(F, L1, O1, R1, L2, O2) :-
	postfixop(F, L2, O2),
	infixop(F, L1, O1, R1).

%   rt(+TokenList, +Precedence, -Term, -LeftOver)
%   parses a Token List in a context of given Precedence,
%   returning a Term and the unread Left Over tokens.

rt([], _, _, _) :- syntax_error([expression,expected], []).
rt([Token|RestTokens], Precedence, Term, LeftOver) :-
	rts(Token, RestTokens, Precedence, Term, LeftOver).

%   rts(+Token, +RestTokens, +Precedence, -Term, -LeftOver)

rts(var(Variable,_), ['('|S1], Precedence, Answer, S) :- !,
	rt(S1, 999, Arg1, S2),
	r_args(S2, RestArgs, S3), !,
	exprtl0(S3, apply(Variable,[Arg1|RestArgs]), Precedence, Answer, S).
rts(var(Variable,_), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Variable, Precedence, Answer, S).
rts(atom(-), [integer(box(Integer,_))|S1], Precedence, Answer, S) :-
	Negative is 0-Integer, !,
	exprtl0(S1, Negative, Precedence, Answer, S).
rts(atom(-), [atom(F)|S1], Precedence, Answer, S) :-
	float(F),
	float_minus(F,Negative),!,
	exprtl0(S1, Negative, Precedence, Answer, S).
rts(atom(Functor), ['('|S1], Precedence, Answer, S) :- !,
	rt(S1, 999, Arg1, S2),
	r_args(S2, RestArgs, S3),
	Term =.. [Functor,Arg1|RestArgs], !,
	exprtl0(S3, Term, Precedence, Answer, S).
rts(atom(Functor), S0, Precedence, Answer, S) :-
	prefixop(Functor, Prec, Right), !,
	after_prefix_op(Functor, Prec, Right, S0, Precedence, Answer, S).
rts(atom(Atom), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Atom, Precedence, Answer, S).
rts(integer(box(Integer,_)), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, Integer, Precedence, Answer, S).
rts('[', [']'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, [], Precedence, Answer, S).
rts('[', S1, Precedence, Answer, S) :- !,
	rt(S1, 999, Arg1, S2),
	r_list(S2, RestArgs, S3), !,
	exprtl0(S3, [Arg1|RestArgs], Precedence, Answer, S).
rts('(', S1, Precedence, Answer, S) :- !,
	rt(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).
rts('((', S1, Precedence, Answer, S) :- !,
	rt(S1, 1200, Term, S2),
	expect(')', S2, S3), !,
	exprtl0(S3, Term, Precedence, Answer, S).
rts('{', ['}'|S1], Precedence, Answer, S) :- !,
	exprtl0(S1, '{}', Precedence, Answer, S).
rts('{', S1, Precedence, Answer, S) :- !,
	rt(S1, 1200, Term, S2),
	expect('}', S2, S3), !,
	exprtl0(S3, '{}'(Term), Precedence, Answer, S).
rts(string(List), S0, Precedence, Answer, S) :- !,
	exprtl0(S0, List, Precedence, Answer, S).
rts(Token, S0, _, _, _) :-
	syntax_error([Token,cannot,start,an,expression], S0).


%   r_args(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ')' and returns a list of terms.

r_args([','|S1], [Term|Rest], S) :- !,
	rt(S1, 999, Term, S2), !,
	r_args(S2, Rest, S).
r_args([')'|S], [], S) :- !.
r_args(S, _, _) :-
	syntax_error([',)',expected,in,arguments], S).


%   r_list(+Tokens, -TermList, -LeftOver)
%   parses {',' expr(999)} ['|' expr(999)] ']' and returns a list 
%   of terms.

r_list([','|S1], [Term|Rest], S) :- !,
	rt(S1, 999, Term, S2), !,
	r_list(S2, Rest, S).
r_list(['|'|S1], Rest, S) :- !,
	rt(S1, 999, Rest, S2), !,
	expect(']', S2, S).
r_list([']'|S], [], S) :- !.
r_list(S, _, _) :-
	syntax_error(['|]',expected,in,list], S).


%   after_prefix_op(+Op, +Prec, +ArgPrec, +Rest, +Precedence, 
%       -Ans, -LeftOver)

after_prefix_op(Op, Oprec, _, S0, Precedence, _, _) :-
	Precedence < Oprec, !,
        syntax_error([prefix,operator,Op,in,context,
                      with,precedence,Precedence],
	S0).
after_prefix_op(Op, Oprec, _, S0, Precedence, Answer, S) :-
	peepop(S0, S1),
	prefix_is_atom(S1, Oprec), % can't cut but would like to
	exprtl(S1, Oprec, Op, Precedence, Answer, S).
after_prefix_op(Op, Oprec, Aprec, S1, Precedence, Answer, S) :-
	rt(S1, Aprec, Arg, S2),
	Term =.. [Op,Arg], !,
	exprtl(S2, Oprec, Term, Precedence, Answer, S).


%   The next clause fixes a bug concerning "mop dop(1,2)" where
%   mop is monadic and dop dyadic with higher Prolog priority.

peepop([atom(F),'('|S1], [atom(F),'('|S1]) :- !.
peepop([atom(F)|S1], [infixop(F,L,P,R)|S1]) :- infixop(F, L, P, R).
peepop([atom(F)|S1], [postfixop(F,L,P)|S1]) :- postfixop(F, L, P).
peepop(S0, S0).


%   prefix_is_atom(+TokenList, +Precedence)
%   is true when the right context TokenList of a prefix operator
%   of result precedence Precedence forces it to be treated as an
%   atom, e.g. (- = X), p(-), [+], and so on.

prefix_is_atom([Token|_], Precedence) :- prefix_is_atom(Token, Precedence).

prefix_is_atom(infixop(_,L,_,_), P) :- L >= P.
prefix_is_atom(postfixop(_,L,_), P) :- L >= P.
prefix_is_atom(')', _).
prefix_is_atom(']', _).
prefix_is_atom('}', _).
prefix_is_atom('|', P) :- 1100 >= P.
prefix_is_atom(',', P) :- 1000 >= P.
prefix_is_atom([],  _).


%   exprtl0(+Tokens, +Term, +Prec, -Answer, -LeftOver)
%   is called by read/4 after it has read a primary (the Term).
%   It checks for following postfix or infix operators.

exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	ambigop(F, L1, O1, R1, L2, O2), !,
	(   exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, 
		Answer, S)
	;   exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, 
		Answer, S)
	).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	infixop(F, L1, O1, R1), !,
	exprtl([infixop(F,L1,O1,R1)|S1], 0, Term, Precedence, Answer, S).
exprtl0([atom(F)|S1], Term, Precedence, Answer, S) :-
	postfixop(F, L2, O2), !,
	exprtl([postfixop(F,L2,O2) |S1], 0, Term, Precedence, Answer, S).
exprtl0([','|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1000, !,
	rt(S1, 1000, Next, S2), !,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).
exprtl0(['|'|S1], Term, Precedence, Answer, S) :-
	Precedence >= 1100, !,
	rt(S1, 1100, Next, S2), !,
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).
exprtl0([Thing|S1], _, _, _, _) :-
	cant_follow_expr(Thing, Culprit), !,
	syntax_error([Culprit,follows,expression], [Thing|S1]).
exprtl0(S, Term, _, Term, S).

cant_follow_expr(atom(_),	atom).
cant_follow_expr(var(_,_),	variable).
cant_follow_expr(integer(_),	integer).
cant_follow_expr(string(_),	string).
cant_follow_expr('((',		bracket).
cant_follow_expr('(',		bracket).
cant_follow_expr('[',		bracket).
cant_follow_expr('{',		bracket).

exprtl([infixop(F,L,O,R)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	rt(S1, R, Other, S2),
	Expr =.. [F,Term,Other], /*!,*/
	exprtl(S2, O, Expr, Precedence, Answer, S).
exprtl([postfixop(F,L,O)|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= O, C =< L, !,
	Expr =.. [F,Term],
	peepop(S1, S2),
	exprtl(S2, O, Expr, Precedence, Answer, S).
exprtl([','|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1000, C < 1000, !,
	rt(S1, 1000, Next, S2), !,
	exprtl(S2, 1000, (Term,Next), Precedence, Answer, S).
exprtl(['|'|S1], C, Term, Precedence, Answer, S) :-
	Precedence >= 1100, C < 1100, !,
	rt(S1, 1100, Next, S2), !,
	exprtl(S2, 1100, (Term;Next), Precedence, Answer, S).
exprtl(S, _, Term, _, Term, S).


%   This business of syntax errors is tricky.  When an error is 
%   detected, we have to write out a message.  We also have to note 
%   how far it was to the end of the input, and for this we are 
%   obliged to use the data-base.  Then we fail all the way back to 
%   read(), and that prints the input list with a marker where the 
%   error was noticed.  If subgoal_of were available in compiled code 
%   we could use that to find the input list without hacking the 
%   data base.  The really hairy thing is that the original code 
%   noted a possible error and backtracked on, so that what looked 
%   at first sight like an error sometimes turned out to be a wrong 
%   decision by the parser.  This version of the parser makes
%   fewer wrong decisions, and my goal was to get it to do no
%   backtracking at all.  This goal has not yet been met, and it 
%   will still occasionally report an error message and then decide 
%   that it is happy with the input after all.  Sorry about that.


syntax_error(Message, List):-seeing(user),!,
  start_syntax_error(0,0,Message,List),
  fail.
syntax_error(Message, List) :-
	seeing(F),seeing_at(Byte),get_lineno(Line),
  see(user),
	  start_syntax_error(Line,Byte,Message,List),
	see(F),
	fail.

start_syntax_error(Line,Byte,Message,List):-
	length(List, Length),
	bb_def(syntax_error, length, err(Message,Line,Byte,Length)),!.
start_syntax_error(_,_,_,_).

syntax_error(List):-seeing(user),!,finish_syntax_error(List),fail.
syntax_error(List) :-
	seeing(F),see(user),
	finish_syntax_error(List),
	see(F),
	fail.

finish_syntax_error(List):-
	bb_val(syntax_error,length,err(Message,Line,Byte,AfterError)),
        bb_rm(syntax_error,length),
	ttynl,
          display('** SYNTAX ERROR ** LINE='),display(Line),
          display(' (approx.), BYTE='),display(Byte),display(':'),
	display_list(Message),
	length(List, Length),
	BeforeError is Length-AfterError,
	display_list(List, BeforeError),!.

display_list([Head|Tail]) :-
	ttyput(32),
	display_token(Head), !,
	display_list(Tail).
display_list([]) :-
	ttynl.

display_list(X, 0) :-
	display(' <HERE=> '), !,
	display_list(X, 99999).
display_list([Head|Tail], BeforeError) :-
	display_token(Head),
	ttyput(32),
	Left is BeforeError-1, !,
	display_list(Tail, Left).
display_list([], _) :-
	ttynl.

display_token(atom(X))	  :- float(X),!,write_float(X).
display_token(atom(X))	  :- !,display(X).
display_token(var(_,X))	  :- !,	display(X).
display_token(integer(box(I,_))) :- !,display(I).
display_token(string(Xs))  :- !,[Q]="""",
	det_append([Q|Xs],[Q],L),
	name(N,L),display(N).
display_token(X):-display(X).


% --------- rdtok.pl ----------------
%   File   : RDTOK.PL
%   Author : R.A.O'Keefe
%   Updated: 2 July 1984
%   Purpose: Tokeniser in reasonably standard Prolog.

/*  This tokeniser is meant to complement the library READ routine.
    It recognises Dec-10 Prolog with the following exceptions:

	%( is not accepted as an alternative to {

	%) is not accepted as an alternative to )

	NOLC convention is not supported (r_name could be made to 
		do it)

	,.. is not accepted as an alternative to | (hooray!)

	large integers are not read in as xwd(Top18Bits,Bottom18Bits)

	After a comma, "(" is read as '((' rather than '('.  This does 
		the parser no harm at all, and the Dec-10 tokeniser's 
		behaviour here doesn't actually buy you anything.  
		This tokeniser guarantees never to return '(' except 
		immediately after an atom, yielding '((' every
		other where.

    In particular, radix notation is EXACTLY as in Dec-10 Prolog 
    version 3.53.  Some times might be of interest.  Applied to an 
    earlier version of this file:

	this code took		    1.66 seconds
	the Dec-10 tokeniser took   1.28 seconds [DEC-10 assembler -Tim]
	A Pascal version took	    0.96 seconds

    The Dec-10 tokeniser was called via the old RDTOK interface, with
    which this file is compatible.  One reason for the difference in
    speed is the way variables are looked up: this code uses a linear
    list, while the Dec-10 tokeniser uses some sort of tree.  The 
    Pascal version is the program WLIST which lists "words" and their 
    frequencies.  It uses a hash table.  Another difference is the way 
    characters are classified: the Dec-10 tokeniser and WLIST have a 
    table which maps ASCII codes to character classes, and don't do 
    all this comparison and memberchking.  We could do that without 
    leaving standard Prolog, but what do you want from one evening's 
    work?
*/


%   read_tokens(TokenList, Dictionary)
%   returns a list of tokens.  It is needed to "prime" r_toks/2
%   with the initial blank, and to check for end of file.  The
%   Dictionary is a list of AtomName=Variable pairs in no particular 
%   order.  The way end of file is handled is that everything else 
%   FAILS when it hits character "-1", sometimes printing a warning.  
%   It might have been an idea to return the atom 'end_of_file' 
%   instead of the same token list that you'd have got from reading 
%   "end_of_file. ", but (1) this file is for compatibility, and (b) 
%   there are good practical reasons for wanting this behaviour.

read_tokens(TokenList, Dictionary) :-
  r_toks(32, Dict, ListOfTokens),
  append(Dict, [], Dict), !, %  fill in the "hole" at the end
  Dictionary = Dict,	     %  unify explicitly so we'll read and
  TokenList = ListOfTokens.  %  then check even with filled in arguments
read_tokens([atom(end_of_file)], []). %  Eof is all that can go wrong

r_toks(-1, _, _) :- !,	     %  -1 is the end-of-file character
	fail.			     %  in every standard Prolog
r_toks(Ch, Dict, Tokens) :-
	Ch =< 32,	     	     %  ignore layout.  CR, LF, and the
	!,			     %  Dec-10 newline character (31)
	get_code(NextCh),		     %  are all skipped here.
	r_toks(NextCh, Dict, Tokens).
r_toks(37, Dict, Tokens) :- !,	%  %comment
	repeat,				%  skip characters to a line
	    get_code(Ch),
	    is_terminator(Ch),
	!,	%  stop when we find one
	Ch =\= -1,			%  fail on EOF
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(47, Dict, Tokens) :- !,	%  /*comment?
	get_code(NextCh),
	r_solidus(NextCh, Dict, Tokens).
r_toks(33, Dict, [atom(!)|Tokens]) :- !,	%  This is a special case so
	get_code(NextCh),			%  that !. reads as two tokens.
	r_after_atom(NextCh, Dict, Tokens).	%  It could be cleverer.
r_toks(40, Dict, ['(('|Tokens]) :- !,	%  NB!!!
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(41, Dict, [')'|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(44, Dict, [','|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(59, Dict, [atom((;))|Tokens]) :- !,	%   ; is nearly a punctuation
	get_code(NextCh),			%   mark but not quite (e.g.
	r_toks(NextCh, Dict, Tokens).	%   you can :-op declare it).
r_toks(91, Dict, ['['|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(93, Dict, [']'|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(123, Dict, ['{'|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(124, Dict, ['|'|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(125, Dict, ['}'|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(46, Dict, Tokens) :- !,		%  full stop
	get_code(NextCh),				%  or possibly .=. &c
	r_fullstop(NextCh, Dict, Tokens).
r_toks(34, Dict, [string(S)|Tokens]) :- !,	%  "string"
	r_string(S, 34, NextCh),
	r_toks(NextCh, Dict, Tokens).
r_toks(39, Dict, [atom(A)|Tokens]) :- !,	%  'atom'
	r_string(S, 39, NextCh),
	name(A, S),		%  BUG: '0' = 0 unlike Dec-10 Prolog
	r_after_atom(NextCh, Dict, Tokens).
r_toks(Ch, Dict, [var(Var,Name)|Tokens]) :- is_maj(Ch),!,
	%  have to watch out for "_"
	r_name(Ch, S, NextCh),
	(  S = "_", Name = '_'		%  anonymous variable Var = _
	;  S=[C|_],
	   name(Name, S),		%  construct name
	   r_lookup(Dict, C, Name, Var)	%  lookup/enter in dictionary
	), !,
	r_toks(NextCh, Dict, Tokens).
r_toks(Ch, Dict, Tokens) :- is_num(Ch),!,
	r_integer(Ch, I, NextCh,Digits),
	r_toks(NextCh, Dict, Tokens1),
	try_float(Digits,Tokens1,I,Tokens).
r_toks(Ch, Dict, [atom(A)|Tokens]) :- is_min(Ch),!,
	r_name(Ch, S, NextCh),
	name(A, S),
	r_after_atom(NextCh, Dict, Tokens).
r_toks(Ch, Dict, [atom(A)|Tokens]) :-	% THIS MUST BE THE LAST CLAUSE
	get_code(AnotherCh),
	r_symbol(AnotherCh, Chars, NextCh),	% might read 0 chars
	name(A, [Ch|Chars]),			% so might be [Ch]
	r_after_atom(NextCh, Dict, Tokens).


%   The only difference between r_after_atom(Ch, Dict, Tokens) and
%   r_toks/3 is what they do when Ch is "(".  r_after_atom
%   finds the token to be '(', while r_toks finds the token to be
%   '(('.  This is how the parser can tell whether <atom> <paren> must
%   be an operator application or an ordinary function symbol 
%   application.  See the library file READ.PL for details.

r_after_atom(40, Dict, ['('|Tokens]) :- !,
	get_code(NextCh),
	r_toks(NextCh, Dict, Tokens).
r_after_atom(Ch, Dict, Tokens) :-
	r_toks(Ch, Dict, Tokens).


%   r_string(Chars, Quote, NextCh)
%   reads the body of a string delimited by Quote characters.
%   The result is a list of ASCII codes.  There are two complications.
%   If we hit the end of the file inside the string this predicate 
%   FAILS.  It does not return any special structure.  That is the 
%   only reason it can ever fail.  The other complication is that when
%   we find a Quote we have to look ahead one character in case it is 
%   doubled.  Note that if we find an end-of-file after the quote we 
%   *don't* fail, we return a normal string and the end of file 
%   character is returned as NextCh.  If we were going to accept 
%   C-like escape characters, as I think we should, this would need 
%   changing (as would the code for 0'x).  But the purpose of this 
%   module is not to present my ideal syntax but to present something 
%   which will read present-day Prolog programs.

r_string(Chars, Quote, NextCh) :-
	get_code(Ch),
	r_string(Ch, Chars, Quote, NextCh).

r_string(-1, _, Quote, -1) :-
	display('! end of file in: '), ttyput(Quote),
	display(token), ttyput(Quote), ttynl,
	!, fail.
r_string(Quote, Chars, Quote, NextCh) :- !,
	get_code(Ch),			% closing or doubled quote
	more_string(Ch, Quote, Chars, NextCh).
r_string(Char, [Char|Chars], Quote, NextCh) :-
	r_string(Chars, Quote, NextCh).	% ordinary character


more_string(Quote, Quote, [Quote|Chars], NextCh) :- !,
	r_string(Chars, Quote, NextCh).	% doubled quote
more_string(NextCh, _, [], NextCh).		% end



% r_solidus(Ch, Dict, Tokens)
%   checks to see whether /Ch is a /* comment or a symbol.  If the
%   former, it skips the comment.  If the latter it just calls 
%   r_symbol.  We have to take great care with /* comments to 
%   handle end of file inside a comment, which is why r_solidus/2 
%   passes back an end of file character or a (forged) blank that we 
%   can give to r_toks.


patch_slash(['(('|Xs],['('|Xs]):-!. % fixes bug with /\(a) -PT
patch_slash(Xs,Xs).

r_solidus(42, Dict, Tokens) :- !, % 42= '*'
	get_code(Ch),
	r_solidus(Ch, NextCh),
	r_toks(NextCh, Dict, Tokens).
r_solidus(Ch, Dict, [atom(A)|Tokens1]) :-
	r_symbol(Ch, Chars, NextCh),		% might read 0 chars
	name(A, [47|Chars]),
	r_toks(NextCh, Dict, Tokens),
        patch_slash(Tokens,Tokens1).

r_solidus(-1, -1) :- !,
	display('! end_of_file in /*.. comment'), ttynl.
r_solidus(42, LastCh) :-
	get_code(NextCh),
	NextCh =\= 47, !,	%  might be ^Z or * though
	r_solidus(NextCh, LastCh).
r_solidus(42, 32) :- !.	%  the / was skipped in the previous clause
r_solidus(_, LastCh) :-
	get_code(NextCh),
	r_solidus(NextCh, LastCh).


%   r_name(Char, String, LastCh)
%   reads a sequence of letters, digits, and underscores, and returns
%   them as String.  The first character which cannot join this sequence
%   is returned as LastCh.

r_name(Char, [Char|Chars], LastCh) :-
	is_an(Char),!,
	get_code(NextCh),
	r_name(NextCh, Chars, LastCh).
r_name(LastCh, [], LastCh).

%   r_symbol(Ch, String, NextCh)
%   reads the other kind of atom which needs no quoting: one which is
%   a string of "symbol" characters.  Note that it may accept 0
%   characters, this happens when called from r_fullstop.

r_symbol(Char, [Char|Chars], LastCh) :-
	is_spec(Char),
	get_code(NextCh),
	r_symbol(NextCh, Chars, LastCh).
r_symbol(LastCh, [], LastCh).


%   r_fullstop(Char, Dict, Tokens)
%   looks at the next character after a full stop.  There are
%   three cases:
%	(a) the next character is an end of file.  We treat this
%	    as an unexpected end of file.  The reason for this is
%	    that we HAVE to handle end of file characters in this
%	    module or they are gone forever; if we failed to check
%	    for end of file here and just accepted .<EOF> like .<NL>
%	    the caller would have no way of detecting an end of file.
%	(b) the next character is a layout character.  This is a
%	    clause terminator.
%	(c) the next character is anything else.  This is just an
%	    ordinary symbol and we call r_symbol to process it.

r_fullstop(-1, _, _) :- !,
	display('! end_of_file just after full_stop'), ttynl,
	fail.
r_fullstop(Ch, _, []) :-
	Ch =< 32, !.		% END OF CLAUSE
r_fullstop(Ch, Dict, [atom(A)|Tokens]) :-
	r_symbol(Ch, S, NextCh),
	name(A, [46|S]),
	r_toks(NextCh, Dict, Tokens).


%   r_integer is complicated by having to understand radix notation.
%   There are three forms of integer:
%	0 ' <any character>	- the ASCII code for that character
%	<digit> ' <digits>	- the digits, read in that base
%	<digits>		- the digits, read in base 10.
%   Note that radix 16 is not understood, because 16 is two digits,
%   and that all the decimal digits are accepted in each base (this
%   is also true of C).  So 2'89 = 25.  I can't say I care for this,
%   but it does no great harm, and that's what Dec-10 Prolog does.
%   The X =\= -1 tests are to make sure we don't miss an end of file
%   character.  The tokeniser really should be in C, not least to
%   make handling end of file characters bearable.  If we hit an end
%   of file inside an integer, r_integer will fail.

r_integer(BaseChar, IntVal, NextCh,Digits) :-
	Base is BaseChar - 48,
	get_code(Ch), [Dot]=".",
	r_int(Ch, Base, IntVal, NextCh, Ds), Digits=[Dot,BaseChar|Ds].
/*
 % Paul Tarau - float like 0.0003 used to become 0.3
	( name(Name,Digits),
	  errmes(number(Name),Digits)->true
        ; true
        ).
*/

r_int(-1,_, _, _, _):-!,fail.
r_int(39, 0, IntVal, NextCh, [39,IntVal]):-!,
	get_code(IntVal), IntVal =\= -1, get_code(NextCh).
r_int(39, Base, IntVal, NextCh, Ds):-
	r_digits(0, Base, IntVal, NextCh, Ds),!.
r_int(Ch,Base,IntVal, NextCh, Ds):-
	r_digs(Ch, Base, 10, IntVal, NextCh ,Ds).

r_digits(SoFar, Base, Value, NextCh, Ds) :-
	get_code(Ch),
	Ch =\= -1,
	r_digs(Ch, SoFar, Base, Value, NextCh, Ds).

r_digs(Digit, SoFar, Base, Value, NextCh, [Digit|Ds]) :-
	is_num(Digit),!,
	Temp is SoFar*Base, Temp1 is Temp-48, Next is Temp1+Digit,
	% Temp>0, % should check if int overflows here ! $$$
	r_digits(Next, Base, Value, NextCh, Ds).
r_digs(LastCh, Value, _, Value, LastCh, []).

%   r_lookup is identical to memberchk except for argument order and
%   mode declaration.

r_lookup([var(Name,Var,Occ)|_], C, Name, Var) :- !,r_varcount(Occ,C).
r_lookup([_|T], C, Name, Var) :- r_lookup(T, C, Name, Var). 

r_varcount(Occ,C):-[C]="_",!,Occ=s(1).
r_varcount(Occ,_):-var(Occ),!,Occ=s(_).
r_varcount(s(1),_).

% - various char types now generated in builtins.pl - see headers.pl

