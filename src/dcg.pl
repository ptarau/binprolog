%   File   : DCG.PL
%   Author : Richard A. OKeefe
%   Updated: Tuesday July 26th, 1988.
%   Purpose: Definite Clause Grammar rule to Prolog clause translator.

/*  This file is written in the ISO 8859/1 character set.  The "Copyright"
    line contains after the right parenthesis a character which when
    transmitted was character 169, the international copyright symbol.

    Copyright (C)) 1988, Quintus Computer Systems, Inc.

    This file is distributed in the hope that it will be useful,
    but without any warrantee.  You are expressly warned that it is meant
    to serve as a model, not for direct use:  all error checking and
    reporting has been omitted, and mistakes are almost surely present.
    Permission is granted to anyone to distribute verbatim copies of
    this source code as received, in any medium, provided that the
    copyright notice, the nonwarrantee warning, and this permission
    notice are preserved.  Permission is granted to distribute modified
    versions of this source code, or of portions of it, under the above
    conditions, plus the conditions that all changed files carry
    prominent notices stating who last changed them and that the derived
    material is subject to this same permission notice.  Permission is
    granted to include this material in products for which money is
    charged, provided that the customer is given written notice that the
    code is (or is derived from) material provided by Quintus Computer
    Systems, Inc., and that the customer is given this source code on
    request.


	----------------------------------------------------------------

    Now that weve got that (adapted from the GNU copyright notice)
    out of the way, here are the technical comments.

    The predicates are all named dcg_<something>/<some arity> in order
    to keep out of the way, with the exception of phrase/2 and phrase/3
    which bear their proper names.  Only phrase/[2,3] and dcg_rule/2
    are meant to be called directly, and dcg_rule/2 is meant to be called
    from expand_term/2.  You need to keep dcg_body/4 and its dependents
    around at run time so that variables as nonterminals in DCG rule bodies
    will work correctly.

    So that Quintus have _something_ left to sell, this code has been
    rewritten from scratch with no error checking or reporting code at
    all, and a couple of places accept general grammar rule bodies where
    they are really supposed to demand lists of terminals.  However, any
    rule which is valid according to the Quintus Prolog manual will be
    translated correctly, except that this code makes no attempt to handle
    module: prefixes.  (The change is trivial.)	

    Note that dcg_rule/2 and phrase/[2,3] are steadfast.

    Minor modifications as BinProlog is was not optimized for disjunction
    initally. 
    (a:-b;c) replaced in some places by a:-b. a:-c. by Paul Tarau.
    Using also term_append/3 instead of structure crunching.
*/

try_dcg_expansion(H,B,EC):-dcg_rule('-->'(H,B),EC),!.
try_dcg_expansion(H,_,_):-errmes('dcg expansion error->',H).

%   dcg rule(+Grammar Rule, -Equivalent Clause)

/*
% old version : changed to push-back at the left of the body Paul Tarau
dcg_rule(-->(Head0,Body0), Clause) :-
	dcg_head(Head0, Head, PushBack, S0, S),
	dcg_body(Body0, Body1, S0, S),
	dcg_conj(Body1, PushBack, Body),
	Clause = :-(Head,Body).
*/

dcg_rule(X,Y):-var(X),!,Y=X.
dcg_rule(-->(Head,Body), Clause) :-
	dcg_head(Head, DCGHead, DCGPushBack, S0, S),
	dcg_body(Body, DCGBody1, S0, S),
	dcg_conj(DCGPushBack, DCGBody1, DCGBody),
	Clause = ':-'(DCGHead,DCGBody).

%   dcg head(+Head0, -Head, -PushBack, -S0, -S)
%   recognises both
%	NonTerminal, [PushBackList] -->
%   and
%	NonTerminal -->
%   It returns the difference pair S0\S which the body is to parse.
%   To avoid error checking, it will accept an arbitrary body in place
%   of a pushback list, but it should demand a proper list.

dcg_head((Head0 @@ Cont0), (Head @@ Cont), true, S0, S2) :- !,
        dcg_body(Head0,Head,S0,S1),
	dcg_body(Cont0,Cont,S1,S2).
dcg_head((Head0,PushBack0), Head, PushBack, S0, S1) :- !,
	dcg_goal(Head0, Head, S0, S),
	dcg_body(PushBack0, PushBack, S, S1).
dcg_head(Head0, Head, true, S0, S) :-
	dcg_goal(Head0, Head, S0, S).

/* this seems redundant... Paul Tarau
dcg_head(Head0, Head, true, S0, S) :-
	dcg_body(Head0, Head, S0, S).
*/

%   dcg goal(+Goal0, -Goal, +S0, +S)
%   adds the arguments S0, S at the end of Goal0, giving Goal.
%   It should check that Goal0 is a callable term.

dcg_goal(Goal0, Goal, S0, S) :-
   term_append(Goal0,S0+S,Goal).

%   dcg_body(+Body0, -Body, +S0, +S)
%   translates Body0 to Body, adding arguments as needed to parse S0\S.
%   It should complain about bodies (such as 2) which are not callable
%   terms, and about lists of terminals which are not proper lists.
%   To avoid error checking, [a|foo] is accepted as [a],foo, but it
%   really should complain.  ONLY the forms lists here should be treated;
%   other non-terminals which look like calls to built-ins could well be
%   commented on (no error reporting here) but should be expanded even
%   so.  Thus X=Y as a nonterminal is to be rewritten as =(X,Y,S0,S),
%   perhaps with a warning.  If you want the translation X=Y, use {X=Y}.

dcg_body(Var, Body, S0, S) :- var(Var), !,
	Body = phrase(Var,S0,S).
dcg_body((A0,B0), Body, S0, S) :- !,
	dcg_body(A0, A, S0, S1),
	dcg_body(B0, B, S1, S),
	dcg_conj(A, B, Body).
dcg_body((A0->B0), (A->B), S0, S) :- !,
	dcg_body(A0, A, S0, S1),
	dcg_body(B0, B, S1, S).
dcg_body((A0;B0), (A;B), S0, S) :- !,
	dcg_disj(A0, A, S0, S),
	dcg_disj(B0, B, S0, S).
dcg_body({A}, A, S, S) :- !.
dcg_body(!, !, S, S) :- !.
dcg_body([], true, S, S) :- !.
dcg_body([H0|T0], Body, S0, S) :- !,
	dcg_term(H0, H, S0, S1),
	dcg_body(T0, T, S1, S),
	dcg_conj(H, T, Body).
dcg_body(NT0, NT, S0, S) :-
	dcg_goal(NT0, NT, S0, S).


%   dcg_term(+T0, -T, +S0, +S)
%   generates code (T) which succeeds when there is a terminal T0
%   between S0 and S.  This version uses the DEC-10 Prolog predicate
%   C/3 for compatibility with DEC-10 Prolog, C Prolog, Quintus Prolog.
%   This is the only place that knows how terminals are translated, so
%   you could supply instead the definition
%	dcg_term(T0, S0=[T0|S], S0, S).
%   and reap the same benefits.  The one thing you must not do is
%   NO! dcg_term(T0, true, [T0|S], S). DONT DO THAT!

dcg_term(T0, 'C'(S0,T0,S), S0, S).


%  To see why dcg disj/4 is needed, consider the translation of
%  ( [] | [a] ).  We have to insert S1=S0 somewhere, but we do it at
%  "compile"-time if we can.

dcg_disj(Body0, Body, S0, S) :-
	dcg_body(Body0, Body1, S1, S),
        dcg_disj0(S0,S1,S,Body1,Body).

dcg_disj0(S0,S1,S,Body1,Body):-S1==S,!,dcg_conj(S1=S0, Body1, Body).
dcg_disj0(S0,S0,_,Body,Body).

%   dcg_conj(+A, +B, -C)
%   combines two conjunctions A, B, giving C.  Basically, we want to
%   ensure that there arent any excess trues kicking around (in a
%   compiled system, that shouldnt matter).  There is room for some
%   leeway here: I have chosen to flatten A completely.

dcg_conj(A, true, A) :- !.
dcg_conj(A, B, C) :-
	dcg_CONJ(A, B, C).

dcg_CONJ(true, C, C) :- !.
dcg_CONJ((A,As), C0, (A,C)) :- !,
	dcg_CONJ(As, C0, C).
dcg_CONJ(A, C, (A,C)).


%   'C'(S0, T, S)
%   is true when the terminal T "Connects" the "string positions" S0 and S.

'C'([T|S], T, S).


%   phrase(+NT0, ?S0)
%   is true when the list S0 is in the language defined by the
%   grammar rule body NT0.  E.g. phrase(([a],[b]), [a,b]).

phrase(NT0, S0) :-
	phrase(NT0, S0, []).


%   phrase(+NT0, ?S0, ?S)
%   is true when the list S0\S is in the language defined by the
%   grammar rule body NT0.  E.g. phrase(([a],[b]), [a,b|X], X).

phrase(NT0, S0, S) :-
	dcg_body(NT0, NT, T0, T),
	T0 = S0, T = S,
	NT.

portable_expand_term(A,B):-var(A),!,B=A.
portable_expand_term('-->'(H,B),EC):-!,try_dcg_expansion(H,B,EC).
%portable_expand_term('-->>'(H,B),EC):-!,
%	G=try_edcg_expansion(H,B,EC),
%	call_ifdef(G,errmes(expected_to_expand,G)).
% add other fancy term_expansion/2 predicates here (see: '-->>' in edcg.pl)
portable_expand_term(C,NewC):-default(term_expansion(C,NewC),NewC=C).
