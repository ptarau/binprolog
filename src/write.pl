%   File   : WRITE.PL
%   Author : Richard A. O'Keefe.
%   Updated: 22 October 1984
%   Purpose: Portable definition of write/1 and friends.

/* minor changes: Paul Tarau 1992
- uses fast_write (written in C) for simple things
- disconects unimplemented current_... predicates
- minor change by replacing (strange) l_magic with l_magic_nl
- removed code not used in BinProlog
*/


% this can be safely overriden, it seems free of side effects on failure

% generic_write(Term):-assumed(write_style(M)),!,generic_write(Term,M).
generic_write(Term):-generic_write(Term,writeq).

generic_write(Term,Mode):- w_out(Term, Mode, 1200, punct,_).

portable_display(Term) :-
        w_out(Term, display).

portable_print(Term) :-
        w_out(Term, print).

portable_write(Term) :-
        w_out(Term, write).

portable_writeq(Term) :-
        w_out(Term, writeq).


w_out(Term,Style):-
  w_out(Term, Style, 1200, punct,_),
  fail.
w_out(_,_).
        
top_writeq_atom(A):-w_out(A, writeq, 999, punct, _).

%   maybe_paren(P, Prio, Char, Ci, Co)
%   writes a parenthesis if the context demands it.

maybe_paren(P, Prio, Char, _, punct) :-
        P > Prio,
        !,
        put_code(Char).
maybe_paren(_, _, _, C, C).

%   maybe_space(LeftContext, TypeOfToken)
%   generates spaces as needed to ensure that two successive
%   tokens won't run into each other.

maybe_space(punct, _) :- !.
maybe_space(X, X) :- !,
        put_code(32).
maybe_space(quote, alpha) :- !,
        put_code(32).
maybe_space(_, _).

%   put_string(S)
%   writes a list of character codes.

put_string([]).
put_string([H|T]) :-
        put_code(H),
        put_string(T).


%   put_string(S, Q)
%   writes a quoted list of character codes, where the first
%   quote has already been written.  Instances of Q in S are doubled.

put_string([], Q) :-
        put_code(Q).
put_string([Q|T], Q) :- !,
        put_code(Q), put_code(Q),
        put_string(T, Q).
put_string([H|T], Q) :-
        put_code(H),
        put_string(T, Q).



%   w_variable(V)
%   is system dependent.  This just uses whatever Prolog supplies.

w_variable(V) :-
       fast_write(V). % seems to avoid bug with _

portray(T):-write(T).

%   w_out(Term, Style, Priority, Ci, Co)
%   writes out a Term in a given Style (display,write,writeq,print)
%   in a context of priority Priority (that is, operators with
%   greater priority have to be quoted), where the last token to be
%   written was of type Ci, and reports that the last token it wrote
%   was of type Co.

w_out(Term, _, _, Ci, alpha) :-
        var(Term),
        !,
       maybe_space(Ci, alpha),w_variable(Term).
w_out(VAR_V, Style, _, Ci, Co) :- ## numbervar_name(VAR_V,N), !,
        w_VAR(N, Style, Ci, Co).
w_out(N, _, _, Ci, alpha) :-
        integer(N),
        (   N < 0, maybe_space(Ci, other)
        ;   maybe_space(Ci, alpha)
        ),  !,
        fast_write(N).
w_out(Term, print, _, _Ci , alpha) :-
        portray(Term),
        !.
w_out(Atom, Style, Prio, _, punct) :-
        atom(Atom),
        current_op(P, _, Atom),
        P > Prio,
        !,
        put_code(40),
        (   Style = writeq, w_atom(Atom, Style, punct, _)
        ;   fast_write(Atom)
        ),  !,
        put_code(41).
w_out(Atom, Style, _, Ci, Co) :-
        atom(Atom),
        !,
        w_atom(Atom, Style, Ci, Co).
w_out(Term, display, _, Ci, punct) :- !,
        functor(Term, Fsymbol, Arity),
        w_atom(Fsymbol, display, Ci, _),
        w_args(0, Arity, Term, 40, display).
w_out({Term}, Style, _, _, punct) :- !,
        put_code(123),
        w_out(Term, Style, 1200, punct, _),
        put_code(125).
w_out([Head|Tail], Style, _, _, punct) :- !,
        put_code(91),
        w_out(Head, Style, 999, punct, _),
        w_tail(Tail, Style).
w_out((A,B), Style, Prio, Ci, Co) :- !,
        %  This clause stops writeq quoting commas.
        maybe_paren(1000, Prio, 40, Ci, C1),
        w_out(A, Style, 999, C1, _),
        put_code(44),
        w_out(B, Style, 1000, punct, C2),
        maybe_paren(1000, Prio, 41, C2, Co).
w_out(N, _, _, Ci, alpha) :-
        float(N),
        (   N < 0,maybe_space(Ci, other)
        ;   maybe_space(Ci, alpha)
        ),  !,
        fast_write(N).
w_out(Term, Style, Prio, Ci, Co) :-
        functor(Term, F, N),
        w_out(N, F, Term, Style, Prio, Ci, Co).

write_float(N):-fast_write(N).

w_out(1, F, Term, Style, Prio, Ci, Co) :-
        arg(1, Term, A),
        (   current_op(O, fx, F), P is O-1
        ;   current_op(O, fy, F), P = O
        ),  !,
        maybe_paren(O, Prio, 40, Ci, C1),
        w_atom(F, Style, C1, C2),
        w_out(A, Style, P, C2, C3),
        maybe_paren(O, Prio, 41, C3, Co).
w_out(1, F, Term, Style, Prio, Ci, Co) :-
        arg(1, Term, A),
        (   current_op(O, xf, F), P is O-1
        ;   current_op(O, yf, F), P = O
        ),  !,
        maybe_paren(O, Prio, 40, Ci, C1),
        w_out(A, Style, P, C1, C2),
        w_atom(F, Style, C2, C3),
        maybe_paren(O, Prio, 41, C3, Co).
w_out(2, F, Term, Style, Prio, Ci, Co) :-
        arg(1, Term, A),
        arg(2, Term, B),
        (   current_op(O, xfy, F), P is O-1, Q = O
        ;   current_op(O, xfx, F), P is O-1, Q = P
        ;   current_op(O, yfx, F), Q is O-1, P = O
        ),  !,
        maybe_paren(O, Prio, 40, Ci, C1),
        w_out(A, Style, P, C1, C2),
        w_oper(F, O, Style, C2, C3),
        w_out(B, Style, Q, C3, C4),
        maybe_paren(O, Prio, 41, C4, Co).
w_out(N, F, Term, Style, _Prio, Ci, punct) :-
        w_atom(F, Style, Ci, _),
        w_args(0, N, Term, 40, Style).


w_oper(Op, Prio, Style, Ci, Co) :-
        Prio < 700, !,
        w_atom(Op, Style, Ci, Co).
w_oper(Op, _, Style, _Ci, punct) :-
        put_code(32),
        w_atom(Op, Style, punct, _),
        put_code(32).


w_VAR(N, _Style, Ci, alpha) :-
        integer(N), N >= 0, !,
        maybe_space(Ci, alpha),
        Temp is N mod 26, Letter is Temp + 65, % $$$
        put(95), % writes "_"
        put_code(Letter),
        (   N < 26
        ;   Rest is N//26, fast_write(Rest) ), !.
w_VAR(A, _Style, Ci, Co) :-
        atom(A), !,
        maybe_space(Ci, alpha),
        w_atom(A, write, Ci, Co).
w_VAR(X, Style, Ci, punct) :-
	##numbervar_name(VAR_V,X),
        w_atom('$VAR', Style, Ci, _),
        w_args(0, 1, VAR_V, 40, Style).


w_atom(('!'), _, _, punct) :- !,
        put_code(33).
w_atom((';'), _, _, punct) :- !,
        put_code(59).
w_atom([], _, _, punct) :- !,
        put_code(91), put_code(93).
w_atom('{}', _, _, punct) :- !,
        put_code(123), put_code(125).
w_atom('.', writeq, _, quote):-!,put_string("'.'").
w_atom(Atom, Style, Ci, Co) :-
        name(Atom, String),
        (   classify_name(String, Co),
            maybe_space(Ci, Co),
%$$         put_string(String)    %-- seems safe
            fast_write(Atom)
        ;   Style = writeq, Co = quote,
            maybe_space(Ci, Co),
            put_code(39), 
            put_string(String, 39)
        ;   Co = alpha, maybe_space(Ci, Co),
%$$         put_string(String)    %-- seems safe
            fast_write(Atom)
        ),  !.

%   classify_name(String, Co)
%   says whether a String is an alphabetic identifier starting
%   with a lower case letter (Co=alpha) or a string of symbol characters
%   like ++/=? (Co=other).  If it is neither of these, it fails.  That
%   means that the name needs quoting.  The special atoms ! ; [] {} are
%   handled directly in w_atom.  In a basic Prolog system with no
%   way of changing the character classes this information can be
%   calculated when an atom is created, and just looked up.  This has to
%   be as fast as you can make it.

/* Paul Tarau -> defined in read.pl: is_min, etc. */

classify_name([H|T], alpha) :-
        is_min(H),
        !,
        classify_alpha_tail(T).
classify_name([H|T], other) :-
        is_spec(H),
        classify_other_tail(T).

classify_alpha_tail([]).
classify_alpha_tail([H|T]) :-
        is_an(H),
        classify_alpha_tail(T).

classify_other_tail([]).
classify_other_tail([H|T]) :-
        is_spec(H),
        classify_other_tail(T).


%   w_args(DoneSoFar, Arity, Term, Separator, Style)
%   writes the remaining arguments of a Term with Arity arguments
%   all told in Style, given that DoneSoFar have already been written.
%   Separator is 0'( initially and later 0', .

w_args(N, N, _, _, _) :- !,
        put_code(41).
w_args(I, N, Term, C, Style) :-
        J is I+1,
        arg(J, Term, A),
        put_code(C),
        w_out(A, Style, 999, punct, _),
        w_args(J, N, Term, 44, Style).


%   w_tail(Tail, Style)
%   writes the tail of a list of a given style.

w_tail(Var, _) :-                       %  |var]
        var(Var),
        !,
        put_code(124),
        w_variable(Var),
        put_code(93).
w_tail([], _) :- !,                     %  ]
        put_code(93).
w_tail([Head|Tail], Style) :- !,        %  ,Head tail
        put_code(44),
        w_out(Head, Style, 999, punct, _),
        w_tail(Tail, Style).
w_tail(Other, Style) :-         %  |junk]
        put_code(124),
        w_out(Other, Style, 999, punct, _),
        put_code(93).

%namevars0(T,T).
%namevars0(T,T):-namevars1(T,0,_).
namevars0(V,NewV):-copy_term(V,NewV),numbervars(NewV,0,_).

pp_clause(C):-portray_clause(C).

portray_clause(C):-namevars0(C,C1),pp_clause0(C1),fail.
portray_clause(_).

pp_term(T):-namevars0(T,T1),write(T1),nl,fail.
pp_term(_).

pp_clause0(:-(Body)) :- !,
        nl,
        l_clauses(Body, 0, 2, 8). % 0!='' in sicstus,sb..
pp_clause0((Pred:-Body)) :-
        top_writeq_atom(Pred),
        l_clauses(Body, 0, 2, 8), !.
pp_clause0((Pred)) :-
        pp_clause0((Pred:-true)).


l_clauses((A,B), L, R, D) :- !, % portray_clause bug with dynamic/1 here
        l_clauses(A, L, 1, D), !,
        l_clauses(B, 1, R, D).
l_clauses(true, _, 2, _) :- !,[P]=".",
        put_code(P), nl.
l_clauses((A;B), L, R, D) :- !,
        l_magic(fail, L, D),
        l_magic((A;B), 0, 2, D),
        l_magic_nl(R, '.').

l_clauses((A->B), L, R, D) :- !,
        l_clauses(A, L, 5, D), !,
        l_clauses(B, 5, R, D).
l_clauses(Goal, L, R, D) :-
        l_magic(Goal, L, D),
        top_writeq_atom(Goal),
        l_magic_nl(R,'.').

l_magic(!,    0, _) :- !,fast_write(' :- ').
l_magic(!,    1, _) :- !,fast_write(',  ').
l_magic(_, 0, D) :- !,
        fast_write(' :- '),
        nl, tab(D).
l_magic(_Goal, 1, D) :- !,
        [Char]=",",
        put_code(Char),
        nl, tab(D).
l_magic(_, 3, _) :- !,fast_write('(   ').
l_magic(_, 4, _) :- !,
        fast_write(';   ').
l_magic(_, 5, D) :- !,
        fast_write(' ->'),
        nl, tab(D).
l_magic(_, Key, D) :-
        atom(Key),
        fast_write((':- ')), fast_write(Key),
        nl, tab(D).

l_magic_nl(2, C) :- !, fast_write(C),nl.
l_magic_nl(_, _).

l_magic((A;B), L, R, D) :- !,
        l_magic(A, L, 1, D), !,
        l_magic(B, 1, R, D).
l_magic(Conj,  L, R, D) :-
        E is D+8,
        M is L+3,
        l_clauses(Conj, M, 1, E),
        nl, tab(D),
        l_magic2(R, ')' ).

l_magic2(2, C) :- !, fast_write(C).
l_magic2(_, _).

