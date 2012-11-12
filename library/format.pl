
%   File   : WRITEF.PL
%   Author : Lawrence + Richard
%   Updated: 10 September 1983
%   Purpose: Formatted write routine (and support)
%                       WRITEF - FORMATTED WRITE UTILITY
% 
% fwritef(File, Format)
% 
%         Formatted write to file.  Equivalent to 'fwritef(File, Format, [])'.
% 
% fwritef(File, Format, List)
% 
%         Formatted  write  to  File.    Temporarily redirects output to the file
%         named by File, but is otherwise like 'writef(Format, List)'.
% 
% writef(Format)
% 
%         Formatted write.  Equivalent to 'writef(Format, [])'.
% 
% writef(Format, List)
% 
%         Formatted write.  Format is an atom whose characters  will  be  output.
%         Format  may  contain  certain special character sequences which specify
%         certain  formatting  actions.    The  following  sequences  result   in
%         particular (otherwise hard to use) characters being output.
% 
%             '\n'  --  <NL> is output
%             '\l'  --  <LF> is output (same as <NL> on UNIX)
%             '\r'  --  <CR> is output
%             '\t'  --  <TAB> is output
%             '\e'  --  <ESC> is output
%             '\\'  --  The character "\" is output
%             '\%'  --  The character "%" is output
%             '\nnn' -  where nnn is an integer (1-3 digits)
%                       the character with ASCII code nnn is output
%                       (NB : nnn is read as DECIMAL)
% 
%         The  following  special  sequences specify that items be taken from the
%         List and output in some way.  List, then, provides all the actual terms
%         that are required to be output, while Format specifies  where  and  how
%         this is to occur.
% 
%             '%t'  --  output the next item (mnemonic: Term)
%             '%w'  --  Write the next item
%             '%q'  --  Writeq the next item
%             '%d'  --  Display the next item
%             '%p'  --  Print the next item (identical to %t)
%             '%l'  --  output the next item using prlist
%             '%c'  --  output the next item using prconj
%             '%e'  --  output the next item using prexpr
%             '%n'  --  put the next item as a character (ie it is
%                              an iNteger)
%             '%r'  --  Repeat the next item N times where N is the
%                             second item (an integer)
%             '%s'  --  write the next item as a String (so it must
%                             be a list of characters)
%             '%i'  --  the next item is a format, and the item after that
%                             a list.  Apply writef to that format and
%                             list (Indirect).  Use for error messages.
%             '%f'  --  perform a ttyflush (no items used)
% 
%         %l  (prlist)  works  on  lists  [A1,...,An].    %c  (prconj)  works  on
%         conjunctions A1&...&An.  They both display A1, ..., An with one element
%         per line, preceded by four  spaces.    You  will  probably  want  a  \n
%         (newline) in front of %l or %c, but unless you want a blank line in the
%         output there is no need for a \n afterwards.
% 
%         There is a new set of three special sequences:
% 
%             '%Nc' --  write the next item Centered in N columns.
%             '%Nl' --  write the next item Left justified in N columns.
%             '%Nr' --  write the next item Right justified in N columns.
%                       N is a decimal number with at least one digit.
%                       The item must be an atom, integer, or string.
% 
%         In  each  of  these  cases,  a minimum of N characters will be written.
%         Enough spaces will be added around (c), after (l),  or  before  (r)  to
%         make the total up to N. If the item is too long, exactly one space will
%         be added.
% 
%         The following examples may help to explain the use of writef.
% 
%             writef('Hello there\n\n')
%             writef('%t is a %t\n',[Person,Property])
%             writef('Input : %l \nBecomes : %e\n \7',[In,Out])
%             writef('%20l %4r clauses %3r predicates.\n', [File,Ccnt,Pcnt])
%             fwritef('demo.for', '\tDO %w I = 1, %w\n', [End,Limit])
% 

% FIXES
%
%  (11 May 81)	LB
%
%	Split the (now obsolete) module IOROUT into two: WRITEF (this one)
%	and TRACE.
%	Added cuts to writefs to make it determinate (it's tail recursive).
%
%  (8 September 82)	ROK
%
%	Added a clause to writef to allow the format to be a string.
%	Added the format items nL, nR, nC for atoms/numbers/strings.
%	Added the %s format code.  Made getxxx things grammar rules.
%
%  (9 September 82)	ROK
%
%	Fixed long-standing bug in ttyprint: 'tell' was 'see' !!
%
%  (22 June 83)		ROK
%
%	Added fwrite/2 and fwritef/3 by analogy to fprintf.
%	They are very often useful.
%	Also added the %i (indirect) format item, and the \e
%	escape (generates ESC) for talking to terminals.
%
%  (10 September 1983)  ROK
%
%	Added the %g (agglutinated) format item.  The idea of this is that
%	you can have a term like +(A,B,C,D) written as A + B + C + D.  ASA
%	is the only program to use it so far, but since such records are
%	quite a bit more compact than lists, it sems like a good idea.
%
%	Added the %x (ignore) format item, so that you can compute
%	a format : get_format(Key,Fmt), writef(Fmt, [List]) where
%	some of the variations don't want to display all the arguments.
%
%	Added the \b (backspace) and \f (formfeed) escapes.  This wants
%	to be done when strings are read, and wants to be exactly the same
%	as C.  Maybe in the next Prolog system...
%
%	If the list argument is neither [] nor a list, it will be turned
%	into a list of one element.  I keep forgetting to do this in my
%	source code, so writef might as well do it fo me.
%
%	Changed uses of & and # as operators to uses as function symbols,
%	so this file can be loaded when you're not using those operators.
%	Also made the logical stuff treat , as conjunction and ; (same as
%	|) as disjunction.  Renamed all prexpr's subroutines to prexpr,to
%	remove possible name conflicts.  That was a bit dubious, but I also
%	renamed special->wf_char and action->wf_act; those two were *bound*
%	to get in someone's way sooner or later, probably mine.
%
%    (15 September 1983)	ROK
%
%	Added the %v hack, which calls numbervars on the items in the list.
%	This is to make variables come out as letters, which I think looks
%	pretty, and it is harmless, because writef fails anyway!
%
%    (6 December 1983)		ROK
%	Moved to the VAX, again again
%

/*
:- public
	prconj/1,		%   print conjunction
	prexpr/1,		%   print logical expression
	prlist/1,		%   print list, one per line
	ttyprint/1,		%   print on terminal
	fwritef/2,
	fwritef/3,		%   formatted write to file
	writef/1,
	writef/2.		%   formatted write


:- mode
	ttyprint(?),
	prlist(?),
	prconj(?),
	prexpr(?),
		prexpr(+,+,-,?,?),
		prexpr(+,-,-,-),
		prexpr(+,+),
	fwritef(+,+),
	fwritef(+,+,+),
	writef(+),
	writef(+,+),
		wf_act(+,+,-),
		getcode(-,+,-),
		getdigits(+,-,+,-),
		getpad(+,-),
		getpad(+,+,-),
		getpad(-,-,+,-),
		padout(+),
		padout(+,+,+),
		padout(+,+,+,-,-),
		praggl(+,+,+,+),
		wf_char(+,-),
		writelots(?,+),
		writef_nonlist(+,-),
		writefs(+,+).



			% Print (therefore use pretty printing) onto
			%  the terminal (no-one uses this routine).

ttyprint(X) :-		% fwritef(user, '%p', [X])
	telling(Old),
	tell(user),
	print(X),
	tell(Old).

*/

			% Print a list, one element per line

format(Format,Xs):-writef(Format,Xs).

prlist([]) :- !.
prlist([Head|Tail]) :-
	tab(4), print(Head), nl,
	prlist(Tail).



			% Print a conjunction, one element per line

prconj(true) :- !.
prconj(&(A,B)) :-
	prconj(A), !,
	prconj(B).
prconj((A,B)) :-
	prconj(A), !,
	prconj(B).
prconj(A) :-
	tab(4), print(A), nl.



			% Pretty print a simple logical expression
			%  This is done by first printing the logical
			%  structure using X1 X2 etc to name the components
			%  and then printing the 'values' of X1 X2 etc on
			%  separate lines.

prexpr(Expr) :-
	prexpr(Expr, 1, _, Elements, []),
	nl, write('  where :'), nl,
	prexpr(Elements, 1).


prexpr(Term, Nin, Nout, Elements, Z) :-
	prexpr(Term, Conn, A, B), !,
	put(40), prexpr(A, Nin, Nmid, Elements, Rest),
	put(32), put(Conn),		%  40 is "("
	put(32), prexpr(B, Nmid, Nout, Rest, Z),
	put(41).			%  41 is ")"
prexpr(Term, Nin, Nout, [Term|Z], Z) :-
	succ(Nin, Nout),
	put(88), write(Nin).		%  88 is "X"


	prexpr(&(A,B),	38, A, B).	%  38 is "&"
	prexpr(#(A,B),	35, A, B).	%  35 is "#"
	prexpr((A,B),	38, A, B).	%  38 is "&"
	prexpr((A;B),  124, A, B).	% 124 is "|"


prexpr([Head|Tail], M) :-
	write('    X'), write(M), write(' =  '),
	print(Head), nl,
	succ(M, N), !,
	prexpr(Tail, N).
prexpr([], _).


			% Formatted write utility
			%  This converts the format atom to a string and
			%  uses writefs on that. Note that it fails back over
			%  itself to recover all used space.

fwritef(File, Format) :-
	fwritef(File, Format, []).

fwritef(File, Format, List) :-
	telling(Old),
	tell(File),
	writef(Format, List),
	tell(Old).

writef(Format) :-
	writef(Format, []).


writef(Format, Item) :-
	writef_nonlist(Item, List), !,
	writef(Format, List).
writef([F|String], List) :-
	writefs([F|String], List),
	fail.
writef(Format, List) :-
	atom(Format),
	name(Format, Fstring),
	writefs(Fstring, List),
	fail.
writef(_, _).


writef_nonlist([], _) :- !, fail.
writef_nonlist([_|_], _) :- !, fail.
writef_nonlist(Item, [Item]).



			% Formatted write for a string (ie a list of
			%  character codes).

writefs([], _).

writefs([37,A|Rest], List) :-		%   %<action>
	wf_act(A, List, More), !,
	writefs(Rest, More).

writefs([37,D|Rest], [Head|Tail]) :-	%   %<columns><just>
	48 =< D, D =< 57,		%   48 is "0", 57 is "9"
	getpad(Size, Just, [D|Rest], More),
	padout(Head, Size, Just), !,
	writefs(More, Tail).

writefs([92,C|Rest], List) :-		%   \<special>
	wf_char(C, Char),
	put(Char), !,
	writefs(Rest, List).

writefs([92|Rest], List) :-		%   \<character code in decimal>
	getcode(Char, Rest, More),
	put(Char), !,
	writefs(More, List).

writefs([Char|Rest], List) :-		%   <ordinary character>
	put(Char), !,
	writefs(Rest, List).



wf_act( 99, [Head|Tail], Tail) :-	%   Conjunction
	nl, !, prconj(Head).

wf_act(100, [Head|Tail], Tail) :-	%   Display
	display(Head).

wf_act(101, [Head|Tail], Tail) :-	%   Expression
	nl, !, prexpr(Head).

wf_act(102, List, List) :-		%   Flush
	flush.

wf_act(103, [Head|Tail], Tail) :-	%   aGglutinated
	functor(Head, F, N),
	praggl(1, N, F, Head).

wf_act(105, [Format,List|Tail], Tail):-	%   Indirect
	writef(Format, List).

wf_act(108, [Head|Tail], Tail) :-	%   List
	nl, !, prlist(Head).

wf_act(110, [Char|Tail], Tail) :-	%   iNteger (character)
	put(Char).

wf_act(112,  [Head|Tail], Tail) :-	%   Print
	print(Head).

wf_act(113, [Head|Tail], Tail) :-	%   Quoted
	writeq(Head).

wf_act(114, [Thing,Times|Tail],Tail) :-	%   Repeatedly
	writelots(Times, Thing).

wf_act(115, [Head|Tail], Tail) :-	%   String
	padout(Head).

wf_act(116, [Head|Tail], Tail) :-	%   Term
	print(Head).

wf_act(118, List, List) :-		%   numberVars
	numbervars(List, 0, _).

wf_act(119, [Head|Tail], Tail) :-	%   Write
	write(Head).

wf_act(120, [_|Tail], Tail).		%   X (skip)




wf_char( 37, 37).		%  %
wf_char( 92, 92).		%  \
wf_char( 98,  8).		%  Backspace
wf_char(101, 27).		%  Escape
wf_char(102, 12).		%  Formfeed
wf_char(108, 10).		%  Linefeed
wf_char(110, 10).		%  Newline (=LF on UNIX)
wf_char(114, 13).		%  Return
wf_char(116,  9).		%  Tab



getcode(Char) -->
	getdigits(3, Digits), !,
	{   Digits \== [], name(Char, Digits),  Char < 128   }.

getdigits(Limit, [Digit|Digits]) -->
	{   Limit > 0   },
	[Digit],	{   48 =< Digit, Digit =< 59   },
	{   succ(Fewer, Limit)   }, !,
	getdigits(Fewer, Digits).
getdigits(_, []) --> [].


writelots(N, T) :-
	N > 0,
	write(T),
	succ(M, N), !,
	writelots(M, T).
writelots(_, _).


%   praggl(ArgNo, Arity, Func, Term)
%   prints the arguments of the term one after the other, starting with
%   argument ArgNo.  Arguments are separated by " Func ".  This is meant
%   mainly for ASA, but should be generally useful.

praggl(N, N, _, Term) :- !,
	arg(N, Term, Arg),
	print(Arg).
praggl(L, N, F, Term) :-
	arg(L, Term, Arg),
	print(Arg),
	put(32), write(F), put(32),
	succ(L, M), !,
	praggl(M, N, F, Term).


/*  The new formats are %nC, %nL, and %nR for centered, left, and right
    justified output of atoms, integers, and strings.  This is meant to
    simplify the production of tabular output when it is appropriate.
    At least one space will always precede/follow the item written.
*/

getpad(Size, Just) -->
	getdigits(3, Digits),	{   name(Size, Digits)   },
	[Char],			{   getpad(Char, Just)   }.

	getpad(114, r).		%  right justified
	getpad(108, l).		%  left justified
	getpad( 99, c).		%  centered
	getpad( 82, r).		%  right justified
	getpad( 76, l).		%  left justified
	getpad( 67, c).		%  centered


				%   padout(A,S,J) writes the item A in a
				%   field of S or more characters, Justified.

padout(Atom, Size, Just) :-
	atomic(Atom),
	name(Atom, Name), !,
	padout(Name, Size, Just).
padout(String, Size, Just) :-
	length(String, Length),
	padout(Just, Size, Length, Left, Right),
	tab(Left),
	padout(String),
	tab(Right).

				%   padout(Just,Size,Length,Left,Right)
				%   calculates the number of spaces to put
				%   on the Left and Right of an item needing
				%   Length characters in a field of Size.

padout(l, Size, Length, 0, Right) :- !,
	plus(Excess, Length, Size), 
	getpad(Excess, 1, Right).
padout(r, Size, Length, Left, 0) :- !,
	plus(Excess, Length, Size),
	getpad(Excess, 1, Left).
padout(c, Size, Length, Left, Right) :-
	plus(Excess, Length, Size),
	is(Prefix, div, Excess, 2),
	plus(Remainder, Prefix, Excess),
	getpad(Prefix, 1, Left),
	getpad(Remainder, 1, Right).


				%   getpad(A,B,Max) returns the maximum.

getpad(A, B, A) :- A >= B, !.
getpad(_, B, B).


				%   padout(Str) writes a string.

padout([Head|Tail]) :-
	put(Head), !,
	padout(Tail).
padout([]).


succ(N,N1):-N1 is N+1.
plus(X,Y,Z):-Z is X+Y.
