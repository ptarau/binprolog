:-module(tree,
    [
      ppt/1
    ]).



/*
From fornax!nntp.cs.ubc.ca!cyber2.cyberstore.ca!math.ohio-state.edu!howland.reston.ans.net!europa.eng.gtefsd.com!uunet!newsgate.watson.ibm.com!watnews.watson.ibm.com!hawnews.watson.ibm.com!syllog2!neumann Tue Dec 21 23:45:21 PST 1993

In article <1993Dec20.154218.7769@news.unige.ch> from [Mon, 20 Dec 1993 15:42:18 GMT] you wrote:
 |> Does anyone have code for visually representing trees?
 |> Something of the form showtree(root(branch1,branch2,branch3))
 |> which prints
 |> 
 |> 			    root
 |> 		_____________|_____________
 |> 	       /	     |             \
 |> 	    branch1       branch2        branch3
 |> 
 |> 
 |> or similar.
 
 Below is a program for printing Prolog terms as trees on typewriter
 output devices (eg. ASCII terminals). I have posted this program here
 some time ago, here it comes again with some minor modifications. The
 program assumes no Prolog libraries and works at least under SISCtus and
 Quintus Prolog. I will put it later this day on 
 
    ftp.wu-wien.ac.at:pub/src/Languages/Prolog/asciitree.pl
 
 A graphical version, which is much more elaborated (various tree layouts,
 arc-types etc) can be found in the same directory in ptd.tar.gz. It has
 backends for xwip and tgif as they existed in April 91. Some changes
 may be necessary.
 
 Please no complaints about spacing between arguments.
 -gustaf

==========================================================================

   printing Prolog terms in a tree layout for typewriter output.

   Written in Spring 1985 -- Gustaf Neumann 

   (c)1991 Gustaf Neumann, Wirtschaftsuniversitaet Wien,
      Augasse 2-6, 1090 Vienna, Austria *222/31 336-4533. 
      email:  neumann@wu-wien.ac.at

   Permission is granted to use, copy and distribute this program as long 
   (1) this note remains intact, 
   (2) no fees are charged and 
   (3) no further restrictions are imposed.

   ppt/1 ..... layouts and prints the term given as argument
   ppt/2 ..... second argument can be set to the atom 'arc' or 'noarc'
               depending on whether or not tree arcs should be printed

   The following predicates are not defined within this program:
   -   length(List,Length), 
   -   tab(Exp)

   Do not try to print infinite trees :-)

   To show, what this program does, issue the goal: examples. 
*/

:-op(100,xfy,::).

examples:- example(X), ppt(X), nl, nl, write(X), nl,
	wait_for_input, fail.
examples.

example(sin(alpha)/cos(beta-phi)+cos(beta)*tan(360-alpha)).
example(buch(titel(wirschaftsinformatik1),autor(hans_robert, hansen))).
example((a:-b,c,d)).
%example((ppt(X,Y):-Body)):- clause(ppt(X,Y),Body).
example(sentence(np(proper(gustaf)),vp(v(likes),np(proper(prolog))))).
example(sentence(np(det(that),n(man),rel(that,vp(iv(whistle)))),
           vp(tv(tunes),np(det(nil),n(pianos),rel(nil))))).
example(wirtschaftsinformatik(leitung(hans_robert),
           sekretariat(virly,anita),
           assistenten(lore,rony,goeha,gu,margret,andy,stessi))).


/************************************************************************
*                      top level predicate ppt/1                        *
************************************************************************/
ppt(Term):- ppt(Term,arc).
ppt(Term,Arc) :-
      number_vars(Term,0,_),          /* ground all variables in Term     */
      pos(Term,Pos,C,0-Right)->       /* compute hor. positions of nodes  */
      inv([Pos],[]::_,H::T,s)->      /* invert structure for printing    */
      posdiff(-(72-Right)//2,0,Tab),  /* compute hor. tab for centering   */
      print_tree(H::T,[C],Tab,Arc)->true.
                                      /* print tree in line printer mode  */

/************************************************************************
*                      Compute Positions of Nodes                       *
************************************************************************/
pos(Head,t(Head,Rel,L,[],0)-[], Nc, N0-Nn):-     /* leaf node         */
     atomic(Head), !,
     string_length(Head,L), Nn is N0+L,
     Rel is L//2,                                /* middle of the node */
     Nc  is (N0+Nn)//2.                          /* center over node   */
pos(X,t(Head,Rel,L,Centers,Adj)-A, Nc, N0-N2):-  /* non-leaf node      */
     X =.. [Head|Args],
     pos_list(Args,A,Centers,N0-N1),
     string_length(Head,L), posdiff(N1-N0,L,Error),
     Adj is (Error+((N1-N0) mod 2))//2,
     N2 is N1+Error,
     Rel is L//2,                                /* middle of the node */
     Nc  is (N0+N2)//2.

pos_list([],   [],      [],         N-N).
pos_list([H],  [A],     [Center],   N-N1) :- !, pos(H,A,Center,N-N1).
pos_list([H|T],[A|Args],[C|Centers],N0-Nn):-
     pos( H,    A,       C,         N0-N1),
     N2 is N1+2, pos_list(T,Args,Centers,N2-Nn).


posdiff(Expr,L,Adj) :- X is Expr, Adj is L-X, Adj > 0, !.
posdiff(_,_,0).

/************************************************************************
*                              invert tree                              *
************************************************************************/
inv([Node-Sons|Brothers], List::Deep, [Node|List1]::Deep2, _) :-
     inv(Brothers, List::Deep, List1::Deep1, b),
     inv(Sons, Deep1, Deep2, s).
inv([], []::[], [], s).
inv([], []::[], []::_, b).
inv([], E, E, _).

/************************************************************************
*                              print tree                               *
************************************************************************/
print_tree([],[],_,_).
print_tree(Node::Deep, Centers, Tab, Arc) :-
     tab(Tab), print_list(Node,0,Centers,Cd), nl,
     (   Arc  == noarc
      ;    Deep == []
      ;    tab(Tab), marks(Centers,Node,0),
           tab(Tab), horarc(Node,0,_), nl,
           tab(Tab), marks(Cd,0)
     ),!,
     print_tree(Deep,Cd,Tab,Arc).

print_list([],_,[],[]).
print_list([t(H,Rel,L,Cd,Adj)|R], P0, [C|Centers], Ca) :-
     P is C-Rel, tab(P-P0), write(H), Pn is P+L,
     print_list(R,Pn,Centers,Cr),
     add_to(Cd,Adj,Cda), 
     local_append(Cda,Cr,Ca).

/************************************************************************
*                              draw arcs                                *
************************************************************************/
marks([],[],_) :- nl.
marks([H|T],[t(_,_,_,[],_)|R],E) :- !, tab(H-E), write(' '),marks(T,R,H+1).
marks([H|T],[_|R],            E) :-    tab(H-E), write('|'),marks(T,R,H+1).

marks([],_) :- nl.
marks([H|T],E) :- tab(H-E), write('|'), marks(T,H+1).

horarc([], A,A).
horarc([t([],_,_,_,_  )|R],P,P2) :- !, horarc(R,P,P2).
horarc([t(_,_,_,Cd,Adj)|R],P,P2) :-    line(Cd,Adj,P,P1), horarc(R,P1,P2).

line([],   _,E,P) :- P is E.
line([H],  A,E,P) :- !, tab(H+A-E), write('.'), P is H+A+1.
line([H|T],A,E,P) :-    tab(H+A-E), write('.'), line_([H|T],A,H+A+1,P).

line_([],      _,E,P) :- P is E.
line_([H],     A,E,P) :- line_to(H+A-E), P is H+A+1.
line_([_,T|Tt],A,E,P) :- line_to(T+A-E), write('.'), line_([T|Tt],A,T+A+1,P).

line_to(Exp)  :- L is Exp, line_to_(L,'-').
line_to_(L,_) :- L < 1.
line_to_(L,C) :- L >= 1, write(C), L1 is L-1, line_to_(L1,C).

add_to([],_,[]).
add_to([H|T],A,[Ha|Ta]) :- Ha is H+A, add_to(T,A,Ta).

/************************************************************************
*                       misc utility predicates                         *
************************************************************************/

wait_for_input. % :- get0(_).

% some of the following predicates are builtins or available from
% libraries in some prolog implementations

string_length(X,L):- atomic(X), name(X,S), length(S,L).

% use numbervars/3, if it's a builtin
number_vars(Term,N0,N1) :- 
	var(Term), !, 
	name(N0,Digits), name('V',[C]), 
	name(Term,[C|Digits]),
	N1 is N0+1.
number_vars(Term,N0,N0) :- 
	atomic(Term), !.

number_vars(Term,N0,N1) :- 
	Term =.. [_|Args], 
	number_list(Args,N0,N1).

number_list([],N0,N0).
number_list([H|T],N0,N2) :- number_vars(H,N0,N1), number_list(T,N1,N2).

% to avoid conflicts with builtin append in Quintus 
local_append([],L,L).
local_append([X|A],B,[X|C]) :- local_append(A,B,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% hint how to use
tree_test:- nl,
   write('       Issue the goal ''tree:examples'' to see a few trees!'), 
   nl, nl.
/*
==========================================================================
--
Gustaf Neumann                     neumann@watson.ibm.com
Postdoctoral/Visiting Scientist    Tel: (914) 784 7086
IBM T.J.Watson Research Center, P.O.Box 704
Yorktown Heights, New York 10598
*/

:-module(user).

/* Adapted for BinProlog's restricted is/2 
   and made a library module for bp3.32 by Paul Tarau 1995
*/

