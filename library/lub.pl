% Some useful progs from Paul Tarau: PhD Thesis, 1990 Univ. de Montreal

% lub & glb

lub(T1,T2,R):-lub_term(T1,T2,R,db([],[],[]),_).

glb(T1,T2,R):-findall(T1,T1=T2,[R]).

% lub of a list of terms
xlub([T|Ts],L):-map_lub(Ts,T,L).

map_lub([],L,L):-!.
map_lub([T|Ts],L1,L3):-
	lub(L1,T,L2),
	map_lub(Ts,L2,L3).

lub_term(T1,T2,Lub,L,L):-atomic(T1),T1=T2,!,Lub=T1.
lub_term(T1,T2,Lub,L1,L2):-
        functor(T1,F1,N),functor(T2,F2,N),F1=F2,!,
        functor(Lub,F1,N),
        lub_args(N,T1,T2,Lub,L1,L2).
lub_term(T1,T2,Var,db(As,Bs,Vs),db(As,Bs,Vs)):-
        assoc(T1,T2,Var,As,Bs,Vs),!.
lub_term(T1,T2,Var,db(As,Bs,Vs),db([T1|As],[T2|Bs],[Var|Vs])).
 
lub_args(0,_,_,_,L,L):-!.
lub_args(I,T1,T2,Lub,L1,L3):-I>0,
        arg(I,T1,A1),
        arg(I,T2,A2),
        arg(I,Lub,A),
        lub_term(A1,A2,A,L1,L2),
        NewI is I-1,
        lub_args(NewI,T1,T2,Lub,L2,L3).

assoc(T1,T2,Var,[A|_],[B|_],[V|_]):-
  T1==A,
  T2==B,!,
  Var=V.
assoc(T1,T2,Var,[_|As],[_|Bs],[_|Vs]):-
  assoc(T1,T2,Var,As,Bs,Vs).

% anticompose

anticompose(A<=B,C<=D,AA<=DD):-
        lub_term(B,C,_Lub,db([],[],[]),db(Bs,Cs,Vs)),
        anti_subst(A,AA,Bs-Vs),
        anti_subst(D,DD,Cs-Vs).

% anti_subst

anti_subst(T,Var,As-Vs):-get_subst(T,Var,As,Vs),!.
anti_subst(T,Gen,_):-atomic(T),!,Gen=T.
anti_subst(T,Gen,_):-var(T),!,Gen=T.
anti_subst(T,Gen,Subst):-
        functor(T,F,N),!,
        functor(Gen,F,N),
        anti_subst_args(N,T,Gen,Subst).

anti_subst_args(0,_,_,_):-!.
anti_subst_args(I,T,Gen,Subst):-I>0,
        arg(I,T,A1),
        arg(I,Gen,A),
        anti_subst(A1,A,Subst),
        NewI is I-1,
        anti_subst_args(NewI,T,Gen,Subst).

get_subst(T,Var,[A|_],[V|_]):-T==A,!,Var=V.
get_subst(T,Var,[_|As],[_|Vs]):-get_subst(T,Var,As,Vs).

% compose

composition(F1,F2,A<=C):-
   copy_term(F1,A<=B),
   copy_term(F2,B<=C).


% arrow2subst

arrow2subst(Gen-Spec,Vs-Bs):-
  subsumes_chk(Gen,Spec),
  lub_term(Gen,Spec,_Lub,db([],[],[]),db(As,Bs,Vs)),
  Vs=As.

% tests

pp(X):-pp_clause(X).

t1:-T1=f(g(a),g(a)),T2=f(b,b),
   T=h(g(g(a)),g(a),g(X),g(X)),
   lub_term(T1,T2,_Lub,db([],[],[]),db(As,_Bs,Vs)),
   anti_subst(T,Gen,As-Vs),
   pp(T:As-Vs),
   pp(Gen).

t2:-Gen=f(A,A,B,B),Spec=f(g(C),g(C),D,D),
   arrow2subst(Gen-Spec,R),pp(Gen-Spec=R).


t3:-T1=f(A,A,g(_,h(_))),
   T2=f(a,a,g(D,D)),
   glb(T1,T2,Glb),
   lub(T1,T2,Lub),
   pp(glb(T1+T2=Glb)),
   pp(lub(T1+T2=Lub)).

t4:-T1=f(A,A,g(_,h(_))),
    T2=f(a,a,g(D,D)),
    glb(T1,T2,Glb),
    lub_term(T1,T2,Lub,db([],[],[]),db(As,Bs,Vs)),
    pp(glb=Glb),
    pp(Lub:As+Bs=Vs),
    arrow2subst(Lub-T1,R),
    pp(Lub-T1=R).

t5:-anticompose(a(1,1)<=b(1,1),b(2,2)<=true,R),pp(R).

t6:-Ts=[f(a,a,B,B),f(X,X,X,X),f(A,A,g(b),g(b)),
        f(1,1,1,1),f(2,2,2,2)],
        xlub(Ts,Lub),
        pp(Lub).

t7:-Ts=[app([1,2],[3,4],[1,2,3,4])<=app([2],[3,4],[2,3,4]),
        app([a],[],[a])<=app([],[],[])
        ],
        xlub(Ts,Lub),
        pp(Lub).
 

test:-t1,t2,t3,t4,t5,t6,t7.

