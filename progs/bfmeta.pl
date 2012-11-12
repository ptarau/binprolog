:-op(600,xfx,<=).

clauses([
	app([],A,A,B)<=B,
	app([C|D],E,[C|F],G)<=app(D,E,F,G),

	nrev([],[],H)<=H,
	nrev([I|J],K,L)<=nrev(J,M,app(M,[I],K,L)),

	perm([],[],N)<=N,
	perm([O|P],Q,R)<=perm(P,S,ins(O,S,Q,R)),

	ins(T,U,[T|U],V)<=V,
	ins(W,[X|Y],[X|Z],X0)<=ins(W,Y,Z,X0)
]).

all_(G,R):-clauses(C),all_instances(G,C,R).

all_instances(Goal,Clauses,Answers):-
        derive_all([Goal<=Goal],Clauses,[],Answers).

% derives all answers until there is no Arrow=(Answer<=Goal) left

derive_all([],_,As,As).
derive_all([Arrow|Fs],Cs,OldAs,NewAs):-
        derive_one(Arrow,Cs,Fs,NewFs,OldAs,As),
        derive_all(NewFs,Cs,As,NewAs).

% if Answer<=true has been deduced then keep answer
% else replace Answer<=Goal with its consequences Answer<=Body
% obtained from input clauses of the form Goal<=Body

derive_one(Answer<=true,_,Fs,Fs,As,[Answer|As]).
derive_one(Answer<=Goal,Cs,Fs,NewFs,As,As):-Goal\==true,
        match_all(Cs,Answer<=Goal,Fs,NewFs).

match_all([],_,Fs,Fs).
match_all([Clause|Cs],Arrow,Fs1,Fs3):- 
        match_one(Arrow,Clause,Fs1,Fs2),
        match_all(Cs,Arrow,Fs2,Fs3).

% basic inference step

match_one(F1,F2,Fs,[F3|Fs]):-compose(F1,F2,F3),!.
match_one(_,_,Fs,Fs).

/*
compose(F1,F2,A<=C):-
	write(F1+F2=before),nl,
        copy_term(F1,A<=B),
	write(after(A<=B)),nl,
        copy_term(F2,B<=C),write(F1+F2=(A<=C)),nl
.
*/

compose(A<=B1,B2<=C,R):-findall(A<=C,B1=B2,[R]).

time(G,T):-statistics(runtime,_),G,!,statistics(runtime,[_,T]).

i1:-G=nrev([a(X),b,c(X)],_,true), all_(G,R),write(R),nl.

i2:-G=app(_,_,[a,b],true), all_(G,R),statistics,write(R),nl.

i3:-G=perm([a,b,c],_,true), all_(G,R),statistics,write(R),nl.
     
integers([],I,I):-!.
integers([I0|L],I0,I):-I0<I,I1 is I0+1,integers(L,I1,I).

bm(N):-
	Len is N+1,
	integers(Is,1,Len),
	G=nrev(Is,_,true), 
     time(all_(G,_),T),
     L is 1000*(N+1)*(N+2)//(2*T),
     statistics, 
     write(time-T+lips-L),nl.
     
go:-bm(20).
