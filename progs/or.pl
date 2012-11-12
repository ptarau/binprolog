% TO BE PORTED to the Tcl/Tk environment

p:-compile('or.pl').
% p:-[-po7ok]. % user can enter new processes on the fly

max(16,16).

:-op(600,xfx,<=).
:-op(900,yfx,:).

% the process dies if it has no room for expansion
% generous neighbours take care of it and its childs
% it must pack itself and give its place
% to its best (youngest,oldest,richest etc.) neighbour 
% processes fight for a processor: the lighter process wins


scr_clear:-max(L,_),Max is L+4,for(_,0,Max),nl,fail; true.

scr_send(p(L0,C0),Char):-
  L is L0+1, C is C0+1,
  put(27),
  cwrite('['),cwrite(L),
  cwrite(';'),cwrite(C),
  cwrite('H'),
  put(Char).

scr_rec(_):-fail.

dir_depl(0,p( 0, 1)).   % right -77
dir_depl(1,p( 1, 0)).   % down -80
dir_depl(2,p( 0,-1)).   % left -75
dir_depl(3,p(-1, 0)).   % up -72

usr_dir(-77,0).
usr_dir(-80,1).
usr_dir(-75,2).
usr_dir(-72,3).
usr_dir(27,0):-scr_clear,abort.

next(Dir,p(L1,C1),p(L2,C2)):-
	max(MaxL,MaxC),
  dir_depl(Dir,p(DL,DC)),
  L2 is L1+DL,C2 is C1+DC,
  L2>=0,L2<MaxL,C2>=0,C2<MaxC.

move(Char,P1,P2):-
        scr_send(P1,32),
        scr_send(P2,Char).      

ranperm(Xs,Ps):-ranperm(Xs,Ps,_).

ranperm([],[],0):-!.
ranperm([X|Xs],Zs,Max):-
        ranperm(Xs,Ys,Max1),
        Max is Max1+1,
        random(Max,N),
        ins(N,X,Ys,Zs).

ins(0,X,Ys,[X|Ys]):-!.
ins(K,X,[Y|Ys],[Y|Zs]):-
        K1 is K-1,
        ins(K1,X,Ys,Zs).

random(Max,R):-
	random(N),
	R is N mod Max. 

randir(Dir):-ranperm([0,1,2,3],Perm),member(Dir,Perm).

super(P1,P2) -->
        {scr_rec(C)},
        serve(C,P1,P2),
        !.
super(P,P) --> [].

usr_depl(Dir,S,U1,U2):-
        next(Dir,U1,U2),
        free(U2,S),
        !,
        move(1,U1,U2).

serve(C,U1,U2,S,S):-			% move cursor
        usr_dir(C,Dir),
        !,
        usr_depl(Dir,S,U1,U2).
serve(13,U1,U2,S1,S3):-			% move process near cursor
        usr_depl(_,S1,U1,U2),
        deq(obj(N,P,Code),S1,S2),
        move(N,P,U1),
        !,
        enq(obj(N,U1,Code),S2,S3).

serve(32,U1,U2,S1,S2):-			% start new process
	usr_depl(_,S1,U1,U2),
	scr_send(p(18,0),63),write('-- '),read(G),
	[New]="0",
	!,
	enq(obj(New,U1,G<=G:[]),S1,S2),
	scr_send(U1,New).

empty(0:_).

enq(X,K:Xs-[X|Qs],K1:Xs-Qs):-K1 is K+1.

deq(X,K1:[X|Ys]-Qs,K:Ys-Qs):-K1>0,K is K1-1.

sel(X) --> deq(X).
sel(X) --> deq(Y),sel(X),enq(Y).

free(_,S):-empty(S),!.
free(P,S1):-deq(obj(_,OtherP,_),S1,S2),P\==OtherP,!,
        free(P,S2).
     
sked(_,_,[],S,S):-!.
sked(N,P,[G|Gs],S1,S2):-
        N1 is N+1,
        enq(obj(N1,P,G:Gs),S1,S2),
        scr_send(P,N1).

reduce(_,obj(N,P,Answer<=true:Gs)) --> !,	% broadcast answer
	out(N,P,Answer),
	sked(N,P,Gs).
reduce(_,obj(N,P,A<=G1:[])) --> !,		% fork
	{ scr_send(P,32),
		findall(A<=G2,G1<=G2,Gs)
	},
	sked(N,P,Gs).
reduce(U,obj(N,P,G:[G1|Gs])) -->		% place its children
	find_place(U,P,Q),!,
	{scr_send(Q,N)},
	enq(obj(N,P,G:Gs)),
	enq(obj(N,Q,G1:[])).
reduce(_,obj(N1,P1,G1:[G|Gs1])) -->		% fight for a processor
	sel(obj(N2,P2,X2)),
	{ randir(Dir),
	  next(Dir,P1,P2),
	  compose_proc(obj(N1,P1,G1:[G|Gs1]),obj(N2,P2,X2),Winner,Looser)
	},
	!,
	assimilate_proc(Winner,Looser).
	
compose_proc(	obj(N1,P1,G1:Gs1),obj(N2,P2,G2:Gs2),
							obj(N1,P1,G1:Gs1),obj(N2,P2,G2:Gs2)):-
	length(Gs1,L1),length(Gs2,L2),
	scr_send(p(20,0),62),
	put(N1),put(58),write(L1),write( ' against '),
	put(N2),put(58),write(L2),write('   '),
	L1=<L2, % lightest is better
	!.
compose_proc(O1,O2,O2,O1).

assimilate_proc(obj(N1,P1,G1:Gs1),obj(N2,P2,G2:Gs2)) -->
	{ det_append(Gs1,[G2|Gs2],Gs3),
	  scr_send(P2,32)
	},
	!,
	enq(obj(N1,P1,G1:Gs3)).

find_place(UserPos,OldP,NewP,S,S):-
        randir(Dir),
        next(Dir,OldP,NewP),
        free(NewP,S),
        NewP\==UserPos.

demo(U1,S1):- 
        new_state(U1,U2,S1,S2),
        !, 
        demo(U2,S2).

new_state(U1,U2)-->
	 deq(O),
   reduce(U1,O),
   super(U1,U2).

solve(G):-
  scr_clear,
	max(MaxL,MaxC),
	L is MaxL // 2,
	C is MaxC // 2,
	P=p(L,C),
	[N]="0",
	User=p(0,0),scr_send(User,1),
  scr_send(P,N),
  enq(obj(N,P,G<=G:[]),0:Q-Q,S1),
	demo(User,S1).

out(N,P,X,S,S):-
        scr_send(P,32),scr_send(p(21,0),N),write('=>'),write(X),nl,
        S=(Len:_),write('active processes':Len),
        statistics(global_stack,L),
        write(-L).

% data

app([],A,A,B)<=B.
app([C|D],E,[C|F],G)<=app(D,E,F,G).

nrev([],[],H)<=H.
nrev([I|J],K,L)<=nrev(J,M,app(M,[I],K,L)).

perm([],[],N)<=N.
perm([O|P],Q,R)<=perm(P,S,ins(O,S,Q,R)).

ins(T,U,[T|U],V)<=V.
ins(W,[X|Y],[X|Z],X0)<=ins(W,Y,Z,X0).

rperm(X,W,Cont)<=nrev(X,Y,perm(Y,Z,nrev(Z,W,Cont))).
	
xperm(X,Y,Cont)<=rperm(Y,X,Cont).

i1:-G=xperm(R,S,true),solve(G).
i2:-G=app(X,Y,Z,true),solve(G).

go:-write('use bp -h3000'),nl,
    L=[1,2,3,4,5],G=rperm(L,R,true), solve(G).

bad:-G=rperm(L,R,true), solve(G).

bm:-G=nrev([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18],X,true), 
     time((solve(G)),T),L is (19*20*1000)//(2*T),pp(time:T+lips:L).
bp:-G=perm([1,2,3,4,5 ],R,true),
        time(solve(G),T),pp(perm:T).

time(G,_):-statistics(runtime,_),G,fail.
time(_,T):-statistics(runtime,[_,T]).
