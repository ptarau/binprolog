% benchmark by D. Diaz from the WAMCC distribution
% slightly modified to remove irrelevant output by Paul tarau

go:- 	statistics(runtime,_), 
	ham,
	statistics(runtime,[_,Y]), 
	write('Hamilton benchmark time : '), write(Y), nl.

ham:-cycle_ham([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t],_),fail.
ham.


cycle_ham([X|Y],[X,T|L]):-
	chain_ham([X|Y],[],[T|L]),
	edge(T,X).


chain_ham([X],L,[X|L]).
chain_ham([X|Y],K,L):-
	del(Z,Y,T),
	edge(X,Z),
	chain_ham([Z|T],[X|K],L).

del(X,[X|Y],Y).
del(X,[U|Y],[U|Z]):-
	del(X,Y,Z).

edge(X,Y):-
	connect(X,L),
	el(Y,L).

el(X,[X|_]).
el(X,[_|L]):-
	el(X,L).

connect(a,[b,j,k]).
connect(b,[a,c,p]).
connect(c,[b,d,l]).
connect(d,[c,e,q]).
connect(e,[d,f,m]).
connect(f,[e,g,r]).
connect(g,[f,h,n]).
connect(h,[i,g,s]).
connect(i,[j,h,o]).
connect(j,[a,i,t]).
connect(k,[o,l,a]).
connect(l,[k,m,c]).
connect(m,[l,n,e]).
connect(n,[m,o,g]).
connect(o,[n,k,i]).
connect(p,[b,q,t]).
connect(q,[p,r,d]).
connect(r,[q,s,f]).
connect(s,[r,t,h]).
connect(t,[p,s,j]).



