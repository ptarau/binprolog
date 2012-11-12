:-[lattice].

wam(0,[constant,deep,head]).
wam(1,[deep,head,structure]).
wam(2,[deep,head,value]).
wam(3,[deep,head,variable]).
wam(4,[body,constant,deep]).
wam(5,[body,deep,structure]).
wam(6,[body,deep,value]).
wam(7,[body,deep,variable]).
wam(8,[constant,head,top]).
wam(9,[head,structure,top]).
wam(10,[head,top,value]).
wam(11,[head,top,variable]).
wam(12,[body,constant,top]).
wam(13,[body,structure,top]).
wam(14,[body,top,value]).
wam(15,[body,top,variable]).

lcontext(L,Rs):-wam(L,Rs).

