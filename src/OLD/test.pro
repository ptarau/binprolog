a(1).
a(2).
b(X):-a(X).

test(N):-for(I,1,N),remote_run(eq(_,I)),fail.
test(_).

go(N):-ctime(T1),test(N),ctime(T2),'is'(T,'-'(T2,T1)),println(T).

go:-go(20).

