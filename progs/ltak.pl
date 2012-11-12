:-write('Program: ltak.pl'),nl.
:-write('Author: Paul Tarau'),nl.
:-write('tak program with constant acces and create-time lemmas'),nl.

tak(X,Y,Z,A) :- X =< Y, !, Z = A.
tak(X,Y,Z,A) :-
        X1 is X - 1,    
        Y1 is Y - 1,    
        Z1 is Z - 1,    
        ltak(X1,Y,Z,A1), 
        ltak(Y1,Z,X,A2), 
        ltak(Z1,X,Y,A3), 
        ltak(A1,A2,A3,A).

ltak(X,Y,Z,A):-
	tak_encode(X,Y,XY),
	tak_lemma(XY,Z,tak(X,Y,Z,A),A).

tak_encode(Y,Z,Key):-Key is Y<<16 \/ Z.                
tak_decode(Key,Y,Z):-Y is Key>>16, Z is Key <<17>>17 .

%optimized lemma <P,I,G> --> O (instantiated executing G)
tak_lemma(P,I,_,O):-val(P,I,X),!,X=O.
tak_lemma(P,I,G,O):-G,!,def(P,I,O).

go:-    statistics(runtime,_), 
        tak(24,16,8,X),
        statistics(runtime,[_,T]),statistics, 
        write('BMARK_ltak:'=[time=T,tak=X]), nl.

