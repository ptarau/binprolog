/*

ISO:
catch(Goal,Ball,Do)

	execute Goal
	on throw(Term): look for closest catch
			copy Term
			undo bindings until call to catch; remove choices
			unify with Ball
				if fail: throw Ball again
				if succeed: call Do, continuation of catch goal


catch(Goal,Ball,Do,Cont) ::- c4(Goal,Ball,Do,Cont,Cont) .
catch(_,_,_) :- fail .

c4(Goal,Ball,Do,Cont) :- get_neck_cut(Choice) ,
		(catchmarker(Ball,Do,Choice,Cont) -:: dogoal(Goal)) .

dogoal(Goal) :- Goal.

throw(Term) :-	copy_term(Term,Copied) ,
		catchmarker(Ball1,Do1,Choice,Cont1) , ! ,
		Ball1 = Ball ,
		Do1 = Do,
		Cont1 = Cont ,
		_Copied1 = Copied ,
		untrail_to(Choice) ,
		(Ball = Copied -> call2(Do,Cont) ; throw(Term)) .

call2(Do,NewCont,_) ::- call(Do,NewCont) .
*/

/* non exhaustive tests follow */

l :- [catch4] .


a :- catch(b,X,write(ball(X))) , write(7) .

b :- throw(1) .

c :- catch(d,X,write(ball(X))) , write(7) , nl, fail .

d .
d :- throw(66) , write(oei) .
d .

f :- catch(g,X,write(h(X))) , write(99) , nl , fail .

g :- catch(h,3,write(c(3))) , write(bla) , nl .

h :- throw(4) .
h :- write(h2) .


k :- catch(s,X,t(h(X))) , write(99) , nl, fail .
m :- catch(s,X,t(h(X))) , write(99) , nl , ! , fail .

s :- throw(137) .

t(X) :- write(X) , nl .
t(X) :- write(2-X) , nl .


u :- catch(g(X),B,write(f(B,X))) .

g(X) :- X = 2 , throw(X) .
