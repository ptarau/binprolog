a(Y):-X=(println(Y),X),X.

repeat(Goal):-Gs=(Goal,Gs),Gs.

go:-repeat(println(hello)).

/*

revisiting occur check

Lemma 1:

If infinite term unification fails then
unification with occur check fails to.

Corollarry:

In a Prolog supporting infinite terms, occur check
only needs to be performed when infinite
term unification succeeds.

Lemma 2:

Assume infinite term unification is implemented with
value trailing. Assume T0 is the top of the trail
before infinite term unification is started on
two terms. 

Let T1 be the top of the trail after the
unification has succeded. If a pattern like

X=f(... X ...)

has occured in the unification process
then X is reachable from at least one
value trail cell between T0 and T1.

Proof: Infinite term unification will not
stop unless when f(..X ..) = f(..X..) shows up
at some point, one of the two references to f
is made to point to the other - which triggers
value trailing.
=> wrong - X and f(...X...) could be in the
same segment

=> a fast (how fast?) unification with occure check
algorithm seem to be possible to be implemented by
checking for cycles from the value trailed cells after
successful unifications.


*/
