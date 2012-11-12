/* facts */
likes(joe,beer).
likes(mary,wine).
likes(bill,water).
likes(bill,wine).

/* rules */
drinks(X,Drink):-likes(X,Drink).
drinks(_,Drink):-likes(mary,Drink).

/* projection */
person(X):-likes(X,_).

/* join */
dry(X):-person(X), not(likes(X,beer)),not(likes(X,wine)).

water_drinker(X):-drinks(X,water).

