go:- go(Xs),write(Xs),nl.

	color_map([]).
	color_map([R|Rs]) :-
  		color(red)-:
  		color(yellow)-:
  		color(blue)-:
		color(white)-:
		color_region(R),
		color_map(Rs).

        color_region(region(Color,Neighbors)):-
	   color(Color),
	   color_neighbors(Neighbors).

  
        color_neighbors([]).
        color_neighbors([X|Xs]) :- 
		color(X), 
		color(X)-:color_neighbors(Xs).


go([france=F,belgium=B,holland=H,germany=G,luxembourg=L,
    italy=I,switzerland=S,austria=A]):-
	Rs=
	  [
            region(F,[I,S,B,G,L]),
	    region(B,[F,H,L,G]),
	    region(H,[B,G]),
	    region(G,[F,A,S,H,B,L]),
	    region(L,[F,B,G]),
	    region(I,[F,A,S]),
	    region(S,[F,I,A,G]),
	    region(A,[I,S,G])
          ],
	color_map(Rs).
  

% Linear logic version of map coloring prog. 14.4 from Sterling & Shapiro
