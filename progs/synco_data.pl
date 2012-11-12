go:-schedule(top).

top:-
  init,         % creates prototypes `unique' and `cloneable'
  prelude,      % specific prelude
  login(wizard),
   take(song,How),
   dig(room),
   craft(song),
   go(room),
   drop(song,How),
   logout,listing,
   fail.

go1:-
  init,         % creates prototypes `unique' and `cloneable'
  prelude,      % specific prelude
  login(wizard),
   dig(room),
   go(room),
   craft(flower),
   move(flower,lobby),
  logout,
  login(mary),
   clone(poem),
   take(poem,Cloneable),
   go(room),
   drop(poem,Cloneable),
  logout,
  listing,
fail.
  

