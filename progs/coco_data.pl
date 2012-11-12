

test:-
  init,         % creates prototypes `unique' and `cloneable'
  prelude,      % specific prelude
  login(wizard),clone(song),
   take(song,How),
   dig(room),
   go(room),
   drop(song,How),
   logout,listing,
   fail.

go:-
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
  

