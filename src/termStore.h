/* 
  see term.h for external Term representations
  and operations on them
*/

/*
  THESE OPERATIONS WILL BE PROVIDED
  BY THE EXTERNAL TERM STORE.
*/

/* adds a Term to the Term store as the FIRST term associated to a key
   - multiple values can be associated to a key 
   as a key is a compound Term its toplevel subterms can be seen as independent
   subkeys to which the same reference to the value Term should be associated 
*/
void pushTerm(Term key, Term value);
/* adds a Term to the Term store as the LAST value assocuated to a key
   - multiple values can be associated to a key 
   as a key is a compound Term its toplevel subterms can be ssen as independent
   subkeys to which the same reference to the value Term should be associated 
*/
void putTerm(Term key, Term value);

/* creates a new iterator returning Terms associated to a key*/
ulong newIterator(Term key);

/* closes a given iterator and frees all the resources it uses */
void closeIterator(ulong iterator);

/* checks if there's at least one Term associated to a key, without
  necessarily creating an iterator for that - should be a fast operation 
*/
BYTE hasTerms(Term key);

/* iterator that retrieves and possibly removes the next Term from the Term store 
   it returns NULL when no more terms are found. If it detects that all terms
   associated to a key have been removed, it frees all the resource relate to
   the key on which the iterator operates.
*/
Term getNextTerm(ulong iterator);

void removeCurrentTerm(ulong iterator);

void updateCurrentTerm(ulong iterator,Term term);

/* deletes all terms associated to a key and ensures
   that the memory used by them is ALL reclaimed 
*/
void deleteAllTerms(Term key);

/*
   returns the number of Terms currently
   associated to a key;
*/
bp_long countTerms(Term key);

/* creates an iterator enumerating one key at a time */
ulong newKeyIterator();

/* allows Prolog to call various user defined 
   Term->Term functions for each OpCode=0,1,...
   The functions will:
     - return NULL to indicate failure to Prolog
     - call freeTerm their input argument if it is not needed further
   The interface will free the returned Term after internalizing it.
*/
Term processTerm(bp_long OpCode,Term argument);

/* returns a new (possibly UNIQUE!) string */
char *newTermString(char *s);

/* frees a string created with newTermString - possibly implementing 
   a reference counting mechanism such that a given UNIQUE string
   is only freed when no reference to it exists
*/
void freeTermString(char *s);
