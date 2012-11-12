#include <string.h>

#define BAD_TYPE 0
#define STRING_TYPE 1
#define INT_TYPE 2
#define FLOAT_TYPE 3
#define VAR_TYPE 4
#define TERM_TYPE 5

#define LAST_TYPE TERM_TYPE

#define TRUE 1
#define FALSE 0

typedef unsigned char BYTE;
typedef cell ulong;
typedef void *OBJECT;

typedef struct Term {
  bp_long size;
  BYTE *types;
  OBJECT *args;
} *Term;

/* EXTERNAL REPRESENTATION OF Prolog Terms */

/* creates a new compound term with given arity - to be filled
   in with arguments using setArg
*/
Term newTerm(bp_long arity);

/* creates a new internal representation for a string */
OBJECT S2O(char *s);
/* creates a new internal representation for a bp_long int */
OBJECT I2O(bp_long l);
/* creates a new internal representation for a double float */
OBJECT F2O(double x);
/* creates a new internal representation for a var */
OBJECT V2O(ulong v);
/* casts a Term to OBJECT, usually to store it as an argument */
OBJECT T2O(Term T);

/* extracts a string from an OBJECT */
char *O2S(OBJECT O);
/* extracts a bp_long int from an OBJECT */
bp_long O2I(OBJECT O);
/* extracts a double float from and OBJECT */
double O2F(OBJECT O);
/* extracts a var from an OBJECT */
ulong O2V(OBJECT O);
/* extracts a Term from an OBJECT */
Term O2T(OBJECT T);

/* extends a List of strings (NULL if empty) with a new first element */
Term newStringList(char *head, Term tail);
/* creates a list terminator corresponding to [] in Prolog */
Term NIL();
/* recognizes the list terminator */
BYTE isNIL(Term T);

/* frees memory allocated to term and its components */
void freeTerm(Term T);

/* sets argument i of T to value arg of type termType */

BYTE setArg(bp_long i,Term T,BYTE termType,OBJECT arg);

/* returns the type of an argument */
BYTE getType(bp_long i,Term T);

/* returns an argument as a generic pointer to be casted according to its type */
OBJECT getArg(bp_long i,Term T);

/* returns the arity of a Term, argument being in 0..arity */
bp_long getArity(Term T);

/* 
  compares two ground terms by scanning them recursively
  and taking into accoun the comparison value of their 
  simple components (string, ints, doubles)
  returns -1 if T1<T, 0 if T1==T2 and 1 if T1>T2

  in the special case that T1==T2 this is guaranted to return 0
*/
bp_long compareTerms(Term T1,Term T2);

/*  
  returns a comparison compatible hash value for a Term,
  i.e. if 0==compareTerms(T1,T2) then hashCode(T1)==hashCode(T2)
*/
bp_long hashCode(Term T);

/* Creates a string representing a Term.
   in the buffer provided by the user. The
   user is responsable for providing a
   large enough buffer.
*/
void toString(Term T,char *buffer);

/* prints out a Prolog-like representation of a term */
void showTerm(Term T);

/* prints a Term to a newly allocated string */
char* toNewString(Term T);

/* computes the length of the string the term needs to be printed to */
bp_long stringLengthOf(Term T);


