#include "global.h"
#include <string.h>

#ifdef TSTORE

#if !defined(EXTERNAL_TSTORE)
#include "term.h"
#include "termStore.h"

/*
TODO: provide here implementations for the prototypes
described in termStore.h

THE folowing provide STUBS for trying out the API - to
be progressively replaced with the real thing !!!
*/

static Term theTerm=NULL;

void pushTerm(Term key, Term value){theTerm=value;}
void putTerm(Term key, Term value){theTerm=value;}
ulong newIterator(Term key){return ZERO;}
void closeIterator(ulong iterator) {}
BYTE hasTerms(Term key) {return NULL!=theTerm;}
Term getNextTerm(ulong iterator) {return theTerm;}
void removeCurrentTerm(ulong iterator) {theTerm=NULL;}
void updateCurrentTerm(ulong iterator,Term value) {theTerm=value;}
void deleteAllTerms(Term key) {theTerm=NULL;}
bp_long countTerms(Term key) {return 1;}
ulong newKeyIterator(){return ZERO;}
Term processTerm(bp_long OpCode,Term arg){return arg;}
char *newTermString(char *s) {return strdup(s);}
void freeTermString(char *s) {free(s);}
#endif

#endif
