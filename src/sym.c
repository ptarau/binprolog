#include <string.h>
#include "global.h"
extern struct specsyms g;
extern struct limit max;
extern byte *make_byte_array(bp_long a_size);
extern char *make_char_array(bp_long a_size);

string lextable,newlex;
string *atomtable,*make_atomtable(bp_long a_size);
byte *atomstamp;
no newatom;

/*************************** SYMBOL TABLE MANAGEMENT ******************/


extern void fatal_error (string mes);
extern void warnmes (string mes);
extern void bp_halt (int i);

no make_symtable(void)
{ no errctr=0;
  errctr+=!(atomtable=make_atomtable(MAXATOM));
  errctr+=!(atomstamp=make_byte_array(MAXATOM));
  newatom=0;
  errctr+=!(lextable=newlex=make_char_array(MAXLEX));
  return !errctr;
}

static string insert_lex(string from)
{ string currlex=newlex;
  if(newlex+strlen(from)>=lextable+(MAXLEX-1)) 
    fatal_error("string table overflow");
  while((*newlex++ = *from++))
  ;
  return currlex;
}

/* this should not be performed when a C-ified application
is overridden and forced to load a wam.bp file
*/

#define INSERT_LEX() \
if(!g.inC) \
{ \
  if(SYSTIME==g.timestamp) atomtable[i]=s; \
  else atomtable[i]=insert_lex(s); \
} \
else \
{ \
  if(SYSTIME==g.timestamp || (g.self && LOADTIME==g.timestamp)) \
    atomtable[i]=s; \
  else \
    atomtable[i]=insert_lex(s); \
}

/* if we want absolutely no reuse of existing C-strings
#define INSERT_LEX() atomtable[i]=insert_lex(s)
*/

#define SAME_STRING(s,t) (0==strcmp(s,t))

/* STRING HASHING */

#define SUSED() (atomtable[i])
#define SFOUND() (atomtable[i] && SAME_STRING(s,atomtable[i]))

#define SKEY() \
  STRING_HASH(s,i,ZERO); \
  i=MOD(i,MAXATOM)

#if TRACE>1 || PROF
no skey(string s)
{ register no i;
  SKEY();
  return i;
}
#endif

void sfull(string mes)
{
   fprintf(STD_err,"%ld symbols, %s!!! ",newatom,mes);
   warnmes("symbol hashing table (almost) full");
   bp_halt(5);
}

static no atomno(register string s)
{
  register no i,last;
  SKEY();
  last=MOD((i+newatom-1),MAXATOM);
  while(i!=last && SUSED() && !SFOUND())
    i=MOD((i+1),MAXATOM);
  if(!SUSED())
  {
    INSERT_LEX();
    newatom++; atomstamp[i]=g.timestamp;
    if(newatom>(MAXATOM>>1)+(MAXATOM>>2) ) sfull("(%75)");
    return i;
  }
   else if(i==last)
  {
    sfull("unexpected!!!"); 
  }
  /* else if found */
  return i;

}

void atombak(byte stamp)
{       no i; 
  for(i=0; i<MAXATOM; i++)
    if(atomstamp[i]>stamp && atomtable[i])
    { 
#if TRACE>2
      fprintf(STD_err,"cleaned up atom-> [%ld],%s\n",i,atomtable[i]);
#endif
      atomtable[i]=NULL;
      newatom--;
    }
}


void atomcommit(byte stamp)
{       no i; 
  for(i=0; i<MAXATOM; i++)
    if(atomstamp[i]>stamp && atomtable[i])
    { 
#if TRACE>2
      if(atomstamp[i]>RUNTIME) 
         fprintf(STD_err,"strange atom-> [%ld],%s\n",i,atomtable[i]);
#endif
      atomstamp[i]=stamp;
    }
}

cell new_func(string name, no argctr)
{ 
  if(argctr>MAXARITY)
    { fprintf(STD_err,"%s/%ld",name,argctr);warnmes("arity limit exceeded");
      argctr=0;
    }
  return C2C(PUTARITY(PUTSYMNO(FUNTAG,atomno(name)),argctr));
}
