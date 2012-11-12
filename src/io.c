#include <string.h>
#include <ctype.h>
#include "global.h"


extern struct specsyms g;
extern struct limit max;
extern string *atomtable;
extern term local_error(cell xval, string Msg, register stack wam);
extern term unify(register cell v1, register cell v2, register stack wam, register term *A);
extern cell new_func (string name, no argctr);
extern void warnmes (string mes); 
extern hentry htable;

/* 
* sread0 has been moved to c_inter as it is used only there in vers > 5.37
*/

/***************************************************************************
FILE write.c: IT CAN BE USED TO write out ATOMS+VARS by the new IO functions
***************************************************************************/

#define COUT(Char) (*g.stop++ = (Char))

#define SOUT0(Obj) {register string s0=(Obj); \
while((*g.stop++ = *s0++));} g.stop--

#define qSOUT1(Obj) {register string s0=(Obj); char x; \
while((*g.stop++ = x= *s0++)) if('\''==x) *g.stop++=x;} g.stop--

static void qSOUT0(char *str) {
  char c='\0'; 
  char *s=str;
  /* if(*s!='\'') { */
    while((c = *s++)) {
      if(c<'a'||c>'z') break;
    }
  /*}*/
  if(c=='\0' && s!=str+1) {
	  SOUT0(str);
  }
  else {
	  COUT('\'');
	  qSOUT1(str);
	  COUT('\'');
  }
}


static no g_quote=0;

#define SOUT(Obj) if(g_quote) {qSOUT0(Obj);} else {SOUT0(Obj);}

#define IOUT(Obj) BP_LONG2STR(ibuf,(Obj)); SOUT0(ibuf)

#define MAX1 64

void trim_float(string buf) {
  int l=strlen(buf); 
  while('0'==buf[--l]); 
  if(buf[l]=='.')
    buf[l]='\0';
  else 
    buf[l+1]='\0';
}

/* using %g instead of %f creates read back problems in both BinProlog and Jinni */
#if VCC>0
#define FLOAT_OUT(Obj) {sprintf(ibuf,"%#.8Lf",(Obj)); trim_float(ibuf); SOUT0(ibuf);}
#else
#define FLOAT_OUT(Obj) {sprintf(ibuf,"%g",(Obj));SOUT0(ibuf);}
#endif
typedef cell half;
extern double ints_to_double(register half i1, 
                             register half i2, register half i3);

/* prints a term given as a heap reference */

#define VOUT(Stk,Mark) \
if((term)xval>=(term)wam[Stk].base && \
  (term)xval< (term)wam[Stk].end) \
{ COUT('_'); \
  Mark;\
  IOUT((cell)(((char *)xval-(char *)(wam[Stk].base)) / sizeof(term)) ); \
} \
else

#define BOUT(Mark) \
if((term)xval>=(term)g.shared[BBoardStk].base && \
  (term)xval< (term)g.shared[BBoardStk].end) \
{ COUT('_'); \
  Mark;\
  IOUT((cell)(((char *)xval-(char *)(g.shared[BBoardStk].base)) / sizeof(term)) ); \
} \
else

#define TVOUT(Base,Size,Mark) \
if((term)xval>=(term)(Base) && \
   (term)xval< (term)(Base)+(Size)) \
   { COUT('_'); \
     Mark;\
     IOUT((cell)(((char *)xval-(char *)(Base)) / sizeof(Base)) ); \
   } \
else

#define MVOUT(Mark) \
   { COUT('_'); \
     Mark; \
     IOUT(xval); \
   }

static void out(register cell xval, stack wam)
{ register term xref;
  static char ibuf[MAX1];
  
  FDEREF(xval);
  if(g.stop-g.sbuf>(bp_long)max.SBUF-MAX1)
    { warnmes("string buffer (-i option) exceeded or infinite term"); return; }
  if(VAR(xval))
    { 
/* obsolete
      ASSERT2((void*)g.shared[BBoardStk].base<(void*)htable &&
              (void*)htable<(void*)wam[HeapStk].base, xval);
*/
      VOUT(HeapStk,COUT('x'))
        BOUT(COUT('b'))
           TVOUT(htable,max.DICT*3*sizeof(cell),COUT('h')) 
            MVOUT(COUT('m'));
    }
  else
    {
      if(INTEGER(xval))
        {IOUT(OUTPUT_INT(xval));}
      else
  { 
    if(!GETARITY(xval))
      {SOUT(NAME(xval));}
    /* operators can be handled here easily
    else if(g.DIF==xval)
      {
        out(xref+1,wam),
        SOUT(NAME(s));
        out(xref+2,wam);
      }
    */
    else if IS_LIST(xval)
      {
        COUT('[');
        out((cell)(++xref),wam); 
        ++xref;
        FDEREF(T2C(xref));
        while(IS_LIST(xval))
         {
           COUT(',');
           out((cell)(++xref),wam);
           ++xref;
           FDEREF(T2C(xref));
         }
        if(g.NIL!=xval)
          {
            COUT('|');
            out((cell)xref,wam);
          }
        COUT(']');
      }
        else if (BP_FLOAT(xval)) 
        {
           FLOAT_OUT(ints_to_double(
                (half)(xref[1]),
                (half)(xref[2]),
                (half)(xref[3])));
        }
    else
      { register no i;
        SOUT(NAME(xval));
        COUT('(');
        for (i=1; i<GETARITY(xval); i++)
          {
            out(xref[i],wam);
            COUT(',');
          }
        out((cell)(xref+i),wam);
        COUT(')');
      }
  }
    }
}

#define BUFOUT() \
  g.stop=g.sbuf; \
  out(xval,wam); COUT('\0')

string swrite(cell xval, stack wam)
{
  g_quote=0;
  BUFOUT();
  return g.sbuf;
}


cell sout(cell xval, stack wam)
{
  g_quote=0;
  BUFOUT();
  return INPUT_STRING(g.sbuf);
}

string qsout(cell xval, stack wam) {
  g_quote=1;
  BUFOUT();
  g_quote=0;
  return g.sbuf;
}

void fout(cell xval, stack wam, FILE *f)
{
  g_quote=0;
  BUFOUT();
  fprintf(f,"%s",g.sbuf);
#if defined SGI || defined AIX
  fflush(f);
#endif
}

#ifdef VIVO
bp_long obfuscate(bp_long c) {
  return c+77;
}

bp_long unobfuscate(bp_long c) {
  return c-77;
}
#endif

void qprint(cell xval, stack wam, FILE *f)
{ g_quote=1;
  BUFOUT();
  g_quote=0;
  {string t;
  t=g.sbuf+strlen(g.sbuf);
  *t++='.';
  *t++='\n';
  *t='\0';
  }
#ifdef VIVO
  {
   if(QLEVEL()==1111) {
    bp_long i; string s=g.sbuf; bp_long l=strlen(s);
    for(i=0;i<l;i++) {
      s[i]=(char)obfuscate((bp_long)s[i]);
    }
   }
  }
#endif
  fprintf(f,"%s",g.sbuf);
  fflush(f);
}

void errmes(string mes, cell arg, stack wam)
{ fprintf(STD_err,"%s ",mes);
  fout(T2C(&arg),wam,STD_err);
  fprintf(STD_err,"\n");
}
