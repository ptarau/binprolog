#define FLOAT_TRACE 2

#include "defs.h"
#include "global.h"

extern struct specsyms g;
extern string *atomtable;

#include <math.h>

typedef unsigned int half;

typedef union {
  double d;
  struct {half t0,t1;} i;
} split_double;

#define RIGHTBITS TAGBITS
#define LEFTBITS (sizeof(half)*8-(RIGHTBITS))


extern void warnmes (string mes);

void double_to_ints(double f, no *pi1, no *pi2, no *pi3)
{  register half i1,i2,i3; half i0; split_double s;
   s.i.t0=0;s.i.t1=0;
   s.d=f; 
   i2=s.i.t0;
   i3=s.i.t1;
   i0=(i2<<LEFTBITS)>>LEFTBITS;
   i1=((i3<<LEFTBITS)>>LEFTBITS)<<RIGHTBITS;
   i1=(i1|i0)<<RIGHTBITS;
   i2=(i2>>RIGHTBITS)<<RIGHTBITS;;
   i3=(i3>>RIGHTBITS)<<RIGHTBITS;
   i1+= INTTAG;
   i2+= INTTAG;
   i3+= INTTAG;
#if TRACE>FLOAT_TRACE
   fprintf(STD_err,"f=%f --> i1=%d i2=%d i3=%d\n",f,i1,i2,i3);
#endif
   *pi1=i1; *pi2=i2; *pi3=i3;

}

double ints_to_double(register half i1, register half i2, register half i3)
{  split_double s; half i0;
#if TRACE>FLOAT_TRACE
   fprintf(STD_err,"[1] ints->%d,%d,%d\n",i1,i2,i3);
#endif
   i1 -= INTTAG;
   i2 -= INTTAG;
   i3 -= INTTAG;
#if TRACE>FLOAT_TRACE
   fprintf(STD_err,"[2] ints->%d,%d,%d\n",
      i1>>TAGBITS,i2>>TAGBITS,i3>>TAGBITS);
#endif
   i1=i1>>RIGHTBITS;
   i0=(i1<<LEFTBITS)>>LEFTBITS;
   i1=i1>>RIGHTBITS;
   i2=i2|i0;
   i3=i3|i1;
   s.i.t0=i2;
   s.i.t1=i3;
#if TRACE>FLOAT_TRACE
   fprintf(STD_err,"[3] float->%f\n",s.d);
#endif
   return s.d;
}

#define MAKE_FLOAT(I,J,K) \
{  H[0]=g.bp_float; \
   H[1]=(I); H[2]=(J); H[3]=(K);\
   H+=4;\
}

#if TRACE > 0
extern string smartref(cell x, register stack wam);
extern stack root_wam;
#endif

void bad_arith_arg(cell xval, string mes)
{
  warnmes("bad arg in arithmetic operation");
  fprintf(STD_err,"%s TAG=%ld,VALUE=%ld\n",mes,GETTAG(xval),xval>>TAGBITS);
#if TRACE>0
  { 
    fprintf(STD_err,"%s %s ???\n",mes,smartref(xval,root_wam));
  }
#endif
}

#define TRY_GET_FLOAT(T,F) \
  xref=(T);\
  if(INTEGER(T2C(xref))) (F)=0.0+OUTPUT_INT(T2C(xref)); \
  else if(VAR(T2C(xref))) \
    { cell xval=GETREF(xref); \
      if(INTEGER(xval)) \
         (F)=0.0+OUTPUT_INT(xval); \
      else if BP_FLOAT(xval) \
         (F)=ints_to_double((half)(xref[1]), \
			   (half)(xref[2]),(half)(xref[3])); \
      else /* ugly, unexpected var, const or struct */ \
        { (F)=0.0; \
          bad_arith_arg(xval,"=>");\
          return 0; \
        }\
     }


#define GET_FLOAT(T,F) \
  TRY_GET_FLOAT(T,F) \
  else { bad_arith_arg(T2C(xref),"==>"); \
         return NULL; \
  } 

#define PUT_FLOAT(F) \
double_to_ints((F),&i1,&i2,&i3); \
MAKE_FLOAT(i1,i2,i3)

term make_float(term H, double f)
{ no i1,i2,i3;
  PUT_FLOAT(f);
  return H;
}

#define EVAL_FLOAT(Expr) \
f=(Expr); \
break;

#define FLOAT_REL(Relop) \
return (f1 Relop f2)?H:NULL;

bp_long float_compare(term t1, term t2)
{ term xref; double f1,f2;
  TRY_GET_FLOAT(t1,f1) else f1=0.0;
  TRY_GET_FLOAT(t2,f2) else f2=0.0;
#if TRACE>FLOAT_TRACE
  fprintf(STD_err,"FLOAT_COMPARE: f1=%f f2=%f\n",f1,f2);
#endif
  return ZERO-(f1<f2)+(f2<f1);
}

/* float_op(+,X2,X3,X4) X-->H, H+= size */

term float_op(register term H, byte opcode, term t1, term t2)
{ term xref; double f1,f2,f; no i1,i2,i3;
#if TRACE>FLOAT_TRACE
  fprintf(STD_err,"FLOAT_OP 1: opcode=%u t1=%lu t2=%lu\n",opcode,t1,t2);
#endif
  GET_FLOAT(t1,f1);
  GET_FLOAT(t2,f2);
#if TRACE>FLOAT_TRACE
  fprintf(STD_err,"FLOAT_OP 2: opcode=%u f1=%f f2=%f\n",opcode,f1,f2);
#endif
  switch(opcode)
  {
    case PLUS_3: EVAL_FLOAT(f1+f2)
    case SUB_3: EVAL_FLOAT(f1-f2)
    case MUL_3: EVAL_FLOAT(f1*f2)
    case FDIV_3: EVAL_FLOAT(f1/f2)

    case LESS_2: FLOAT_REL(<)
    case GREATER_2: FLOAT_REL(>)
    case LESS_EQ_2: FLOAT_REL(<=)
    case GREATER_EQ_2: FLOAT_REL(>=)
    case ARITH_EQ_2: FLOAT_REL(==)
    case ARITH_DIF_2: FLOAT_REL(!=)
    default: { 
		/*fprintf(STD_err,"*** UNEXPECTED FLOAT_OP 2: opcode=%u f1=%f f2=%f\n",opcode,f1,f2);*/
		return 0;
	}
  }
#if TRACE>FLOAT_TRACE
  fprintf(STD_err,"FLOAT_OP 3: opcode %u (Res)=H=%ld\n",opcode,H);
#endif
  PUT_FLOAT(f);
#if TRACE>FLOAT_TRACE
  fprintf(STD_err,"FLOAT_OP 4: opcode %u (Res+16)=NewH=%ld\n",opcode,H);
#endif

  return H;
}

term float_fun2(register term H, register term regs)
{  cell xval=X(1); 
   term xref,t1=C2T(X(2)),t2=C2T(X(3)); 
   double f1,f2,f; no i1,i2,i3; byte opcode;

   ATOMIZE(xval); if(!SYMCONST(xval)) return NULL;
   opcode = *NAME(xval);

   GET_FLOAT(t1,f1);
   GET_FLOAT(t2,f2);

#if (TRACE>FLOAT_TRACE)
   fprintf(STD_err,"FLOAT_FUN2: opcode=%u f1=%f f2=%f\n",opcode,f1,f2);
#endif

  switch(opcode)
  {
    case 'p': EVAL_FLOAT(pow(f1,f2))
    case 'l': EVAL_FLOAT(log(f2)/log(f1))
    case 'a': EVAL_FLOAT(atan2(f1,f2))
    /*case 'h': EVAL_FLOAT(hypot(f1,f2)) seems buggy under Solaris 2.4 */

    case 'h': EVAL_FLOAT(sqrt(f1*f1+f2*f2))

    default: return 0;
  }
  PUT_FLOAT(f);
  return H;
}

term float_fun(register term H, register term regs)
{ cell xval=X(1); term t1=C2T(X(2)),xref;
  double f1,f; no i1,i2,i3; byte opcode;
   ATOMIZE(xval);
   if(!SYMCONST(xval)) return NULL;
   opcode = *NAME(xval);
   GET_FLOAT(t1,f1);
#if TRACE>FLOAT_TRACE
  fprintf(STD_err,"FLOAT_FUN: opcode=%u f1=%f\n",opcode,f1);
#endif
  switch(opcode)
  {
    case 'i': if(f1<0) f1++; /* SICStus but not SWI compatible */
              i1=(no)floor(f1);
              SETCELL(H++,INPUT_INT(i1)); return H;
    case 'e': EVAL_FLOAT(exp(f1))
    case 'l': EVAL_FLOAT(log(f1))
    case 's': EVAL_FLOAT(sin(f1))
    case 'c': EVAL_FLOAT(cos(f1))
    case 't': EVAL_FLOAT(tan(f1))
    case 'T': EVAL_FLOAT(atan(f1))
    case 'S': EVAL_FLOAT(asin(f1))
    case 'C': EVAL_FLOAT(acos(f1))
    case 'Q': EVAL_FLOAT(sqrt(f1))
    default: return 0;
  }
  PUT_FLOAT(f);
  return H;
}


term input_float(register term H, register term regs, register stack wam)
{ no i1,i2,i3; double f;
  char sbuf[255];
  ATOMIZE(X(1)); ATOMIZE(X(2)); ATOMIZE(X(3));
  if(!(INTEGER(X(1)) && SYMCONST(X(2)) && INTEGER( X(3) ))) return FALSE;
  sprintf(sbuf,"%ld%-se%-ld ",
    OUTPUT_INT(X(1)),NAME(X(2)),OUTPUT_INT(X(3)));
#if TRACE>FLOAT_TRACE
  fprintf(STD_err,"input_float: <%s>\n",sbuf);
#endif
  f=atof(sbuf);
  double_to_ints(f,&i1,&i2,&i3);
  MAKE_FLOAT(i1,i2,i3);
  return H;
}

