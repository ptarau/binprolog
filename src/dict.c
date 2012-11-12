#include "global.h"
#include <string.h>

extern struct specsyms g;
extern struct limit max;
extern string *atomtable;
extern byte *make_byte_array(bp_long a_size);
extern hentry make_hentry(bp_long a_size);
extern void warnmes (string mes);
extern void bp_halt (int i);
extern void overflow_by (term *Top, bp_long LimitNo, stack wam, string culprit);
extern term local_error (cell xval, string Msg, register stack wam);
extern void debugmes(string format, string mes);

/* ATOM HASHING MACROS */

#define FULL -1
#define FREE 0
#define FOUND 1

#if 1
#define HMIX1(x) (x<<2)
#define HMIX2(x) ((x<<5)^(x>>17))
#define HKEY(pred,fun) MOD(HMIX1(pred)+HMIX2(fun),HMAX)
#else
#define HKEY(pred,fun) MOD((pred+fun),HMAX)
#endif

#define HDELETED() (!htable[i].val)
#define HUSED() (htable[i].pred && htable[i].fun)
#define HASH_NEXT() i=MOD((i+1),HMAX)
#define HFOUND() (htable[i].pred==pred && htable[i].fun==fun)
#define HNOTFULL() (i!=last)
#define HLIMIT(Lim) (Lim)=MOD((i+hcount),HMAX)

#define HCHECK() if(hcount>(bp_long)((HMAX>>1)+(HMAX>>2)) ) hfull("(75%)")

/*
int inthash(int key)
{
  key += ~(key << 15);
  key ^=  (key >>> 10);
  key +=  (key << 3);
  key ^=  (key >>> 6);
  key += ~(key << 11);
  key ^=  (key >>> 16);
  return key;
}
*/

/* LINEAR HASHING FOR INDEXING */
/* assert: val is not 0, hput avoids duplications */

hentry htable;
byte *hstamp;
bp_long hcount;

void hfull(string mes)
{
   fprintf(STD_err,"%ld bytes, %s!!! ",hcount*sizeof(struct hentry),mes);
   warnmes("hashing table (almost) full");
   bp_halt(6);
}

bp_long hinit(void)
{
  htable=make_hentry(HMAX);
  hstamp=make_byte_array(HMAX);
  hcount= -1;
  return htable && hstamp;
}

#define HPUT(Index,Pred,Fun,Val,Stamp) \
       HCHECK(); \
       htable[(Index)].pred=(Pred);\
       htable[(Index)].fun=(Fun);\
       htable[(Index)].val=(Val);\
       hcount++;\
       hstamp[(Index)]=(Stamp)

#define STAMP_ERROR()\
{\
   fprintf(STD_err,"Stamp found %d, expected %d\n",stamp,hstamp[i]);\
   return (no)local_error(X(1),"hash op on protected object",wam);\
}


#define H_VARCHECK() \
	if(VAR(X(1))) ERREXIT("arg 1 of hash op should be nonvar")\
	if(VAR(X(2))) ERREXIT("arg 2 of hash op should be nonvar")

#define SAFE_HASH_OP(ProtectStamp) HASH_OP0(H_VARCHECK(),ProtectStamp)

/* #define HASH_OP(ProtectStamp) HASH_OP0(EMPTY,ProtectStamp) 
   $$$ removed - because of potential gc bug
*/

#define HASH_OP0(Check,ProtectStamp) \
	ATOMIZE(X(1)); ATOMIZE(X(2)); \
        Check; \
	found=hindex(X(1),X(2),&i);\
	if(FOUND==found && \
           hstamp[i]!=(ProtectStamp) && \
           htable[i].val!=g.empty \
           ) \
          STAMP_ERROR() \
	if(FULL==found) \
	  return (no)local_error(X(1),"htable full in hash op",wam) \
/* at this point found == FOUND || FREE */ 

no hdef(register no pred, register no fun, register no val, byte stamp)
{
  register no i=HKEY(pred,fun),last;
  HLIMIT(last);
  while(HNOTFULL() && HUSED() && !(HFOUND()))
    HASH_NEXT();
  if(!HUSED() || (HDELETED() && HFOUND()) )
      {
        HPUT(i,pred,fun,val,stamp);
        return 1;
      }
  return 0;     
}

no hset(register no pred, register no fun, register no val)
{
  register no i=HKEY(pred,fun),last; HLIMIT(last);
  while(HNOTFULL() && HUSED() && !(HFOUND()))
    HASH_NEXT();
  if(HFOUND())
      {
        htable[i].val=val;
        return 1;
      }
  warnmes("hdef/3 required before using hset/3");
#if 0
  fprintf(STD_err,"%s/%ld+...->%s/%ld\n",
     NAME(pred),GETARITY(pred),
     NAME(val),GETARITY(val)
    );
#endif
  return 0;
}

#define HDELETE(Pred,Fun) hset(Pred,Fun,(no)0)

no hget(register no pred, register no fun)
{
  register no last,i=HKEY(pred,fun);
  if(HFOUND()) return htable[i].val;
  HLIMIT(last);
  do {
       HASH_NEXT();
       if(HFOUND()) return htable[i].val;
     }
  while(HNOTFULL() && HUSED());   
  return (no)NULL;
}

bp_long hindex(register no pred, register no fun, bp_long *index)
{
  register no last,i=HKEY(pred,fun);
  if(HFOUND()) {*index=i; return FOUND;}
  if(!HUSED()) {*index=i; return FREE;}
  HLIMIT(last); 
  do  
     {
       HASH_NEXT();
       if(HFOUND()) {*index=i; return FOUND;}
     }
  while(HNOTFULL() && HUSED());
 
  if(!HNOTFULL()) {hfull("culprit->hindex"); return FULL;}
  else {*index=i; return FREE;}
}

void hbak(byte stamp)
{ no i;

  if(BBOARDTIME==stamp)
    {
     for(i=0; i<HMAX; i++)
       if(hstamp[i]>stamp && htable[i].pred)
        { /* if(VARTIME!=hstamp[i]) {warnmes("bad stamp"); bp_halt(7);} */
        htable[i].val=(no)&htable[i].val;
      }
    }
  else 
   {
     for(i=0; i<HMAX; i++)
       if(hstamp[i]>stamp && htable[i].pred)
       {       
         hcount--;
         htable[i].pred=0;
         htable[i].fun=0;
         htable[i].val=0; 
       }
   }
}

void hcommit(byte stamp)
{ no i;
     for(i=0; i<HMAX; i++)
       if(hstamp[i]>stamp && htable[i].pred)
       {       
         hstamp[i]=stamp;       
       }
}

#define SAVE_FUN(Fun) \
{ register cell f=(Fun); \
  if(INTEGER(f))\
  {\
     PUSH_LIST(f); \
     PUSH_LIST(INPUT_INT( 0 )); \
  }\
  else\
  {\
    PUSH_LIST(PUTARITY(f,0)); \
    PUSH_LIST(INPUT_INT(GETARITY(f))); \
  }\
}


#define HDEREF(x) \
while(ON(HeapStk,x) && !NONVARREF(x) && (x)!=(term)GETREF(x)) \
 (x)=(term)GETREF(x)

#define BDEREF(x) { \
xref=(term)x; xval=x;\
while((ON(HeapStk,xval)||ONSTACK(g.shared[BBoardStk],xval)) \
  && !NONVARREF(xval) && (xval)!=(term)GETREF(xval)) {\
   xref=(term)xval;\
   fprintf(STD_err,"BDEREF: %s->%s\n",smartref(xref,wam),smartref(xval,wam));\
   xval=GETREF(xref);\
  }\
}

static term cleanup(term p, register stack wam)
{ 
  if(ON(HeapStk,p) && NONVARREF(p))
    p=(term)GETREF(p);
  return p; 
}

static void str(cell ptr, register stack wam,string s)
{ int i;
  for(i=0; i<MaxStk; i++)
    if(ON(i,ptr))
      {
        sprintf(s,"%s[%ld]",wam[i].name,(term*)ptr-wam[i].base);
        return;
      }
  if(ONSTACK(g.shared[BBoardStk],ptr)) {
      sprintf(s,"BBOARD[_%ld]",(term)ptr-(term)g.shared[BBoardStk].base);
    }
  else if(ONSTACK(g.shared[InstrStk],ptr)) {
      sprintf(s,"CODE[_%ld]",(term)ptr-(term)g.shared[InstrStk].base);
  }
  else if(INTEGER(ptr)) 
     sprintf(s,"bp_long(%ld)",OUTPUT_INT(ptr));
  else 
  {       
    if(GETSYMNO(ptr)<MAXATOM && NAME(ptr))
      sprintf(s,"%s/%lu",NAME(ptr),GETARITY(ptr));
    else
      sprintf(s,"MEM[%lu]",(no)ptr);
  }
}

string smartref(cell x, register stack wam)
{ char s[80];
  if(VAR(x))
  {
    term ptr=(term)x;
    term temp=(term)x;
    HDEREF(temp);
    temp=cleanup(temp,wam); 
    if(ON(HeapStk,ptr) && !NONVARREF(ptr) && temp!=cleanup(ptr,wam))
      { char t[80];
        str((no)temp,wam,t);
        sprintf(s,"_%ld -> %s",ptr-(term)wam[HeapStk].base,t);
      }
    else
      str((cell)temp,wam,s);
  }
  else {
    /*sprintf(s,"UNSIGNED=%lu",x); */ 
    str(x,wam,s);
  }

  return strdup(s);
}

#if TRACE>0

void bbcheck(stack wam) {
  {term *tp;
  fprintf(STD_err,"\nBlackBOARD => %lu\n",g.shared[BBoardStk].top-g.shared[BBoardStk].base);
  for(tp=g.shared[BBoardStk].base; tp<g.shared[BBoardStk].top; tp++)
    if(VAR(tp) && ! ONSTACK(g.shared[BBoardStk],tp)) {
      fprintf(STD_err,"[%lu] %s\n",tp-g.shared[BBoardStk].base,
       smartref((cell)(*tp),wam));
    }
  }
}
#endif

/* at the end either Ref is var and Val is compound or Ref==Val */


term hlist(register term H, register term regs, stack wam)
{ no i; cell xval; bp_long ival; byte stamp;
#if TRACE>0
  fprintf(STD_err,"entering hlist, wam=%d, bboard=%d H=%d\n",
    wam,g.shared[BBoardStk].base,H);
  bbcheck(wam);
#endif
  if(!INTEGER(X(1))) return NULL; /* first arg: stamp */
  stamp=(byte)(OUTPUT_INT(X(1)));
  xval=X(2); /* second arg: starting arity of listed terms */
  if(!INTEGER(xval)) return NULL;
  ival=OUTPUT_INT(xval);
  for(i=0; i<HMAX; i++)
    if(hstamp[i]>=stamp && HUSED())
      { term xref=C2T(g.predmark);

        if(hstamp[i]<=RUNTIME)
          { /* gets preds of arity < ival `represented' as g.predmark*/
            if(g.predmark!=htable[i].pred 
                || GETARITY(htable[i].fun)<(no)ival) 
              continue;
              xval=g.predmark;
          }
        else
          { /* gets RUNTIME data of arity > ival */
            cell v=htable[i].val;
			if(NULL==(term)v) 
			  continue;
            if(VAR(v) &&
              !(
                 ONSTACK(g.shared[BBoardStk],v) ||
                 ONSTACK(g.shared[InstrStk],v) /*|| ON(HeapStk,v) */
               )) { 
#if TRACE>0
                fprintf(STD_err,
                 "unexpected data in htable[%d]=>\n<%s,%s>->%s\n",i,
                  smartref(htable[i].pred,wam),
                  smartref(htable[i].fun,wam),
                  smartref(v,wam));
#endif
                /* continue; */
            }      
         
            FDEREF(v);

            if((INTEGER(xval) && ival>0) 
                || VAR(xval)
                || (GETARITY(xval) < (no)ival)
                || xval==g.empty 
             )  
            continue;
            if(COMPOUND(xval))
              xval=T2C(xref);
          }
        IF_OVER("COPY_KEYS",(term *)H,HeapStk,bp_halt(9));
        SAVE_FUN(htable[i].pred);
        SAVE_FUN(htable[i].fun);
#if 0
        ASSERT2(( ATOMIC(xval)
           || ONSTACK(g.shared[BBoardStk],xval)
           || ON(HeapStk,xval)), /* will fail with multiple engines */
        xval);
#endif
        PUSH_LIST(xval);
      }
  PUSH_NIL();
  return H;
}

#if TRACE_EXEC>0
void show_ucount0(doit)
  no doit;
{ no i;
  static bp_long total;
  if(!doit) total=0.0;
  for(i=0; i<HMAX; i++)
    if(HUSED() && g.ucount==htable[i].pred)
       { bp_long k=OUTPUT_INT(htable[i].val); 
         if(doit)
           {
             cell f=htable[i].fun; double p=(100.0*k)/total;
             if(p>0.1)
               fprintf(g.tellfile,"%14ld: %5.1f%c %s/%d\n",
                     k,p,'%',NAME(f),GETARITY(f)-1);
          
            }
          else total+=k;
       }
  if(doit)
    fprintf(g.tellfile,"%14ld: %5.1f%c --------------------\n",
                     total,100.0,'%');
}

void show_ucount()
{
  show_ucount0(0);
  show_ucount0(1);
}
#endif

cell op_class(register cell a)
{  
   if(a==g.xfx||a==g.xfy||a==g.yfx) return g.infixop;
   if(a==g.fx||a==g.fy) return g.prefixop;
   if(a==g.xf||a==g.yf) return g.postfixop;
   return 0;
}

cell valid_op(cell p, register cell a, cell v)
{ bp_long i;
  if(
      SYMCONST(p) && 
      INTEGER(v) && ((i=OUTPUT_INT(v))>=0) && (i<=1200)
    )
    return  op_class(a);
  return 0;
}

bp_long opcount=0;

bp_long make_op(cell op, cell assoc, cell pri, byte stamp)
{ bp_long i, found; cell cls;
        if(!(cls=valid_op(op,assoc,pri))) return 0;

	found=hindex(op,cls,&i);
	if(FULL==found) return 0;
        if(FOUND==found)
            {   htable[i].val=pri;
                hset(cls,op,assoc);
#if TRACE>2
		fprintf(STD_err,
                "found op=%s assoc=%s pri=%d %d->?, opcount=%d\n",
                NAME(op),NAME(assoc),OUTPUT_INT(pri),i,opcount);
#endif
	    }
        else
	    {  cell k=INPUT_INT(opcount);
               HPUT(i,op,cls,pri,stamp);
               hdef(cls,op,assoc,stamp);
               if(hdef(op,g.opmark,k,stamp))
                 {
                   hdef(g.opmark,k,op,stamp);
                   opcount++;
                 }
#if TRACE>2
	       fprintf(STD_err,
                "notfound(i) op=%s assoc=%s pri=%d i=%d, opcount=%d\n",
               NAME(op),NAME(assoc),OUTPUT_INT(pri),i,opcount);
#endif
	    }
	return 1;
}

bp_long op0(register term regs, stack wam)
{ 
  ATOMIZE(X(1)); ATOMIZE(X(2)); ATOMIZE(X(3));
  return make_op(X(1),X(2),X(3),g.timestamp);
}

/* todo for $$$GC: make var on heap have it to point to val in htable, 
   trail it ???
*/

cell lval(register term regs, register stack wam, byte stamp)
{ bp_long i,found;
  SAFE_HASH_OP(stamp);
	if(FREE==found ) {
     HPUT(i,X(1),X(2),((cell)(&htable[i].val)),stamp);
	}
	return htable[i].val;
}

#define ON_BBOARD(Val) \
  ((term*)(Val)>=g.shared[BBoardStk].base && \
  (term*)(Val)<g.shared[BBoardStk].top )

#define BB_SAFE(Val) \
if(!(ATOMIC(Val) || ON_BBOARD(Val)) ) \
  return (no)local_error((Val),"unsafe data in def/3 or set/3",wam)

no def(register term regs, stack wam, byte stamp)
{ bp_long i,found; cell xval=X(3);
  SAFE_HASH_OP(stamp);
  if(FOUND==found && htable[i].val!=g.empty) 
    return 0;  
  BB_SAFE(xval);
  if(FOUND==found) 
	htable[i].val=xval;
  else
  	{HPUT(i,X(1),X(2),xval,stamp);}
  return 1;
}

no set(register term regs, stack wam, byte stamp)
{ bp_long i,found; cell xval=X(3);
  ATOMIZE(X(1)); ATOMIZE(X(2));
  found=hindex(X(1),X(2),&i);
  if(FOUND!=found) 
    return (no)local_error(X(1),
     "def/3 expected before set/3 or rm/2",
    wam);
  if(hstamp[i]!=stamp || htable[i].val==g.empty) 
#if 0
    return 0;
#else
    return (no)local_error(X(1),
     "bad stamp or empty slot in set/3 or rm/2",
    wam);
#endif
  BB_SAFE(xval);
  htable[i].val=xval;
  return 1;    
}

#if HAS_OVERRIDE

#if TRACE>0
void check_pred(cell fun,stack wam)
{ instr pred;
  if(!IDENTIFIER(fun))
    {
      (void)LOCAL_ERR(fun,"IDENTIFIER EXPECTED AS PREDICATE NAME");
    }
  pred=GETPRED(fun);
  if(!pred)
  {
    (void)LOCAL_ERR(fun,"PREDICATE ADDRESS NOT FOUND");
  }
#if GC>0
  if(fun!=ADDR2FUN(pred))
  {
    (void)LOCAL_ERR(fun,"PREDICATE WITH NO FUNCTOR");
  }
#endif
}
#define CHECK_PRED() check_pred(xval,wam)
#else
#define CHECK_PRED()
#endif

#define PRED2ADR(Pred,Adr) \
  xval=(Pred); ATOMIZE(xval); \
  xval=PUTARITY(xval,GETARITY(xval)+1); \
  CHECK_PRED(); \
  (Adr)=GETPRED(xval); \
  if(!(Adr)) return FALSE


no override(register term regs,
            register stack wam)
{
  instr P,NewP; cell xval,fun;
  debugmes("*** %s\n","override can break gc and bbgc");

  ATOMIZE(X(1));
  if( !INTEGER(X(1)) ) 
    {
     (void)LOCAL_ERR(X(1),
     "arg 1 of of override/3 should be integer");
     return FALSE;
    }
  /* the one overriden */
  PRED2ADR(X(2),P); /* X(2) ---> xval, containing the functor */
  fun=xval;
  if(IS_JCOMPRESSED(xval)) 
   {
     (void)LOCAL_ERR(xval,
     "unable to override, maybe used as a first goal in a clause");
     return FALSE;
   }
  /* the one which overrdes */
  PRED2ADR(X(3),NewP);
  if(P!=NewP) 
  {  bp_long ires=OUTPUT_INT(X(1));
     switch(ires)
     {
       case 0: /* backtrackable: also flows to 1: */
         VTRAIL_IT(P,GETREF(P));
         VTRAIL_IT(P+1,GETREF(P+1)); /* flows to next! */
       case 1: /* just do it, without backtracking */
         SETOP(P,EXECUTE); /* insert jump to redefinition */
         SETLABEL(P,NewP);
         break;
       case 2: /* delete predicate, non-first calls will not reach it */
               /* see: disable_static/1 */
          HDELETE(g.predmark,fun);
       break;
       case 3: /* replace non-first called P with NewP */
          {
            SETPRED(fun,(cell)NewP);
          }
       break;
       default:         
         (void)LOCAL_ERR(X(1),"bad option in override/3");
     }
  }
  else
  {
     (void)LOCAL_ERR(xval,"unable to override with itself");
     return FALSE;
  }
  return TRUE;
}
#endif
