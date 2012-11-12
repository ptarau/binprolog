#include <string.h>
#include <stdio.h>

#if VCC>0
#include <process.h>
#include <io.h>
#include <direct.h>
#endif

#include "global.h"
#include "defs.h"

#if VCC==0
/* HAD PROBLEMS WITH VCC which does not have unistd.h */
#include <unistd.h>
#endif


#include <signal.h>

extern struct specsyms g;
extern struct limit max;
extern string *atomtable;
extern term local_error(cell xval, string Msg, register stack wam);
extern no unify(register cell v1, register cell v2, register stack wam, register term *A);
extern no unify_to(register cell v1, register cell v2,
            register stack wam, register term *A);

extern string lextable,newlex;
extern no newatom;
extern bp_long hcount;
extern no hget(register no pred, register no fun),
       hset(register no pred, register no fun, register no val),
       hdef(register no pred, register no fun,
            register no val, byte stamp),
       make_bboard(register stack wam);

extern void bp_halt (bp_long i);
extern cell new_func (string name, no argctr);
extern bp_long cputime (void), realtime (bp_long i);
extern void warnmes (string mes);
extern no insert_op (register no opcode, register no reg, register string name, register no arity, register stack wam);
extern bp_long float_compare (term t1, term t2);
extern cell input_fun (string name, no arity);
extern string smartref(cell x, register stack wam);
extern
  void overflow_by (term *Top, bp_long LimitNo, stack wam, string culprit),
  errmes(string mes, cell arg, stack wam);

#if TRACE > 0 || PROF
extern void *calloc_trace(no nb,no size),
            *malloc_trace(no nb,no size),
            free_trace(void *ptr);
#endif

/* BUILTINS */

#ifndef TSTORE
term term_store_op(register term H, register term regs, register stack wam)
{
  fprintf(STD_err,"TODO: term_store_op\n");
  return NULL;
}
#endif

static bp_long hkey1(register cell xval,bp_long max) {
  register term xref=C2T(xval);
  register bp_long ires;
  /*fprintf(STD_err,"HERE hkey1:entering xval=%d, max=%d\n",xval,max);*/

  if(max<0) return -2; /* depth or size limit reached */

  ATOMIZE(xval);
  if(VAR(xval)) return -1;

  if(IDENTIFIER(xval)) {
    bp_long n=GETARITY(xval);
    bp_long i; bp_long x; term f=xref;
    STRING_HASH(NAME(xval),ires,n);
    /* fprintf(STD_err,"HERE hkey1 %s/%d=>%d\n",NAME(xval),n); */
    for(i=1; i<=n; i++) {
      xref=f+i;
      FDEREF(xref);
      /*fprintf(STD_err,"HERE hkey1:for max=%d i=%d\n",max,i);*/
      x=hkey1(T2C(xref),max-1);
      if(x<0) return x;
      ires+=(ires<<7)+x;
    }
  }
  else
    ires=OUTPUT_INT(xval);

  if(ires<0) ires= -ires;

  /*fprintf(STD_err,"HERE hkey1:exiting xval=%d, max=%d\n",xval,max);*/

  return ires;
}

bp_long deep_hash(register cell xval,bp_long max,bp_long mod) {
  bp_long ires=hkey1(xval,max);
  if(ires<0) return ires; /* error */
  {  bp_long l=(WSIZE-1-TAGBITS);
     ires=MOD(ires,ONE<<l);
  }
  if(mod>0) ires=ires % mod;
  if(ires<0) ires= -ires;
  return ires;
}

term tval(register term H, register cell xval, register bp_long num)
{
   register cell tag;
   /*DEREF1(xval);*/
                    tag=GETTAG(xval);
                    switch(tag)
                    {
                     case VARTAG:
                        H[0]=g.VAR;
                        H[1]=xval-tag+INTTAG;
                        H+=2;
                      break;

                      case INTTAG:
                        H[0]=g.INT;
                        H[1]=xval;
                        H+=2;
                      break;

                      case FUNTAG:
                        H[0]=g.DIV;
                        H[1]= (num) ? INPUT_INT(GETSYMNO(xval))
                                    :  PUTARITY(xval,0)
                                    ;
                        H[2]=INPUT_INT(GETARITY(xval));
                        H+=3;
                      break;

                      default:
                        H[0]=g.DIF;
                        H[1]=xval-tag+INTTAG;
                        H[2]=INPUT_INT(tag);
                        H+=3;
                      }
   return H;
}

cell untval(register term xref)
{
   register cell xval;
   register term vref,tref;
   register cell vval,tval,res=0;
   if(NONVAR(xref)) bp_halt(51);
   xval=GETREF(xref);
   FDEREF3(xref+1,vref,vval);
                     if(xval == g.VAR)
                     {
                        if(!INTEGER(vval)) bp_halt(52);
                        res = vval-INTTAG;
                     }
                     else if(xval == g.INT)
                     {
                         if(!INTEGER(vval)) bp_halt(53);
                         res = vval;
                     }
                     else if(xval == g.DIV) /* FUNCTOR */
                        {
                          if(!IDENTIFIER(vval)) bp_halt(54);
                          FDEREF3(xref+2,tref,tval);
                          if(!INTEGER(tval)) bp_halt(55);

                          res = PUTARITY(vval,
                                       OUTPUT_INT(tval));
                        }
                     else if(xval == g.DIF)  /* OTHER TAG */
                        {
                          if(!INTEGER(vval)) bp_halt(56);
                          FDEREF3(xref+2,tref,tval);
                          if(!INTEGER(vval)) bp_halt(57);
                          res = vval-INTTAG+OUTPUT_INT(tval);
                        }
                     else
                        {bp_halt(58);}
   return res;
}


no tlet(register term regs)
{ register cell xval;
                    ATOMIZE(X(1)); ATOMIZE(X(2));
                    xval=untval(C2T(X(3)));

                    if(hget(X(1),X(2)))
                      xval=hset(X(1),X(2),xval);
                    else
                      xval=hdef(X(1),X(2),xval,g.timestamp);
                    return xval;
}

term array_ref(register cell base, register cell index, register stack wam)
{ register term xref;
  register cell xval;
  register bp_long ires;
                    xval=index;
                    ATOMIZE(xval);
                    if(!INTEGER(xval))
                      return
                        LOCAL_ERR(xval,
                          "array_ref/3: arg2 bad array index");
                    ires=OUTPUT_INT(xval);
                    xval=base;
                    ATOMIZE(xval);
                    if(!INTEGER(xval))
                      return
                        LOCAL_ERR(xval,
                          "array_ref/3, arg 1: bad array");
                    xref=INT2PTR(xval);
  return xref+ires;
}

cell get_asserted(register term regs, register stack wam)
{ register term xref;
  register cell xval,key=X(1);
  ATOMIZE(key);
  xval=hget(g.current_db,key);
  if(!xval || g.empty==xval) return (cell)0;
  FDEREF(xval); /*ok*/
  if(g.DIF!=xval)
     return (cell)LOCAL_ERR(xval,"'-'/2 expected in get_asserted");
  FDEREF(xref[1]); /*ok*/
  if(xval!=g.DOT)
     return (VAR(xval))? 0:
       (cell)LOCAL_ERR(xval,"'.'/2 expected in get_asserted");
  xval=hget(key,g.current_db);
  if(xval && g.empty!=xval)
  { term xref; bp_long ires;
    FDEREF(xval);
    if(!INTEGER(xval))
      return (cell)LOCAL_ERR(xval,"integer ctr expected in get_asserted");
    ires=OUTPUT_INT(xval);



    /* only do the counding down if thresahlold is reasonably low

       switch it off if higher than that - see dynco/1

    */

    if(ires < 1<<16)  {
      if(ires<=0) return INPUT_INT(0);
      xval=INPUT_INT(ires-1);
      hset(key,g.current_db,xval);

    }
  }
  return T2C(xref)+INTTAG;
}

bp_long stat_used(term *top, bp_long s, register stack wam)
{
  return (top - wam[s].base)*sizeof(*top);
}

cell unix_argv(register term regs, register stack wam)
{  cell xval; bp_long ires;
                    xval=X(1);
                    if(!INTEGER(xval))
                      return (cell)LOCAL_ERR(xval,
                         "unix_argv's 1st arg must be integer");

                    ires=OUTPUT_INT(xval);
                    if(!(ires>=0 && ires<g.argc))
                        return (cell)LOCAL_ERR(xval,
                          "unix_argv's 1st arg not in range");
                    xval=new_func(g.argv[ires],0);
   return xval;
}

cell unix_getenv(register term regs, register stack wam)
{  cell xval; string s;
   xval=X(1);
   if(!SYMCONST(xval))
     return (cell)LOCAL_ERR(xval,"unix_getenv's 1st arg must be a symbol");
   s=NAME(xval);
   if(!(s=(string)getenv(s))) return 0;
   /* return (cell)LOCAL_ERR(xval,"unix_getenv's/2: no such variable");*/
   return new_func(s,0);
}

cell unix_access(register term regs, register stack wam)
{  cell xval; bp_long ires;
                    xval=X(2);

                    if(!INTEGER(xval))
                        return (cell)LOCAL_ERR(xval,
                          "unix_access/2: 2nd arg must be integer");
                    ires=OUTPUT_INT(xval);

                    xval=X(1);
                    if(!SYMCONST(xval))
                      return (cell)LOCAL_ERR(xval,
                         "unix_access's 1st arg must be a symbol");
return !access(NAME(xval),ires);
}

cell unix_cd(register term regs, register stack wam)
{  cell xval;
                    xval=X(1);
                    if(!SYMCONST(xval))
                      return (cell)LOCAL_ERR(xval,
                         "unix_cd's 1st arg must be a symbol");
   if(!!chdir(NAME(xval))) return (cell)LOCAL_ERR(xval,
      "unix_cd/1: bad directory");
   return TRUE;
}

cell unix_kill(register term regs, register stack wam)
{
#if 0==VCC
   /* unavailable with VCC */
   int kill(pid_t pid, int sig);

/* to avoid djgcc errors in DOS and cc on Next */
   cell pid,sig;
   pid=X(1);
   if(!INTEGER(pid))
     return (cell)LOCAL_ERR(pid,"unix_kill's 1st arg must be an integer");
   sig=X(2);
   if(!INTEGER(sig))
     return (cell)LOCAL_ERR(sig,"unix_kill's 2nd arg must be an integer");
   if(!!kill( (pid_t)OUTPUT_INT(X(1)), (int)OUTPUT_INT(X(2))) )
     return (cell)LOCAL_ERR(pid,"error on kill signal to this process");
#endif
   return TRUE;
}

#if 0==VCC
#include <fcntl.h>

int unix_fork() {
  int code=fork();
  if(code==0) {
    fflush(STD_out);
    fflush(STD_err);
    close(0);open("/dev/null", 0,O_RDONLY);
    close(1);open("/dev/null", 1,O_WRONLY);
    close(2);open("/dev/null", 2,O_WRONLY);
  }
  return code;
}

int unix_pid() {
   return getpid();
}

int unix_sleep(int n) {
  return sleep(n);
}

#else
  int unix_fork() {return -1;}
#if 0==VCC
  int unix_pid() {return 0;}
  int unix_sleep(int n) {return -1;}
#else
 /* if VCC */
int FORCE_CDECL _getpid();

 int unix_pid() {int p=_getpid(); return (p>= -1)?p:-p;}
 int unix_sleep(int n) {_sleep(n*1000); return 1;}
#endif
#endif

#if 1
#if VCC>0
     extern FILE *popen(const char *, const char *);
#endif

term open_stream(register term H, register term regs,
      register stack wam)
{ no ires;
  string cmd,opt; FILE *f;
  if(!INTEGER(X(1)))
    return LOCAL_ERR(X(1),"bad opcode in open_stream");
  ires=OUTPUT_INT(X(1));
  if(!SYMCONST( X(2) ))
    return LOCAL_ERR(X(2),"bad file name in open_stream");
  cmd=NAME(X(2));
  if(!SYMCONST( X(3) ))
    return LOCAL_ERR(X(3),"unexpected option in open_stream");
  opt=NAME(X(3));
  if (!strcmp(cmd,"user_input")) f = STD_in;
  else if (!strcmp(cmd, "user_output")) f = STD_out;
  else if (!strcmp(cmd, "user_error")) f = STD_err;
  else if (0==ires)
     { if(!(f=fopen(cmd,opt)))
       return LOCAL_ERR(X(2),"C-error in fopen");
     }
  else
    {
#if VCC==0
     f=popen(cmd,opt);
#else
     f=_popen(cmd,opt);
#endif
     if(!f)
      return LOCAL_ERR(X(2),"C-error: popen not available");
    }
  H=tval(H,(cell)f,0);

  return H;
}

no close_stream(register term regs,register stack wam)
{ no retcode; FILE *f=(FILE*)untval(RX(2));
  int pclose(FILE*);
  if(!INTEGER(X(1)) )
        return (no)LOCAL_ERR(X(1),"bad opcode in close_stream");
  retcode = (no)(1==(OUTPUT_INT(X(1)))?
#if VCC>0
	 _pclose(f)
#else
     pclose(f)
#endif
   : fclose(f));

  if(0!=retcode) return (no)LOCAL_ERR(X(2),"C-error in pclose");
  return TRUE;
}
#endif

bp_long stat_left(term *top, bp_long s, register stack wam)
{
  return (wam[s].margin - top)*sizeof(*top);
}

#define UNIFAIL(Term1,Term2) if(!unify((Term1),(Term2),wam,A)) return FALSE;

#define StackSTAT(Top,StackNo,StackGroup) \
  o1=stat_used((term *)(Top),StackNo,(StackGroup)); \
  o2=stat_left((term *)(Top),StackNo,(StackGroup)); \
  break

#define STAT(Top,StackNo) StackSTAT(Top,StackNo,wam)

#define PUT_ARG(I,Val) UNIFAIL(Val,regs[I])
#define PUT_INT_ARG(I,Val) PUT_ARG((I),INPUT_INT(Val))

no bb_reset(register term regs, register stack wam)
{ register term xref;
  register cell xval;
  bp_long useful_data,bb_size;
  no ok=TRUE,change_it=FALSE;
if(OUTPUT_INT(g.bbgc)>=2)
{ /* 2 means that bboard can dynamically grow/shrink */
  /* 1 means that it is gc-able in prolog but will not change size */
  change_it=TRUE;
  IN_VALUE(1,regs[1]);xval=X(1);
  if(!INTEGER(xval))
     return(no) LOCAL_ERR(xval,"bb_size in bb_reset should be an integer");

  useful_data=OUTPUT_INT(xval);
  /*if(useful_data<0)useful_data=0;*/
  bb_size=(g.shared[BBoardStk].margin-g.shared[BBoardStk].base)*sizeof(cell);
#if TRACE > 0
  fprintf(STD_err,"bb_reset: useful_data=%ld, bb_size=%ld\n",
                   useful_data,bb_size);
#endif
  if(useful_data > bb_size)
    {   max.BOARD=useful_data<<1;
    }
  else if(useful_data > (bb_size>>1))
    {  max.BOARD=max.BOARD<<1;
    }
  else if((bb_size>>2)>(g.shared[BBoardStk].over<<4) &&
          useful_data < (bb_size>>2))
    {  max.BOARD=max.BOARD>>1;
    }
  else {
    change_it=FALSE;
  }
}

  g.shared[BBoardStk].top=g.shared[BBoardStk].base;

  /* used to trigger tetris bug on Windows - fixed */

  if(change_it)
  {
    XFREE(g.shared[BBoardStk].base);
    ok=make_bboard(wam);
  }

  g.shared[BBoardStk].top=g.shared[BBoardStk].base;

  return ok;
}

bp_long stats0(register term H, register term regs, register stack wam, register term *A)
{ register term xref;
  register cell xval;
  bp_long ires; bp_long o1,o2;
                IN_VALUE(3,regs[3]);
                if(!INTEGER(X(3))) return FALSE;
                ires=OUTPUT_INT(X(3));
                switch(ires)
                {
                  case STAT_RUNTIME:
                  { bp_long t=cputime();
                    o1=t;
                    o2=t-g.rtime;
                    g.rtime=t;
                  } break;

                  case STAT_GLOBAL_STACK:
                    STAT(H,HeapStk);

                  case STAT_LOCAL_STACK:
                    STAT(A,ChoiceStk);

                  case STAT_TRAIL:
                    STAT(TR_TOP,TrailStk);

                  case STAT_CODE:
                    StackSTAT(ctop,InstrStk,g.shared);

                  case STAT_STRINGS:
                    o1=(newlex-lextable);
                    o2=(lextable+MAXLEX)-newlex;
                  break;

                  case STAT_SYMBOLS:
                    o1=newatom*sizeof(string);
                    o2=(MAXATOM-newatom)*sizeof(string);
                  break;

                  case STAT_HTABLE:
                    o1=(hcount+1)*sizeof(struct hentry);
                    o2=(HMAX-(hcount+1))*sizeof(struct hentry);
                  break;

                  case STAT_BBOARD:
                    StackSTAT(g.shared[BBoardStk].top,BBoardStk,g.shared);
                  break;

                  case STAT_GCTIME:
                  {
                    g.total_gctime += g.gctime;
                    o1 = g.total_gctime;
                    o2 = g.gctime; g.gctime=ZERO;
                  } break;

                  case STAT_REALTIME:
                  {
                    o1 = realtime(2);
                    o2 = realtime(1);
                  } break;

                  default: return FALSE;
                }
                PUT_INT_ARG(1,o1)
                PUT_INT_ARG(2,o2)
             return TRUE;
}

cell new_name(cell x,register stack wam) {
  static no ctr=0;
  char sbuf[MAXNBUF];
  if(!SYMCONST(x))
    return (cell)LOCAL_ERR(x,
       "1-st arg of new_name/2 should be an identfifier");
  sprintf(sbuf,"v%u%lu%s%lu",VERSION%100,MOD(g.rtime+rand(),1024),NAME(x),ctr++);
  return INPUT_STRING(sbuf);
}

cell symcat(register term regs, register stack wam)
{ cell x1=X(1), x2=X(2);
  char sbuf[MAXNBUF];
  if(!SYMCONST(x1))
    ERREXIT("symcat/3: bad arguments")
  if(SYMCONST(x2))
    sprintf(sbuf,"%.250s_%.250s",NAME(x1),NAME(x2));
  else if(VAR(x2))
    sprintf(sbuf,"%.250s__%ld",NAME(x1),
      (cell)((term)x2-(term)wam[HeapStk].base));
  else
    sprintf(sbuf,"%.250s_%ld",NAME(x1),OUTPUT_INT(x2));
  return INPUT_STRING(sbuf);
}

#define IBUFMAX (MAXNBUF>>2)

/* #define INT2S(X,B) if(INTEGER(X)) itoa(OUTPUT_INT(X),B,10); else strncpy(B,NAME(X),IBUFMAX); */


#define INT2S(X,B) if(INTEGER(X)) BP_LONG2STR(B,OUTPUT_INT(X)) else strncpy(B,NAME(X),IBUFMAX);

cell namecat(register term regs, register stack wam)
{ register cell x1=X(1), x2=X(2), x3=X(3);
  char sbuf[MAXNBUF];
  char ibuf1[IBUFMAX];
  char ibuf2[IBUFMAX];
  char ibuf3[IBUFMAX];
  ATOMIZE(x1);ATOMIZE(x2);ATOMIZE(x3);

  if(VAR(x1))
    ERREXIT("namecat/3: bad 1-st arg")
  if(VAR(x2))
    ERREXIT("namecat/3: bad 2-nd arg")
  if(VAR(x3))
    ERREXIT("namecat/3: bad 3-nd arg")

  INT2S(x1,ibuf1);
  INT2S(x2,ibuf2);
  INT2S(x3,ibuf3);

  sprintf(sbuf,"%s%s%s",ibuf1,ibuf2,ibuf3);
  return INPUT_STRING(sbuf);
}

#include <sys/stat.h>

bp_long fsize(FILE *cf)
{
  struct stat cbuf;
  if (-1==fstat(fileno(cf),&cbuf)) ERREXIT("bad file ar in fsize/1")
  return cbuf.st_size;
}

bp_long older_file0(char *of, char *cf)
{
  struct stat obuf, cbuf;
  if (-1==stat(cf,&cbuf)) ERREXIT("file error on 2 arg in older_file/2")
  if (-1==stat(of,&obuf)) return 1;
  return obuf.st_mtime<cbuf.st_mtime;
}

no older_file(register term regs, register stack wam)
{
  register cell xval;
  string of,cf;
  xval=X(1);
  if(VAR(xval) || INTEGER(xval))
    return (no)LOCAL_ERR(xval,"bad old file name in older_file/2");
  of=NAME(xval);

  xval=X(2);
  if(VAR(xval) || INTEGER(xval))
    return (no)LOCAL_ERR(xval,"bad new file name in older_file/2");
  cf=NAME(xval);

  return older_file0(of,cf);
}

no see_tell(register term regs, register stack wam)
{
  byte op=(byte)OUTPUT_INT(X(1));
  register cell xval=X(2);
  FILE *f; string fname;
  if(VAR(xval) || INTEGER(xval))
    return (no)LOCAL_ERR(xval,"bad file name in see or tell");
  fname=NAME(xval);
  if(0==op) /*see */
      {
        if(xval==g.user) g.seefile=STD_in;
        else
        {
          f = (FILE *) hget(g.seemark, xval);
          if ((cell) f == g.closed_file)
            {
              f = fopen(fname, "rb");g.lineno=0;
              if (!f) ERREXIT("unable to reopen to see file")
              if (!hset(g.seemark, xval, (cell)f)) ERREXIT("hset in see/1")
            }
          else if (!f)
            {
              f = fopen(fname, "rb");g.lineno=0;
              if (!f) return FALSE;
              if (!hdef(g.seemark, xval,(cell)f,g.timestamp))
                 ERREXIT("hdef in see")
            }
          /* else already opened and ok; */
          g.seefile = f;
         }
         g.seefunc = xval;
      }
  else if (1==op || 2==op) /* tell */
      {
        if(xval==g.user) g.tellfile=STD_out;
        else
        { string mode= ((2==op) ? "ab":"wb");
          f=(FILE *)hget(g.tellmark,xval);
          if((cell)f==g.closed_file)
            {
              f=fopen(fname,mode);
              if(!f) return FALSE;
              if(!hset(g.tellmark,xval,(cell)f))
                ERREXIT ("hset in tell")
            }
          else if(!f)
            {
              f=fopen(fname,mode);
              if(!f) return FALSE;
              if(!hdef(g.tellmark,xval,(cell)f,g.timestamp))
                ERREXIT("hdef in tell")
            }
          /* else already opened and ok */
          g.tellfile=f;
         }
         g.tellfunc=xval;
      }
  else
    ERREXIT("unimplemented bad see_tell operation")
  return TRUE;
}



no see_tell_at(register term regs, register stack wam)
{
  byte op=(byte)OUTPUT_INT(X(1));
  register cell xval=X(2);
  bp_long ires; no ok=TRUE;
  if(!INTEGER(xval))
    return (no)LOCAL_ERR(xval,
       "arg 1 of see_at or tell_at/1 must be integer");
  ires=OUTPUT_INT(xval);

  if (0==op) /* fseek for see */
      {
         if(-1 == fseek(g.seefile, ires, SEEK_SET)) ok=FALSE;
      }
  else if (1==op) /* fseek for tell */
      {
         if(-1 == fseek(g.tellfile, ires, SEEK_SET)) ok=FALSE;
      }
  else
      { ok = FALSE;
        ERREXIT("unimplemented see_at or tell_at operation")
      }

  return ok;
}

cell seeing_telling_at(register term regs)
{
  bp_long ires;
  byte op=(byte)OUTPUT_INT(X(1));

  if (0==op) /* fseek for seeing */
      {
         ires = ftell(g.seefile);
      }
  else if (1==op) /* fseek for telling */
      {
         ires = ftell(g.tellfile);
      }
  else
     ERREXIT("unimplemented see_at or tell_at operation")

  return INPUT_INT(ires);
}

void seen_told(byte op)
{
 switch(op) {
 case 0: /* seen */ {
  if(g.seefile!=STD_in)
    { cell f=hget(g.seemark,g.seefunc);
      if(f!=g.closed_file)
        {
          if(f) hset(g.seemark,g.seefunc,g.closed_file);
          else hdef(g.seemark,g.seefunc,g.closed_file,g.timestamp);
          fclose(g.seefile);
        }
    }
  g.seefile=STD_in;
  g.seefunc=g.user;
 }
 break;

 case 1: /* told */ {
  if(g.tellfile!=STD_out)
    { cell f=hget(g.tellmark,g.tellfunc);
      if(f!=g.closed_file)
        {
          if(f) hset(g.tellmark,g.tellfunc,g.closed_file);
          else hdef(g.tellmark,g.tellfunc,g.closed_file,g.timestamp);
          fclose(g.tellfile);
        }
    }
  g.tellfile=STD_out;
  g.tellfunc=g.user;
 }
 break;

 case 2: /* flush */
   fflush(g.tellfile);
 break;

 default:
   warnmes("unimplemented option in seen_tell");
 }
}

cell system0(register term regs, register stack wam)
{ register cell xval=X(1); bp_long ires;
  ATOMIZE(xval);
  if(!SYMCONST(xval))
    {
      (void)LOCAL_ERR(xval,"bad command in system/2");
      ires=1;
    }
  else
    {
      string s=NAME(xval);
      ires=system(s);
#if TRACE>1
      fprintf(STD_err,"system--->%ld\n",ires);
#endif
    }
  xval=INPUT_INT(ires>>8);
  return xval;
}

bp_long add_instr(register term regs, register stack code)
{       char sbuf[MAXNBUF]; string name;
                 if(INTEGER(X(3)))
                   {name=sbuf;
					 BP_LONG2STR(name,OUTPUT_INT(X(3)));
				   }
                 else
                   name=NAME(X(3));
                 if(!insert_op(
                            (no)OUTPUT_INT(X(1)),
                            (no)OUTPUT_INT(X(2)),
                            name,
                            (no)OUTPUT_INT(X(4)),
                            code /* shared[InstrStk] contains the code */
                        ))
                   return FALSE;
                 else
                   return TRUE;
}

static bp_long ocompare(register cell vl, register cell vr)
{       register bp_long ires;
  ires=(!!NONVAR(vl)<<1) | (!INTEGER(vl));
  ires-=(!!NONVAR(vr)<<1) | (!INTEGER(vr));
  if(0==ires)
  {
    if(VAR(vl)) ires=vl-vr;
    else if(INTEGER(vl))
      ires=OUTPUT_INT(vl)-OUTPUT_INT(vr);
    else
      {
        ires=strcmp(NAME(vl),NAME(vr));
        if(0==ires)
          ires=GETARITY(vl)-GETARITY(vr);
      }
  }
  return (ires>0) ? !!ires: -!!ires;
}

bp_long compare(register term l, register term r)
{       register bp_long ires;
  register cell vl,vr;
  DEREF2(l,vl); DEREF2(r,vr);

  if(NUMERIC(vl) && NUMERIC(vr))
    return float_compare(l,r);

  ires=ocompare(vl,vr);
  if(0==ires && COMPOUND(vl))
    for(vr=GETARITY(vl),vl=1; 0==ires && vl<=vr; vl++)
      ires=compare(l+vl,r+vl);
  return ires;
}

#define UNITEST(T1,T2) if(!unify(T1,T2,wam,A)) return NULL

term functor(register term H, register term t, register stack wam, register term *A)
{ register cell r,arity;
  register term f=t+1;
  register term n=t+2;
  DEREF2(t,r);
  if(NONVAR(r))
    {
      if(INTEGER(r) || !(arity=GETARITY(r)))
        {
          UNITEST(T2C(f),r);
          UNITEST(T2C(n),INPUT_INT(0));
        }
      else
        {
          UNITEST(T2C(f),PUTARITY(r,0));
          UNITEST(T2C(n),INPUT_INT(arity));
        }
    }
  else /* t is a variable */
    { register cell i;
      DEREF2(f,r);
      DEREF2(n,i);
      if(!INTEGER(i)) return NULL;
      arity=OUTPUT_INT(i);
      if(INTEGER(r))
        {
          if(0!=arity) return NULL;
          UNITEST(T2C(t),r);
        }
      else if(NONVAR(r))
        {
          register term s=H;
          if(arity>=MAXARITY)
            { fprintf(STD_err,"arity=%ld\n",arity);
              ERREXIT("functor has bad arity")
            }
          PUSHVAL(PUTARITY(r,arity));
          while(arity--) {SETREF(H,H); H++;}
          UNITEST(T2C(t),T2C(s));
        }
      else return NULL;
    }
return H;
}

static string name2buf(cell vt, string buf)
{
  if(INTEGER(vt))
    {
       BP_LONG2STR(buf,OUTPUT_INT(vt));
       return buf;
    }
  else
    {
      if(!GETARITY(vt))
        return NAME(vt);
      else
        return NULL;
    }
}

no list2buf(register term l, register cell vl, register string name,register bp_long len)
{ register term car;
  /*if(max.QUIET==INPUT_INT(0))warnmes("entering list2buf");*/
  while(IS_LIST(vl)) {
    car = ++l;
    DEREF2(car,vl);

    if(INTEGER(vl)) {
	    if(--len<2) {
		    warnmes("string buffer overflow in list2buf");
		    return FALSE;
      }
      *name++=(char)OUTPUT_INT(vl);
    }
    else
      return FALSE;

    ++l; DEREF2(l,vl);
  }
  if(g.NIL!=vl) return FALSE;

  *name='\0';
  return TRUE;
}

term string2list(register term H,register string name,register stack wam) {
   IF_OVER("string2list/2",(term *)(H+(strlen(name)<<1)),HeapStk,NO());
   while(*name)
     PUSH_LIST(INPUT_INT((bp_short)(*name++)))
   PUSH_NIL();
   return H;
}

term string2list_with_length(register term H,register string name,register stack wam,bp_long count) {
   IF_OVER("string2list/2",(term *)(H+(count<<2)),HeapStk,NO());
   while(count-->0)
     PUSH_LIST(INPUT_INT((bp_short)(*name++)))
   PUSH_NIL();
   return H;
}

term name2list(register term H, register term t, register term l, register stack wam, register term *A)
{ register cell vt,vl; register string name;

#if THREADS>0 && 10==VCC
  static // os x bug - better: use MAXNBUF
#endif
     char sbuf[MAXNBUF];

  DEREF2(t,vt); DEREF2(l,vl);

  if(NONVAR(vl) && list2buf(l,vl,sbuf,MAXNBUF)) {
      UNITEST(T2C(t),input_fun(sbuf,0));
    }
  else if(NONVAR(vt) && ( name = name2buf(vt,sbuf) )) {
      term r=H;
      H=string2list(H,name,wam);
      /* was: if(!H) return H; */
      if(!H) {warnmes("string2list overflow in name2list(...)"); return H;}

      UNITEST(T2C(r),T2C(l));
    }
  else if(VAR(vt) && VAR(vl))
      ERREXIT("both args of name/2 cannot be variables")
  else if(NONVAR(vt))
      return LOCAL_ERR(vt,"bad data in args(1) of name/2");
  else
      return LOCAL_ERR(vl,"bad data in args(2) of name/2");
  return H;
}

/* call C function named in X(1) here which gets its input from sbuf
and puts back to it its result - the function returns 0 to signal
fail and non-zero to signal success

NOTE: we have no means to check here if the address provided makes sense
as a function. This is FULLY the users responsbility - usually the user
should call BinProlog first to send the address of this callback function
- for instance after getting it from a DLL -

buf works as an bidirectional communication area:
its content is processed by the called function,
which is responsable to replace it with the returned
value  (buf being of type char *), as well as for
signaling success or failure to BinProlog. This is quite fast -
We can do about 160K such calls per sec on a 500MHz PIII.
*/

term call_external(register term H, register term regs, register stack wam) {
  register term xref;
  register cell xval;
  bp_long ires;
  char buf[MAXNBUF];

  if(!INTEGER(X(1)) || NULL==INT2PTR(X(1)))
    return LOCAL_ERR(X(1),"bad first arg (function address) in call_external/3");

  xref=RX(2);

  if(g.NIL==T2C(xref)) {
    buf[0]='\0';
    xval=(cell)xref;
  }
  else {
    if(NONVAR(xref))
      return LOCAL_ERR(T2C(xref),"list of chars second arg expected in call_external/3");

    xval=GETCELL(xref);
    if(FALSE==list2buf(xref,xval,buf,MAXNBUF))
      return LOCAL_ERR(xval,"char list second arg expected in call_external/3");
    }

  xref=INT2PTR(X(1));

  ires=SFUNCALL(xref,(buf));
  if(!ires) return NULL;

  xref=H;
  if(!(H=string2list(H,buf,wam)))
    return LOCAL_ERR(xval,"bad data returned in call_external/3");

  return H;
}


term det_append0(register term H, register term regs, register stack wam)
{
  register term xref;
  register cell xval;
  register cell cons;
  term car;

  PUSHVAL(g.DIF);
  car=H++;
  xref=C2T(X(1));
#if(TRACE > 1)
  fprintf(STD_err,"det_append0   : base=%ld H=%ld margin=%ld end=%ld\n",
    wam[HeapStk].base,H,wam[HeapStk].margin,wam[HeapStk].end);
#endif
  if(VAR(T2C(xref)))
    { cons=xval=GETREF(xref);
      if(NONVAR(xval))
        { register bp_long hmax =((term)wam[HeapStk].margin-H)>>1; /* should be 1/2 as we build 2!!! */

          if(hmax<1) hmax=1; /* has to iterate at least once, anyway, bug otherwise */
          if(2!=GETARITY(cons))
            return LOCAL_ERR(cons,
              "bad constructor starts 1st arg of det_append/3");

#if(TRACE > 1)
  fprintf(STD_err,"det_append0 bf: base=%ld H=%ld margin=%ld end=%ld\n",
    wam[HeapStk].base,H,wam[HeapStk].margin,wam[HeapStk].end);
#endif
          do
            {

#if(TRACE > 1)
              if(!INSPACE(H,(term)wam[HeapStk].base,(term)wam[HeapStk].end)) {
                fprintf(STD_err,"det_append0 do: base=%ld H=%ld margin=%ld end=%ld\n",
                  wam[HeapStk].base,H,wam[HeapStk].margin,wam[HeapStk].end);
                 ASSERT2(INSPACE(H,(term)wam[HeapStk].base,(term)wam[HeapStk].margin),H);
              }
#endif
              H[0]=cons;
#if EAGER_DEREF>1
              { register term xcar;
                FDEREF3(xref+1,xcar,xval);
                if(COMPOUND(xval)) H[1]=T2C(xcar);
                else H[1]=xval;
              }
#elif EAGER_DEREF>0
              xval=xref[1];
              if(COMPOUND(xval)) H[1]=T2C(xref+1);
              else H[1]=xval;
#else
              H[1]=T2C(xref+1);
#endif
              H+=2;
              xref+=2;
              DEREF2(xref,xval);
            }
          while(cons==xval && --hmax);
        }
    }
  PUSHVAL(X(2));
  SETREF(car,xref);
  return H;
}

term list2term(register term H, register term regs, register stack wam)
{
  register term xref;
  register cell xval;
  register cell cons;
  xref=C2T(X(1));
  if(VAR(T2C(xref)))
    { term t=H++; term f=xref+1; bp_long arity=0;
      cons=GETREF(xref);
      if(2!=GETARITY(cons))
        return LOCAL_ERR(cons,
          "bad constructor starts 1st arg of list2term/2");
      xref += 2;
      DEREF2(xref,xval);
      while(cons==xval)
         { arity++;
           IF_OVER("list2term/2",(term *)H,HeapStk,NO());
           PUSHVAL(T2C(xref+1));
           FDEREF(T2C(xref+2));
         }
      if(arity>=MAXARITY) ERREXIT("list too big: 1st arg of list2term")

      if(!ATOMIC(xval))
        return LOCAL_ERR(xval,
          "atomic terminator should end 1st arg of list2term/2");
      FDEREF(T2C(f));
      if(VAR(xval) || (INTEGER(xval) && arity))
        return LOCAL_ERR(xval,
          "bad functor in 1st arg of list2term/2");
      if(IDENTIFIER(xval))
        xval=PUTARITY(xval,arity);
      SETCELL(t,xval);
    }
  else
    return LOCAL_ERR(T2C(xref),
      "unexpected 1st arg of list2term/2");
  return H;
}

term term2list(register term H, register term regs, register stack wam)
{
  register term xref;
  register cell xval;
  register cell cons=X(2); cell nil=X(3);
  register bp_long arity,i;
  if(!IDENTIFIER(cons)) return NULL;
  cons=PUTARITY(cons,2);
  xref=C2T(X(1));
  if(VAR(T2C(xref)))
    { register term t=xref;
      xval=GETREF(xref);
      if(VAR(xval)) return NULL;
      arity=GETARITY(xval);
      H[0]=cons; H[1]=PUTARITY(xval,0); H+=2;

      for(i=1; i<arity; i++)
        {
           H[0]=cons; H[1]=t[i]; H+=2;
        }
      if(arity) {H[0]=cons; H[1]=T2C(t+i); H+=2;}
    }
  else
    { H[0]=cons; H[1]=T2C(xref); H+=2; }
  PUSHVAL(nil);
  return H;
}

term term_append(register term H, register term regs, register stack wam)
{
  register term t=RX(1),c=RX(2);
  register cell val_t,val_c,arity_t,arity_c,arity;
  val_t=NONVAR((cell)t) ? (cell)t : GETREF(t);
  if(!IDENTIFIER(val_t)) return NULL;
  val_c=NONVAR((cell)c) ? (cell)c : GETREF(c);
  if(!IDENTIFIER(val_c)) return NULL;
  arity_c=GETARITY(val_c);
  arity_t=GETARITY(val_t);
  arity=arity_c+arity_t;
  if(arity>=MAXARITY) return NULL;
  SETCELL(H,PUTARITY(val_t,arity));
  if(arity_t) COPY_ARGS(H,t,arity_t);
  H+=arity_t;
  if(arity_c) COPY_ARGS(H,c,arity_c);
  H+=arity_c+1;
  IF_OVER("term_append/3",(term *)H,HeapStk,NO());
  return H;
}

#define VALUE_TRAIL_IT(V,T) \
        {       IF_OVER("value_trailing",TR,TrailStk,NO()); \
                ASSERT2(!INSPACE((T),from,to), (T)); \
                TR[0]=(T);\
                TR[1]=((term)V);\
                TR+=2;\
        }

#define UNWIND_VALUE_TRAIL(New,Old) \
{   while(Old<New) \
    {   register term value; \
                New--; \
                value = *New; \
                New--; \
                SETREF(*New,value); \
    } \
}

/* this function is written by Ulrich Neumerkel */
/* slightly modified by Paul Tarau */

static term copy_term0(register term h,register term t0,register term from,register term to,register stack wam)
{ register term ct = h;
  register term *TR=TR_TOP;
  term *bakTR=TR;
  if(h>=to) return NULL;
  ASSERT2(INSPACE(h,from,to),h);
  ASSERT2(!ATOMIC(T2C(t0)),t0);
  SETREF(h++,t0);
  do
    { term t; cell val_t;
      ASSERT2(INSPACE(ct,from,h),ct);
      t = ct; DEREF2(t,val_t);
      if (t == ct)
        { ASSERT2(NONVAR(val_t) || INSPACE(C2T(val_t), from, h), val_t);
        }
      else if (ATOMIC(val_t)) SETCELL(ct,val_t);
      else if (INSPACE(t,from,h)) SETREF(ct,t);
      else if (VAR(val_t))
        { ASSERT2(VAR(val_t),val_t); Comment("Old var");
          VALUE_TRAIL_IT(val_t,t); SETREF(ct,ct); SETREF(t,ct);
        }
      else
        { ASSERT2(COMPOUND(val_t),t);
          ASSERT2(!INSPACE(t,from,to),t);
          SETREF(ct,h); Comment("Old structure");
          do
            { cell arity;
              VALUE_TRAIL_IT(val_t,t);
              SETCELL(h,val_t); SETREF(t,h);
              arity=GETARITY(val_t);
              if(h>to) {h = NULL; goto untrail;}
              COPY_CELLS(h,t,arity-1); Comment("Copying inner args");
              h += arity;
              t += arity;
              DEREF2(t,val_t);
            }
          while (COMPOUND(val_t) && !INSPACE(t,from,h));
          Comment("Copying last arg:");
          if (COMPOUND(val_t)) SETREF(h++,t);
          else SETCELL(h++,val_t);
          Comment("End of consecutive cells");
        }
      ct++;
    }
  while (ct < h);
  ASSERT2(ct == h,ct);
  untrail: UNWIND_VALUE_TRAIL(TR,bakTR); return h;
}

term copy_term(register term t0, register term from, register term to, register stack wam) {
  return copy_term0(from, t0, from, to, wam);
}

term copy_to_engine(register stack wam, register term t)
{
   term h=(term)wam[HeapStk].top;
   term from=h;
   term to=(term)wam[HeapStk].margin;
   if(ATOMIC(t)) {SETREF(h,t); h++;}
   else
     {
       h=copy_term(t,from,to,wam);
       if(!h) return NULL;
     }
   wam[HeapStk].top=(term *)h;
   return from;
}

term apply(register term H, register term regs, register cell fun, register stack wam)

                     /* the functor to apply */

{
  register term xref; register bp_long arity;

  arity=GETARITY(fun)-1; fun=PUTARITY(fun,arity);

#if 0
   fprintf(STD_err,"!!! apply: %s/%d\n", NAME(fun),arity);
#endif

  xref=H; PUSHVAL(fun);
  COPY_CELLS(xref,regs,arity);
  regs[2]=regs[arity+1];
  regs[1]=T2C(xref);
  return H+arity;
}

term strip_cont0(register term H, register term regs, register stack wam)
{
  register term xref,t; register bp_long arity;
  register cell fun;
  t=C2T(X(1));
  if(NONVAR(t))
    return LOCAL_ERR(T2C(t),"bad continuation");
  fun=GETREF(t);
  if(!COMPOUND(fun))
    return LOCAL_ERR(T2C(t),"compound continuation expected");
  arity=GETARITY(fun);
  PUSHVAL(g.DIF);
  PUSHVAL((cell)(t+arity--)); /* should _point_ to the continuation !!!*/
  fun=PUTARITY(fun,arity);
#if 0
/* source of a VERY nasty bug: PUSHVAL(t[arity--]); */
fprintf(STD_err,"strip_cont0: %s/%d\n",NAME(fun),arity);
#endif
  xref=H;
  PUSHVAL(fun);
  COPY_CELLS(xref,t,arity);
  return H+arity;
}

bp_long dcg_tell(register term regs, register stack wam, register term *A)
{ cell xval; bp_long ires; bp_long ok=1;

  xval=X(1);
  if(!INTEGER(xval)) ok=0;
  ires=OUTPUT_INT(xval);
  if(!(ires > 0 && ires < MAXDCG)) ok=0;
  if(!ok) (void)LOCAL_ERR(xval,"bad argument for dcg_stream");
  else
    { term g_connect=DCGSTART();
#ifndef NO_VALUE_TRAIL
      xval=GETREF(g_connect);
      SMART_VTRAIL_IF(g_connect,xval);
#endif
      SETREF(g_connect,g_connect+ires);
    }
  return ok;
}

term dcg_connect(register term H, register term regs, register stack wam, register term *A)
{
                      cell xval,head,tail; term xref,new,g_connect=DCGSTART();

/* BUG: gc crashes on collecting dcg-connect generated cells */
/* however, the bug seems to be Visual C/C++ specific */


/*
IF_OVER("dcg_connect/2",(term *)H,HeapStk,NO());

fprintf(STD_err,".");
*/
                      xref=C2T(GETREF(g_connect));
#if defined(NO_VALUE_TRAIL)
                      TRAIL_IT(xref);
#else
                      xval=GETREF(xref);
                      SMART_VTRAIL_IF(xref,xval);
#endif
                      new=H;
                      MAKE_LIST();
                      NEWVAR(head);
                      NEWVAR(tail);
                      UNIFAIL(T2C(new),xval);
                      xref=C2T(GETREF(g_connect));
                      SETREF(xref,tail);
                      X(0)=head;
                      return H;
}

no setarg(register term regs, register stack wam, register term *A)
{
  register term xref;
  register cell xval;
  register bp_long ires;

                      xval=X(1);
                      if(!INTEGER(xval))
                        return (no)LOCAL_ERR(xval,
                           "setarg/3's 1st arg must be integer");
                      ires=OUTPUT_INT(xval);

                      xref=C2T(X(2));
                      if(ATOMIC(T2C(xref)) )
                        return (no)LOCAL_ERR(T2C(xref),
                            "setarg/3's 2nd arg cannot be atomic");
                      ASSERT2(VAR(T2C(xref)),xref);
                      xval=GETREF(xref);
                      if(VAR(xval))
                        return (no)LOCAL_ERR(xval,
                           "setarg/3's 2nd arg cannot be unbound");
                      if(ires<=0 || (no)ires>GETARITY(xval))
                        return (no)LOCAL_ERR(xval,
                           "setarg/3's 1st arg must be in 1..arity");
                      xref+=ires;

#if defined NO_VALUE_TRAIL
                      TRAIL_IF(xref);
                      SETREF(xref,X(3));

#elif defined SIMPLE_VALUE_TRAIL
                      xval=GETREF(xref);
                      VTRAIL_IF(xref,xval);
                      SETREF(xref,X(3));
#else
                      {register term newarg;
                       /* cell X2=xval; */
                       FDEREF3(X(3),newarg,xval);
                       if(newarg!=xref)
                          {
                            xval=GETREF(xref); /* xref is the location to be replaced */
                            if(xval==(cell)xref) {
                              /* to THINK: is this really UNSOUND?
                                 LOCAL_ERR(X2,
                                   "setarg/3: attempt to set unbound arg");
                              */
                              TRAIL_IF(xref); /* just conditionally trail ! */
                              SETREF(xref,newarg); /* set location */
                            }
                            else {
                              SMART_VTRAIL_IF(xref,xval);
                              SETREF(xref,newarg); /* set to derefed value! */
                            }
                          }
                      }

#endif

                      /* should be derefed and if == not done ?

                      */
                      return 1;

}

no change_arg(register term regs, register stack wam, register term *A)
{
  register term xref;
  register cell xval;
  register bp_long ires;

                      xval=X(1);
                      if(!INTEGER(xval))
                        return (no)LOCAL_ERR(xval,
                           "change_arg/3's 1st arg must be integer");
                      ires=OUTPUT_INT(xval);

                      xref=C2T(X(2));
                      if(ATOMIC(T2C(xref)) )
                        return (no)LOCAL_ERR(T2C(xref),
                            "change_arg/3's 2nd arg cannot be atomic");
                      ASSERT2(VAR(T2C(xref)),xref);
                      xval=GETREF(xref);
                      if(VAR(xval))
                        return (no)LOCAL_ERR(xval,
                           "change_arg/3's 2nd arg cannot be unbound");
                      if(ires<=0 || (no)ires>GETARITY(xval))
                        return (no)LOCAL_ERR(xval,
                           "change_arg/3's 1st arg must be in 1..arity");
#if 0
                      if(xref>SAVED_H)
                        return (no)LOCAL_ERR(xval,
                           "change_arg/3's 2nd arg found unsafe");
#endif
                      xref+=ires;

                      xval=X(3);
#if 0
                      if(VAR(xval) && xval>(cell)SAVED_H)
                        return (no)LOCAL_ERR(xval,
                           "change_arg/3's 3rd arg found unsafe");
#endif
                      SETREF(xref,xval);
                      return 1;
}

term bp_heap_top=NULL;

cell bp_cons(term hd, term tl)
{ register term t=bp_heap_top;
  SETCELL(t,g.DOT);
  SETREF(t+1,hd);
  SETREF(t+2,tl);
  bp_heap_top+=3;
  return (cell)t;
}

#ifdef BUILTIN_MEMBER
extern term *unwind_trail(register term *new0, register term *old);

  /* written by Bart Demoen Thu Aug 10 10:32:10 ADT 1995 */
  /* Paul Tarau Fri Aug 11 10:01:17 ADT 1995 - made standalone function*/

#define member_ARITY 2
term member_entry(register term H, register term regs, register stack wam, register term *A)
{
  register term xref; register cell xval,fields;
  static term oldA; /* this will be always the second clause of member*/

   if(g.member_entry != (instr)SAVED_P)
     { oldA = SAVED_P;
       SAVED_P = (term)g.member_entry;
     }
   else
     {
       TR_TOP=unwind_trail(TR_TOP,(term *)SAVED_TR);
       H=SAVED_H;
     }

   fields = T2C(SAVED_An(member_ARITY,1)); /* arg 1 of member */
   xval = T2C(SAVED_An(member_ARITY,2));   /* arg 2 of member */

   do
        {
          FDEREF(xval);
          if(xval != g.DOT)
            { if(VAR(xval)) {SAVED_P=oldA;wam[ChoiceStk].top=A;}
              else wam[ChoiceStk].top=(term*)SAVED_lastP(member_ARITY);
              return NULL;
            }
          xval = T2C(xref+2);
         }

   while(!unify(fields,xref[1],wam,A) &&
         (TR_TOP=unwind_trail(TR_TOP,(term *)SAVED_TR))
   );

   SAVED_An(member_ARITY,2) = C2T(xval);
   regs[1] = T2C(SAVED_Cont);
   return H;
}

#define for_ARITY 3
term for_entry(register term H, register term regs, register stack wam, register term *A)
{
   register term xref; register cell i,from,to;

   if(g.for_entry != (instr)SAVED_P)
     {
       wam[ChoiceStk].top=A;

       FDEREF3(SAVED_An(for_ARITY,1),xref,i);
       if(NONVAR(i)) return NULL;
       SAVED_An(for_ARITY,1)=C2T(i);

       FDEREF3(SAVED_An(for_ARITY,2),xref,from);
       if(!INTEGER(from)) return NULL;
       SAVED_An(for_ARITY,2)=C2T(from);

       FDEREF3(SAVED_An(for_ARITY,3),xref,to);
       if(!INTEGER(to)) return NULL;
       SAVED_An(for_ARITY,3)=C2T(to);

       SAVED_P = (term)g.for_entry;

#if 1
       TRAIL_IT(C2T(i)); /* added for optimisation by Bart */
       SAVED_TR++ ; /* added for optimisation by Bart */
#endif

     }
   else
     {
       TR_TOP=unwind_trail(TR_TOP,(term *)SAVED_TR);
       H=SAVED_H;
       i = T2C(SAVED_An(for_ARITY,1)); /* arg 1 of for */
       from = T2C(SAVED_An(for_ARITY,2));   /* arg 2 of for */
       to = T2C(SAVED_An(for_ARITY,3));   /* arg 3 of for */
     }
   if(OUTPUT_INT(from) > OUTPUT_INT(to))
     {
        wam[ChoiceStk].top=(term*)SAVED_lastP(for_ARITY);
        return NULL;
      }
    SETREF(i,from);

#if 0
    TRAIL_IT(C2T(i)); /*  removed for optimisation by Bart */
#endif

    from=INPUT_INT(OUTPUT_INT(from)+1);
    SAVED_An(for_ARITY,2)=C2T(from);

   regs[1] = T2C(SAVED_Cont);
   return H;
}

#endif

#define QHEAD (queue[1])
#define QTAIL (queue[2])

#define QNEW(Stack) \
 if(FREEVAR(T2C(queue))) \
  {\
    RELOCATE_TO_BB(elem) \
    SETREF(queue,H);\
    H[0]=g.DIF;\
    H[1]=T2C(H+3);\
    H[2]=T2C(H+5);\
    H[3]=g.DOT;\
    H[4]=elem;\
    H[5]=T2C(H+5);\
    H+=6;\
    (Stack).top=(term *)H;\
    return TRUE; \
  }

#define QCHECK(Stack) \
ASSERT2(ONSTACK(Stack,QTAIL),QTAIL) \
if(! (VAR(queue) && g.DIF==GETCELL(queue)) ) \
  BP_ERROR(BAD_QUEUE,"BAD_QUEUE",queue,NULL,0)

#define QINIT(Stack) \
  QNEW(Stack) \
  QCHECK(Stack)

#define COPY_ELEM(Elem,To,Error) \
if(VAR(Elem)) \
{ term xref=copy_term(C2T(Elem),H,(To),wam); \
  if(!xref) Error; \
  (Elem)=T2C(H); \
  H=xref; \
}

#define RELOCATE_TO_BB(Elem)\
COPY_ELEM((Elem),maxH,BP_ERROR(BB_OVERFLOW,"BB_OVERFLOW",queue,(Elem),0))

#define RELOCATE_TO_TARGET(TargetStk,Elem) \
{ register cell xval=T2C(Elem); \
  register term H=(term)wam[TargetStk].top; \
    COPY_ELEM(xval,(term)wam[TargetStk].margin, \
    BP_ERROR(HEAP_OVERFLOW,"HEAP_OVERFLOW",queue,xval,0)) \
  wam[TargetStk].top=(term*)H; \
  (Elem)=C2T(xval); \
}

no addq0(register stack wam,
     register term queue,register cell elem)
{
  register term H=(term)g.shared[BBoardStk].top;
  register term maxH=(term)g.shared[BBoardStk].margin;
  register cell tail;
  if(H>maxH) BP_ERROR(BB_OVERFLOW,"BB_OVERFLOW",queue,elem,0)
  QINIT(g.shared[BBoardStk])
  RELOCATE_TO_BB(elem)
  tail=QTAIL;
  DEREF1(tail);
  if(!FREEVAR(tail))
     BP_ERROR(BAD_QUEUE,"BAD_TAIL_IN_addq0",queue,NULL,0)
  if(tail==T2C(queue+2))
  {  /* fixes pointers reversed after breadth-first copy_term
        as [...|Xs]-Ys with Xs --> pointing to --> selfref Ys is BAD
     */
#if 0
     DEREF1(QHEAD);
     DEREF1(QTAIL);
#endif
#if TRACE>1
     /* ERRMES("$$$ queue",T2C(queue)); */
     ERRMES("$$$ QELEM",T2C(C2T(QHEAD)+1));
     ERRMES("$$$ QTAIL",QTAIL);
#endif
     H[0]=g.DIF;      /* reconstructs the queue instead of */
     H[1]=QHEAD;      /*          traversing it to the end */
     H[2]=QTAIL;      /* points to old tail */
#if 0
     QHEAD=T2C(H+1);  /* redirects old head */
     QTAIL=T2C(H+2);  /* redirects old tail now a var */
#endif
     SETREF(queue,H); /* redirects old queue now a var pointing to H*/
     queue=H;         /* initializes new queue */
     H+=3;
  }
  ASSERT2(ONSTACK(g.shared[BBoardStk],tail),tail);
  SETREF(tail,T2C(H));
  H[0]=g.DOT;
  H[1]=elem;
  QTAIL=H[2]=T2C(H+2);
  H+=3;
  g.shared[BBoardStk].top=(term*)H;
  return TRUE;
}


no pushq0(register stack wam,
     register term queue,register cell elem)
{
  register term H=(term)g.shared[BBoardStk].top;
  register term maxH=(term)g.shared[BBoardStk].margin;
  if(H>maxH) BP_ERROR(BB_OVERFLOW,"BB_OVERFLOW",queue,elem,0)
  QINIT(g.shared[BBoardStk])
  RELOCATE_TO_BB(elem)
  H[0]=g.DOT;
  H[1]=elem;
  DEREF1(QHEAD);
  ASSERT2(ONSTACK(g.shared[BBoardStk],QHEAD),QHEAD);
  H[2]=QHEAD;
  QHEAD=T2C(H);
  H+=3;
  g.shared[BBoardStk].top=(term*)H;
  return TRUE;
}

term popq0(no newstk,register stack wam,
  register term queue)
{ term elem;
  QCHECK(g.shared[BBoardStk])
  DEREF1(QHEAD);
  if(NONVAR(QHEAD)) BP_ERROR(BAD_QUEUE,"BAD_QUEUE",queue,NULL,0)
  ASSERT2(ONSTACK(g.shared[BBoardStk],QHEAD),QHEAD);

  DEREF1(QTAIL);
  if(NONVAR(QTAIL)) BP_ERROR(BAD_QUEUE,"BAD_QUEUE",queue,NULL,0)
  ASSERT2(ONSTACK(g.shared[BBoardStk],QTAIL),QTAIL);

  if(QHEAD==QTAIL) return NULL; /* empty but otherwise valid */
  ASSERT2(g.DOT==GETREF(C2T(QHEAD)),GETREF(C2T(QHEAD)))
  elem=C2T(GETREF(C2T(QHEAD)+1));
  { cell next=T2C((C2T(QHEAD)+2));
    DEREF1(next);
    QHEAD=next;
  }
  RELOCATE_TO_TARGET(newstk,elem);
  return elem;
}


term member_scan(register cell x,register cell xval,
                 register stack wam,register term *A)
{
  register term xref;
  register cell cons;
  term *oldtrail=TR_TOP;
  if(VAR(xval))
    { xref=C2T(xval);
      cons=xval=GETREF(xref);
      if(NONVAR(xval))
        {
          if(2!=GETARITY(cons))
            return LOCAL_ERR(cons,
              "bad constructor starts 2nd arg of member_scan/3");
          do
            {
              xval=unify_to(T2C(xref+1),T2C(x),wam,A);
              TR_TOP=unwind_trail(TR_TOP,oldtrail);
              if(xval)
                return xref;
              xref+=2;
              DEREF2(xref,xval);
            }
          while(cons==xval);
        }
    }
  return NULL;
}

term cmember_scan(register term H,register cell x,
     register cell xval,register stack wam,register term *A, no *ok)
{ register term xref,t=H; /* new constructions start here */
  if(!INTEGER(xval)) {*ok=FALSE; return NULL;}
  xval=T2C(INT2PTR(xval));
  DEREF1(xval);
  if(!(xref=member_scan(x,xval,wam,A))) return NULL;
  ASSERT1(t==H);
  H+=3;
  x=T2C(H);
  if(!(H= copy_term(xref+1,H,(term)wam[HeapStk].margin+
   (wam[HeapStk].over>>1),wam)))
      {*ok=FALSE; return NULL;}
  t[0]=xref[0];
  t[1]=x;
  xref+=2;
  DEREF2(xref,xval);
  ASSERT1(VAR(xref));
  t[2]=PTR2INT(xref);
  return H;
}

term cdel_scan(register term H,register cell x,
     register cell xval,register stack wam,register term *A, no *ok)
{ register term xref,t=H; /* new constructions start here */
  if(!INTEGER(xval)) {*ok=FALSE; return NULL;}
  xval=T2C(INT2PTR(xval));
  DEREF1(xval);
  if(!(xref=member_scan(x,xval,wam,A))) return NULL;
  ASSERT1(t==H);
  H+=3;
  x=T2C(H);
  if(!(H= copy_term(xref+1,H,(term)wam[HeapStk].margin+
   (wam[HeapStk].over>>1),wam))) {*ok=FALSE; return NULL;}
  t[0]=xref[0];
  t[1]=x;
  { register term tail=xref+2;
    DEREF2(tail,xval);
    ASSERT1(VAR(tail));
    t[2]=PTR2INT(tail);
    SETREF(xref,tail);
  }
  return H;
}
