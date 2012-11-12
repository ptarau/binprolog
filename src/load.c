#include "defs.h"
#include "global.h"
#include "string.h"

#include <ctype.h>

extern struct specsyms g;
extern struct limit max;
extern string *atomtable;
extern bp_long opcount;
extern no hget(register no pred, register no fun),hset(register no pred, register no fun, register no val),hdef(register no pred, register no fun, register no val, byte stamp);
extern cell new_func(string name, no argctr);
extern void quietmes(string format, string mes);
extern void debugmes(string format, string mes);
extern byte *make_byte_array(bp_long a_size);

byte *instr_len;

extern term local_error (cell xval, string Msg, register stack wam);
extern void warnmes (string mes);
extern void fatal_error (string mes);
extern void overflow_by (term *Top, bp_long LimitNo, stack wam, string culprit);
extern bp_long make_op (cell op, cell assoc, cell pri, byte stamp);

void init_instr_len(void)
{   bp_long i;

    instr_len=make_byte_array(MAXOP);

    for(i=0; i<MAXOP; i++) instr_len[i]=1;

    instr_len[BEGIN_C_CHUNK]        =2;

    instr_len[GET_STRUCTURE]        =2;
    instr_len[PUT_STRUCTURE]        =2;
    instr_len[PUSH_STRUCTURE]       =2;

    instr_len[UNIFY_CONSTANT]       =2;

    instr_len[WRITE_CONSTANT]       =2;

    instr_len[GET_CONSTANT]          =2;
    instr_len[PUT_CONSTANT]          =2;
    instr_len[LOAD_CONSTANT]        =2;

    instr_len[EXECUTE]               =2;
    instr_len[APPLY]                 =2;

    instr_len[PROCEED]               =2;
    instr_len[END]                  =2;

    instr_len[TRY_ME_ELSE]          =2;
    instr_len[RETRY_ME_ELSE]        =2;
    instr_len[TRUST_ME_ELSE]        =2;
    instr_len[TRY_ME_ONLY]          =2;

    instr_len[SWITCH]               =2;
    instr_len[JUMP_IF]              =2;
    instr_len[NONDET]               =2;

#if JUMP_COMPRESS
    instr_len[EXEC_TRY]             =2;
    instr_len[EXEC_SWITCH]          =2;
    instr_len[EXEC_JUMP_IF]         =2;
#endif

#if STRUCT_COMPRESS
    instr_len[GET_UNIFY_VAR_VAR]    =2;
    instr_len[GET_UNIFY_VAL_VAL]    =2;
    instr_len[GET_UNIFY_VAR_VAL]    =2;
    instr_len[GET_UNIFY_VAL_VAR]    =2;

    instr_len[PUT_WRITE_VAR_VAR]    =2;
    instr_len[PUT_WRITE_VAL_VAL]    =2;
    instr_len[PUT_WRITE_VAR_VAL]    =2;
    instr_len[PUT_WRITE_VAL_VAR]    =2;

    instr_len[PUSH_VAR_VAR]    =2;
    instr_len[CONSTANT_VAR_VAR]    =2;
    instr_len[PUSH_VAL_VAR]    =2;
    instr_len[CONSTANT_VAL_VAR]    =2;

    instr_len[PUSH_VAL_VAL]    =2;
    instr_len[CONSTANT_VAL_VAL]    =2;
    instr_len[PUSH_VAR_VAL]    =2;
    instr_len[CONSTANT_VAR_VAL]    =2;
#endif

}

static no get_one_int(char *name, bp_long *ip)
{ register bp_long val=0;
  register char c=*name;
  no neg;
  if((neg=('-'==c))) c= *++name;
  if(!c) return 0;
  for( ; isdigit(c); c= *++name)
    val=(val<<3)+(val<<1)+(c-'0'); /* efficient val*10 */
  *ip=(neg)?(-val):val;
  /* if(c && neg) fprintf(STD_err,"NEG: %ld\n",*ip); */
  return !c;
}


cell input_fun(string name, no arity)
{
  bp_long val; cell f;
  if(get_one_int(name,&val))
     f=INPUT_INT(val);
  else
    f=new_func(name,arity);
  return f;
}

#if STRUCT_COMPRESS
#define MAGIC GET_UNIFY_VAR_VAR
#else
#define MAGIC GET_STRUCTURE
#endif

static instr link_switch(instr p, no doit)
{   instr label=GETLABEL(p+2);
    instr jlabel=label;
    if(label && TRUST_ME_ELSE==GETOP(label) &&
      MAGIC==GETOP(label+2))
      {
        if(doit)
          {
            SETOP(p,JUMP_IF); label+=2; SETLABEL(p,label);
          }
        return jlabel;
      }
    return NULL;
}

#define MAX_FAST_CALL_ARGS 6

no is_call(cell fun)
{
  return GETARITY(fun)>(MAX_FAST_CALL_ARGS+1)
      && PUTARITY(fun,0)==new_func("call",0);
}

no is_dynamic(cell f_plus)
{ cell f=PUTARITY(f_plus, GETARITY(f_plus)-1);
  cell v;
  no ok=
  ((v=hget(g.current_db,f)) && g.empty!=v)
  /* || NONVAR(hget(g.assumed,f)) #RISKY */
  || is_call(f)
  || hget(g.bp_virtual,f);
#if TRACE>3
  if(ok) fprintf(STD_err,"!!! dynamic %s/%ld\n",NAME(f),GETARITY(f));
#endif
  return ok;
}

/* links from p to ctop */
static no link_code(register instr p, register stack wam)
{ register instr label; register cell f; register bp_long l=1;
  for(; p<(instr)ctop; p+=l)
    { l=INSTR_LEN(p);
      switch(GETOP(p))
        {

          case BEGIN_C_CHUNK:
          if(g.inC)
           {
            if(LOADTIME==g.timestamp && g.self) /* i.e. only for C-code */
             {
                while( END_C_CHUNK!=GETOP(p+l) && (IDENTIFIER(p[l]))
                     ) l++;
              if(END_C_CHUNK!=GETOP(p+l))
              {  int i;
                 (void)LOCAL_ERR(T2C(p+l),"expected END_OF_CHUNK");

                 for(i=2;i<=l+4;i++)
                   if(IDENTIFIER(p[i]))
                        fprintf(STD_err,"%s/%ld\n",
                          NAME(p[i]),GETARITY(p[i]));
                   else if(INTEGER(p[i]))
                      fprintf(STD_err,"unexpected integer=%ld\n",
                              OUTPUT_INT(p[i]));
                   else
                      fprintf(STD_err,"opcode?=%ld\n",
                             GETOP(p+i));
                 fprintf(STD_err,"\n");
              }
              ASSERT2(END_C_CHUNK==GETOP(p+l),p[l]);
             }
            }
           else
             {
               (void)LOCAL_ERR(T2C(p),"!inC: unexpected BEGIN_C_CHUNK");
              }
          break;
          case EXECUTE: /* we look to what's next ## */
          {
            f=GETFUN(p);

            if(is_dynamic(f))
              {SETOP(p,APPLY);}
            else if(!(label=GETPRED(f))) {
               SETOP(p,APPLY);
               if(QLEVEL()<1)
                 (void)LOCAL_ERR((PUTARITY(f, GETARITY(f)-1)),
                   "has no compiled definition: left unlinked");
              }
            else
             {
               switch(GETOP(label))
                 {
                  case TRUE_0:
                    SETOP(p,TRUE_0); SETREG(p,1);
                    SETOP(p+instr_len[TRUE_0],NOP);
                    SETREG(p+instr_len[TRUE_0],0);
                  break;

                  case CALL_1:
                    SETOP(p,CALL_1); SETREG(p,1);
                    SETOP(p+instr_len[CALL_1],NOP);
                    SETREG(p+instr_len[CALL_1],0);
                  break;

                  case NONDET:
#if JUMP_COMPRESS
                   if(!is_dynamic(f))
                   {
                      /* MARK_JCOMPRESSED(f); */
                      SETOP(p,EXEC_TRY);
                   }
#endif
                    SETLABEL(p,label+instr_len[NONDET]);
                  break;

#if JUMP_COMPRESS
                  case TRY_ME_ELSE:
                   if(!is_dynamic(f))
                   {
                    /* MARK_JCOMPRESSED(f); */
                       SETOP(p,EXEC_TRY);
                    }
                    SETLABEL(p,label);
                  break;

                  case JUMP_IF:
                   if(!is_dynamic(f))
                   {
                     MARK_JCOMPRESSED(f);
                     SETOP(p,EXEC_JUMP_IF);
                    }
                    SETLABEL(p,label);
                  break;

                  case SWITCH:
                   if(!is_dynamic(f))
                   {
                     MARK_JCOMPRESSED(f);
                     if(link_switch(label,FALSE))
                       { SETOP(p,EXEC_JUMP_IF);}
                     else
                       { SETOP(p,EXEC_SWITCH);}
                    }
                    SETLABEL(p,label);
                  break;
#endif
                  default: SETLABEL(p,label);
                 }   /* switch GETOP(label) */
               } /* else */
         }
         break;

         case SWITCH: (void)link_switch(p,TRUE);
         break;

         case NONDET:
           label=GETPRED(GETFUN(p));
           SETPRED(GETFUN(p),(no)(label+instr_len[NONDET]));
           SETOP(label,TRY_ME_ONLY);
         break;

         default: if(0==l) l=1;
      } /* switch GETOP(P) */
    } /* for */
  return TRUE;
}

static cell currpred;
static no badcode;
/* g.linking; flags the fact that we are in an unsound state */

static instr last;
static no prev_prev_len,prev_len;

#define SKIPBAD() if(badcode) return FALSE

#if STRUCT_COMPRESS==0
#define OCOMPRESS(Simple,Double,First,Triple)
#define PCOMPRESS(Simple,Double,First,Triple,F2,T2,F3,T3)
#else
#define OCOMPRESS(Simple,Double,First,Triple) \
if(1==prev_len && Simple==GETOP(ctop-1)) \
  {       SETOP(ctop-1,Double); ctop--; SETLEFT(ctop,reg); \
    if(2==prev_prev_len && First==GETOP(ctop-2)) \
          {SETOP(ctop-2,Triple);} \
    break; \
  }

#define PCOMPRESS(Simple,Double,First,Triple,F2,T2,F3,T3) \
if(1==prev_len && Simple==GETOP(ctop-1)) \
  { SETOP(ctop-1,Double); ctop--; SETLEFT(ctop,reg); \
    if(2==prev_prev_len && First==GETOP(ctop-2)) \
          {SETOP(ctop-2,Triple);} \
    else if(2==prev_prev_len && F2==GETOP(ctop-2)) \
          {SETOP(ctop-2,T2);} \
    else if(2==prev_prev_len && F3==GETOP(ctop-2)) {SETOP(ctop-2,T3);}  \
    break; \
  }
#endif

no insert_op(register no opcode, register no reg, register string name, register no arity, register stack wam)
{ g.linking=INPUT_INT(1);
  SETOP(ctop,opcode);
  switch(opcode)
  {
    case CLAUSE:
      {   cell pred=input_fun(name,arity);
        no ok=DEFPRED(pred,(no)ctop);
#if (TRACE>2)
	fprintf(STD_err,"inserting clause for %s/%ld\n",name,arity);
#endif
        if(currpred!=g.true) SETLABEL(last,(instr)ctop);
        reg++;
        if(ok)
          { badcode=FALSE;
            if(currpred!=g.true && GETOP(ctop-2)!=END)
              {
                switch(GETOP(last))
                  {
                    case TRY_ME_ELSE: /* begin of single cls */
                      { instr p;
                          for(p=last-2; p<=(instr)ctop-4; p++)
                          *p = *(p+4);
                      }
                      SETOP(ctop-4,END);
                      SETOP(ctop-2,END);
                    break;

                    case RETRY_ME_ELSE:
                      SETOP(last,TRUST_ME_ELSE);
                    break;

                    default: badcode=TRUE;
                      ERREXIT("bad code in backpatching")
                  }
              }
            SKIPBAD();
            currpred=pred;
            SETOP(ctop,SWITCH);
            SETREG(ctop,0);
            SETFUN(ctop,pred);
            ctop++;

            SETOP(ctop,TRY_ME_ELSE); /* reg = how far is fun=nextcls */
            SETREG(ctop,arity);   /* arity is here, not on the stack */
          }
        else if(currpred==pred)
          { SKIPBAD();
            SETOP(ctop,RETRY_ME_ELSE);
            SETREG(ctop,arity);
          }
        else
          { badcode=TRUE;
            fprintf(STD_err,
    "[%lu] %s/%lu <=predicate leads other group of clauses: IGNORED\n",
            ctop-cbase,name,arity);
            ERREXIT(
            "declare it multifile or discontiguous if that's what you want"
            )
          }
        last=(instr)(ctop++);
      }
    break;

    case FIRSTARG: /* MaxReg-FunFirstarg/Arity */
      SKIPBAD();
      if(reg>=MAXREG) ERREXIT("not enough registers")
        { no funval=(no)input_fun(name,arity);
          if('_'==*name || !hdef(currpred,funval,(no)ctop,g.timestamp))
            { instr label=GETPRED(currpred);
              SETOP(label,NONDET);
            }
        }
      ctop--; /* null effect, as we do ctop++ later */
    break;

    case EXECUTE: SKIPBAD();
      SETFUN(ctop,input_fun(name,arity));
    break;

    case LOAD_VARIABLE:
    case PUT_VARIABLE:
    case GET_VALUE: SKIPBAD();
  	SETREG(ctop,reg);
  	SETLEFT(ctop,arity);
    break;

    case GET_STRUCTURE:
    case PUT_STRUCTURE:
    case PUSH_STRUCTURE:
      SKIPBAD();
      SETREG(ctop,reg);
      SETFUN(ctop,input_fun(name,arity));
    break;

    case UNIFY_VARIABLE: SKIPBAD();
#if STRUCT_COMPRESS
      OCOMPRESS(UNIFY_VALUE,UNIFY_VAL_VAR,GET_STRUCTURE,
        GET_UNIFY_VAL_VAR)
      OCOMPRESS(UNIFY_VARIABLE,UNIFY_VAR_VAR,GET_STRUCTURE,
        GET_UNIFY_VAR_VAR)
#endif
      SETREG(ctop,reg);
    break;

    case UNIFY_VALUE: SKIPBAD();
#if STRUCT_COMPRESS
      OCOMPRESS(UNIFY_VALUE,UNIFY_VAL_VAL,GET_STRUCTURE,
        GET_UNIFY_VAL_VAL)
      OCOMPRESS(UNIFY_VARIABLE,UNIFY_VAR_VAL,GET_STRUCTURE,
        GET_UNIFY_VAR_VAL)
#endif
      SETREG(ctop,reg);
    break;

    case PUSH_VARIABLE:
         SKIPBAD();
         opcode=WRITE_VARIABLE;
         SETOP(ctop,opcode);
    case WRITE_VARIABLE: SKIPBAD();
#if STRUCT_COMPRESS
      PCOMPRESS(WRITE_VALUE,WRITE_VAL_VAR,
         PUT_STRUCTURE,PUT_WRITE_VAL_VAR,
         PUSH_STRUCTURE,PUSH_VAL_VAR,
         WRITE_CONSTANT,CONSTANT_VAL_VAR)

      PCOMPRESS(WRITE_VARIABLE,WRITE_VAR_VAR,
         PUT_STRUCTURE,PUT_WRITE_VAR_VAR,
         PUSH_STRUCTURE,PUSH_VAR_VAR,
         WRITE_CONSTANT,CONSTANT_VAR_VAR)
#endif
      SETREG(ctop,reg);
    break;

   case WRITE_VALUE: SKIPBAD();
#if STRUCT_COMPRESS
      PCOMPRESS(WRITE_VALUE,WRITE_VAL_VAL,
        PUT_STRUCTURE,PUT_WRITE_VAL_VAL,
        PUSH_STRUCTURE,PUSH_VAL_VAL,
        WRITE_CONSTANT,CONSTANT_VAL_VAL)

      PCOMPRESS(WRITE_VARIABLE,WRITE_VAR_VAL,
        PUT_STRUCTURE,PUT_WRITE_VAR_VAL,
        PUSH_STRUCTURE,PUSH_VAR_VAL,
        WRITE_CONSTANT,CONSTANT_VAR_VAL)
#endif
      SETREG(ctop,reg);
    break;

    case MOVE_REG: SKIPBAD();
#if STRUCT_COMPRESS
      if(1==prev_len && MOVE_REG==GETOP(ctop-1) &&
         !(1==prev_prev_len && MOVE_REGx2==GETOP(ctop-2) )
        )
        SETOP(ctop-1,MOVE_REGx2); /* instr compression */
#endif
      SETREG(ctop,reg);
      SETLEFT(ctop,arity);
    break;

    case LOAD_VALUE: SKIPBAD();
#if STRUCT_COMPRESS
      if(1==prev_len && LOAD_VALUE==GETOP(ctop-1)
         && 1==GETREG(ctop-1) && 2==reg)
        {
          SETOP(ctop-1,LOAD_VALUEx2); SETREG(ctop-1,arity); ctop--; break;
          /* An = REG = second,  Ai = LEFT = first */
        } /* instr  compression */
#endif
      SETREG(ctop,reg);
      SETLEFT(ctop,arity);
    break;

    case LOAD_CONSTANT:
#if STRUCT_COMPRESS
      { cell small;
        if( 1==prev_len && LOAD_VALUE==GETOP(ctop-1)
            && 1==GETREG(ctop-1) && 2==reg
            && ((small=input_fun(name,arity)), INTEGER(small))
            && (no)OUTPUT_INT(small) <
                 (no)(1<<(2-LSCALE+ARITYBITS-TAGBITS)))
          {
            SETOP(ctop-1, opcode=LOAD_VAL_SHORT);
            SETREG(ctop-1,small); ctop--;
            break;
          }
      } /* instr compression */
#endif
      SETREG(ctop,reg);
      SETFUN(ctop,input_fun(name,arity));
    break;

    case GET_CONSTANT:
    case PUT_CONSTANT:
    case UNIFY_CONSTANT:
    case WRITE_CONSTANT: SKIPBAD();
      SETREG(ctop,reg);
      SETFUN(ctop,input_fun(name,arity));
    break;

    case BEGIN_C_CHUNK:
#if defined(BEGIN_END)
    if(TRUE)
#else
    if(g.inC)
#endif
    {
      SKIPBAD();
      SETREG(ctop,reg);
      SETFUN(ctop,(cell)name);
    }
    else
       fatal_error("unexpected BEGIN_C_CHUNK");
    break;

    case END_C_CHUNK:
#if defined(BEGIN_END)
    if(TRUE)
#else
    if(g.inC)
#endif
      SETREG(ctop,reg);
    else
      fatal_error("unexpected END_C_CHUNK");
    break;

    case END: SKIPBAD();
      SETREG(ctop,reg); ctop++;
      badcode=TRUE;
      if(!link_code((instr)bak_ctop,wam)) return FALSE;
      g.linking=INPUT_INT(0);
    break;

    case ARITH:
      SKIPBAD();
      SETOP(ctop,(opcode+arity));
      if(reg)
        {
          SETREG(ctop,reg);
#if 0
          if(SELF_INFO_1==opcode+arity)
            {cell n=GETARITY(currpred)-1;
             SETLEFT(ctop,n);
            }
          else
#endif
            SETLEFT(ctop,((cell)(*name-'0')));
        }
    break;

    case INLINE:
    case BUILTIN:
      SKIPBAD();
      SETREG(ctop,reg);
      SETOP(ctop,opcode+arity);
    break;

    default: SKIPBAD();
      SETREG(ctop,reg);
  }
  IF_OVERSTACK("insert_op",(term *)(ctop),InstrStk,g.shared,NO());
  prev_prev_len=prev_len;
  prev_len=instr_len[opcode];
  ctop++;
  return TRUE;
}

no init_code(register stack wam)
{
  no ok; opcount=ZERO;
  SETOP(ctop,END); /* therfore the code array starts with an END */
  ctop+=instr_len[END]; /* this creates the action if the WAM fails */
  currpred=g.true;
  badcode=TRUE;
  prev_prev_len=prev_len=2;
  SETOP(ctop,PROCEED);    /* this creates true/0 => if the WAM succeeds */
  ok=DEFPRED(g.true,(no)ctop);
  SETFUN(ctop,g.true);
  ctop++;
  return ok;
}

#define GET_BYTE(a_byte) if(EOF==(a_byte=(bp_long)getc(f))) break

static void skip_header(FILE *f)
{
  no b,c=getc(f);
  if('#'!=c)
    { ungetc(c,f);
      return;
    }
  else
    b=c;
  while(TRUE)
  {
    b=c;
    GET_BYTE(c);
    if('$'==b && '0'==c) {GET_BYTE(c); break;}
  }
}

#define MAXNAME 256
static bp_long loadoperator(string buf)
{
  char assoc[MAXNAME], name[MAXNAME];
  no pri,ok=TRUE; bp_long self=g.self; /* backup the value of g.self */

  if(g.inC)
  {
     /* we do this to force internalizing operator names */
     if(LOADTIME==g.timestamp) g.self=0; /* overrides g.inC */
   }

  sscanf(buf, "%ld%s%s", &pri, assoc, name);

  if (!make_op(input_fun(name,0),
               input_fun(assoc,0),INPUT_INT(pri),g.timestamp))
    {
      fprintf(STD_err, "opcount=%ld g.timestamp=%d op: %ld,%s,%s",
           opcount,g.timestamp, pri, assoc, name);
      warnmes("Invalid operator declaration");
      ok=FALSE;
    }

  if(g.inC)
     {
       g.self=self; /* restores g.self */
     }
  return ok;
}


static no ii(register no opcode, register no reg, register no arity, register string name, register stack wam)
{
  return (opcode == OPERATOR)?
         loadoperator(name):
         insert_op(opcode,reg,name,arity,wam);
}

static no load_file(string fname, register stack wam)
{
  register no opcode,reg,arity;
  no ok; FILE *f=fopen(fname,"rb");
  char sbuf[MAXNBUF];
  if(!f) {quietmes("%s",fname);
          quietmes("%s","??? ");
          ERREXIT("file not found")
  }

  /* quietmes("begin loading %s...",fname); */

  skip_header(f); ok=TRUE;
  while(ok)
    {
      register bp_long i;
      GET_BYTE(opcode); GET_BYTE(reg); GET_BYTE(arity);
      for(i=0; i<(bp_long)MAXNBUF; i++)
        { register int c;
          GET_BYTE(c);
          if(!c) break;
          sbuf[i]=(char)c;
        }
      sbuf[i]='\0';
      ok=ii(opcode,reg,arity,sbuf,wam);
    }
  fclose(f);

  if(!ok)
    quietmes("%s\n","unable to load kernel file");
  return ok;
}

no load_1(register term regs, register stack wam)
{ register term xref; register cell xval;
  FDEREF(regs[1]);
  if(!SYMCONST(xval)) return (no)LOCAL_ERR(xval,"load/1: bad argument");
  return load_file(NAME(xval),wam);
}


#if 1

/*SELF*/

extern struct bp_instr wam_bp[],user_bp[];

static no load_instr_array(string from_file, register stack wam, struct bp_instr *p, byte end)
{  bp_long i,j; no ok; no op;
   g.self=1;

   for(i=0,j=1; end!=(op=p[i].op) ;i += j)
   {
      ok=ii(p[i].op,
            p[i].reg,
            p[i].arity,
            p[i].name,
            wam);

      if(!ok)
         return FALSE;

      j=1;
      if(BEGIN_C_CHUNK==op) {
        for(; END_C_CHUNK!=p[i+j].op; j++)
         {*ctop++=(instr)input_fun(p[i+j].name,p[i+j].arity);
         }
        if(p[i+j].arity!=j) {
          fprintf(STD_err,
            "instruction=[%ld] size(found=%d, computed=%ld)\n",
             i, p[i+j].arity, j);
          {  bp_long k;
             for(k=0;k<=j;k++)
              fprintf(STD_err,"{%d,%d,%d,%s}\n",
                 p[i+k].op, p[i+k].reg, p[i+k].arity, p[i+k].name
              );
          }
          fatal_error("invalid c_chunk: bad size");
        }
      }
    }
  sprintf(g.sbuf,"Finished loading %s C-code (%ld instructions).\n",
     from_file,i);
  /*quietmes("%s",g.sbuf);*/

  return TRUE;
}

static no load_self(register stack wam)
{
  return ( load_instr_array("system",wam,wam_bp,0) &&
           load_instr_array("user",wam,user_bp,255)
  );
}
#endif

static no is_bp_file(string fname)
{  bp_long l=strlen(fname);
   return (l>=3 && 0==strcmp(fname+(l-3),".bp"));
}

/*
  if fname is *.bp, loads it at this point
  otherwise fname is a goal to be executed later (see top.pl, lib.pl)
*/

no load_kernel(string fname, register stack wam)
{
  if(!init_code(wam)) return 0;

  if(!fname || is_bp_file(fname)) g.startup_file_name=g.empty;
  else
    { g.startup_file_name=INPUT_STRING(fname);
      fname=NULL;
    }

  if(!fname)
    {
      if(g.inC)
        return load_self(wam);
     else
        fname="wam.bp";
    }
  return load_file(fname,wam);
}
