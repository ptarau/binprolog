#ifndef _GLOBAL_H
#define _GLOBAL_H

#include <stdlib.h>
#include <stdio.h>

#ifndef VCC
#define VCC 0
#endif

#if VCC==0
/* HAD PROBLEMS WITH VCC which does not have unistd.h */
#include <unistd.h>
#endif

#if VCC>0 && (defined _M_AMD64)
#define WSIZE 64
#elif VCC==0 && (defined W64)
#define WSIZE 64
#else
#define WSIZE 32
#endif

#define TSTORE

#ifndef VERSION
#define VERSION 1200
#endif

/** CHANGES
11.42 - bus error caused by limit of stac allocated objects MAXNBUF DEFAULT NOW REPLACES MAXSBUF in mame2list
11.41 - fixed bus error bug un mac os x in name2list
11.19 - ported TwinProlog to jdk 1.6.0
11.02 - released as BinProlog 2006
10.93 - added termStore API supporting an assert/retract-style external Term storage
        intended to be mapped to fast external term storage code like STL multimaps
        as well as external tuple spaces or persistent object stores

10.75 - reduced operators like dynamic,mode etc. to 850 precedence to
        avoid recurrent problems when parsing lists that contain them
        as nat. lang. words
10.74 - added trim_float - fixed float read back bug with qprint
10.73 - added global_set/global_get/global_rm
10.70 - added deep_hash - with recursion depth control as second param, defined hkey, term_hash with it
10.69 - fixed x_trace
10.55 - moved term_hash to C, changed hkey to do full hashing on ground terms
10.54 - added db_hook allowing redefinition of db predicates
10.41 - added ability to rebrand in ru.c as a OEM product
10.35 - made BP_DLL user code more flexible, added to_compilable to convert files to a canonical form with all preds contiguous
10.33 - ensured exec_run_time_commands never runs more than once
10.29 fixed bug/feature stopping scompile make facility to work when BinProlog was called as bp rather than bp.exe
10.26 released as BinProlog 2005
10.24 file names are now used instead of modules in generating synthetic predicate names with new_name in co.pl
10.23 noticed that \(X,Y,D) is only a means to define bitwise complement - first arg should be always 0
10.22 bug introduced in \(X) fixed in 10.23
10.17 changed float.c with union for double 2 ints conversion
10.16 fixed bb_gc1 estimates by adding htable size
10.15 added hash_save and fixed hkey to be always defined for nonvars and > 0
10.13 made term_hash support multiple val additions to a key
10.12 extended term_hash to cover lists and hash_get to list all key/vals when given var key arg
10.07 added return/1 - same as in Jinni
10.05 standardized new_engine/3, get/2, stop/1, element_of/2 as in Jinni return/1 to be implmented
10.02 added callj for interfacing with Jinni in Twin Prolog
9.97 improvements in the C-interface for fast exchange of
     lists of ints, strings, floats
9.96 released demo on Nov 13 60 days
9.96 for_all added with negation based def
     made public variants of run_server
9.95 released on Sept 17, 2003
9.93 compiles under .NET 2003 cl
9.91 released 60 days
9.89 redisigned hash functions - OS bug - kills XP HE SP1 on huge FrameNet process
9.88 made token_of/2 more space efficient based on line_of/2 derived from sentence_of/3
9.86 fixed bug in destroy_engine inducing http_server crash
9.84 fixed bug in timed_call related to destroy_engine()
9.77 added dir2list directory lister using pcollect
9.75: fixed bug in random: now init_random(0) in each new thread
9.61: fixed vars and floating point bug in new reader codes2terms
8.87 - made topcall=metacall - no ref to lval and setarg from main
       system
       mobile_call/1 has to be used for more general calls
*/

#define BUG(mes)
/* #define BUG(mes) {warnmes(mes);} */

/* editables (here in makefile) */

#define STD_in g.user_input
#define STD_out g.user_output
#define STD_err stderr

#if VCC>0

#define NOGETOPT
#define FORCE_CDECL __cdecl
#define FORCE_STDCALL __stdcall
#else
#define FORCE_CDECL
#define FORCE_STDCALL
#endif

#ifndef THREADS
#define THREADS 0
#endif

#if THREADS>0
#define _REENTRANT
#endif

#ifndef ERROR_HANDLER
#define ERROR_HANDLER 1
#endif

#ifndef GC
#define GC 1
#endif

#ifndef TRACE
#define TRACE 0
#endif

#ifndef PROF
#define PROF 0
#endif

#define PEDANTIC (TRACE+0)

#ifndef EAGER_DEREF
#define EAGER_DEREF 1
#endif


/* editables here only - seldom changed */

#define STRUCT_COMPRESS 1

#define JUMP_COMPRESS 1

#define HAS_OVERRIDE 1
/* was: (!JUMP_COMPRESS) */

#define BUILTIN_MEMBER 1

/* end of configurable paprameters */

#if TRACE < 1 && !defined(NDEBUG)
/* #define NDEBUG 1 */
#endif

/* multiple-engine operations (used also for the Prolog-C interface) */

#define MAXENGINES 1024
#define MAXTSYNC MAXENGINES

#define ROOT_ENGINE 0
#define EMPTY_ENGINE 1
#define LOADED_ENGINE 2
#define HALTED_ENGINE 3
#define DEAD_ENGINE 4
#define RUNNING_ENGINE 5
#define SUSPENDED_ENGINE 6

#define ENGINE_TYPE 0
#define ENGINE_PARENT 1
#define ENGINE_P 2
#define ENGINE_GOAL 3
#define ENGINE_ANSWER 4
#define ENGINE_THREAD 5
#define ENGINE_ERROR 6
#define ENGINE_ID 7
#define ENGINE_RETURN 8
/* this should be 1+ the last prop */
#define MAX_ENGINE_PROP 9

#define SET_ENGINE(Attr,Mes) (wam[MesStk].base[Attr]=(term)(Mes))
#define GET_ENGINE(Attr) (wam[MesStk].base[Attr])
#define SET_ENGINE_TYPE(Mes) SET_ENGINE(ENGINE_TYPE,INPUT_INT(Mes))
#define GET_ENGINE_TYPE() OUTPUT_INT((cell)GET_ENGINE(ENGINE_TYPE))
#define SET_ENGINE_PARENT(Mes) SET_ENGINE(ENGINE_PARENT,PTR2INT(Mes))
#define GET_ENGINE_PARENT() INT2PTR((cell)GET_ENGINE(ENGINE_PARENT))

#define ENGINE_OK 0
#define ENGINE_ABORT -1
#define ENGINE_FORCE_HALT -2
/*
#define ENGINE_SUSPEND -3
#define ENGINE_OVERFLOW -4
#define ENGINE_RESTART -5
#define ENGINE_BREAK -6
*/

#define SET_ENGINE_ERROR(No) SET_ENGINE(ENGINE_ERROR,INPUT_INT(No))
#define GET_ENGINE_ERROR() OUTPUT_INT((cell)GET_ENGINE(ENGINE_ERROR))

#if (TRACE>0)
#define TRACE_ENGINE(Mes) \
   fprintf(STD_err,"!!! %s: (engines=%ld) %ld, type %ld\n", \
          (Mes),(no)(g.lastengine-g.engines),\
          ((cell)wam)>>TAGBITS,GET_ENGINE_TYPE());
#else
#define TRACE_ENGINE(Mes)
#endif


#if (TRACE>0)
#define DCG_TRACE() {fprintf(STD_err,"unexpected dcg_..."); ASSERT1(0);}
#define VTRAIL_TRACE() {fprintf(STD_err,"unexpected value trail..."); ASSERT1(0);}
#else
#define DCG_TRACE()
#define VTRAIL_TRACE()
#endif

#define HASH_SEED 5381L

#define STRING_HASH(String,Key,Init) \
{ register string p=(String); \
  (Key)=(HASH_SEED); while(*p) (Key)+=((Key)<<5)+*p++; (Key)+=(Init)<<7;\
}

/* SIZEs of various DATA AREAS: can be replaced with
variables as they will be dynamically allocated */

#define MAXDCG (MAXARITY-1)

#ifdef STATIC_TABLES
#define HBITS 13
#define ABITS 12
#define HMAX ((no)(1<<HBITS))
#define MAXATOM (1<<ABITS)
#else
#define HMAX (max.DICT)
#define MAXATOM (max.ATOMS)
#endif

#define MAXLEX ((MAXATOM)<<4)

/*
Tracing (debugging) and profiling information:

-----TRACE------
0==>default: debug.c, prof.h are possibly not compiled, ASSERT2 is off
1==>ASSERT2 is on
2==>EXECS AND JUMPS are traced
>=3==>ALL INSTRUCTIONS AND DATA AREAS ARE SHOWN

----PROF-------
0==>default: no profiler, debug.c, prof.h possibly ignored
1==>instructions are counted but not timed
2==>instructions are probabilistically timed (chance 1/13...)
3==>all instructions are timed
*/

#if TRACE > 0
#include <assert.h>
#define ASSERT2(Cond,Term) \
  if(!(Cond)) \
    {(void)local_error((cell)(Term),"ASSERT2 failed",wam); \
    assert(0);}
#define ASSERT1(Cond) assert(Cond);
#else
#define ASSERT2(Cond,Term)
#define ASSERT1(Cond);
#endif

typedef char *string;
typedef unsigned char byte;

#if 0 && (WSIZE == 64)
typedef long long bp_long;
typedef unsigned long long cell;
#define LFORMAT "%lld"
#define UFORMAT "%llu"
#else
typedef long bp_long;
typedef unsigned long cell;
#define LFORMAT "%ld"
#define UFORMAT "%lu"
#endif

#define BP_LONG2STR(BUFFER,LLONG) sprintf(BUFFER,LFORMAT,(LLONG));

typedef unsigned short bp_short;

typedef cell no;

typedef cell *term;
typedef cell *instr; /* to circumvent bug in insight ! KDB */

extern string *atomtable;

#define EMPTY

/*
this seems to remove some gc bugs !!!
TR_VTAG==1!!!
*/

#define VARTAG PUTTAG(ZERO,ZERO)
#define FUNTAG PUTTAG(ZERO,((no)3))
#define INTTAG PUTTAG(ZERO,ONE)
#define BADTAG PUTTAG(ZERO,((no)2))


#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

typedef struct limit
{
  no HEAP,CHOICE,TRAIL,CODE,BOARD, DICT, ATOMS,SBUF,MAX_CORE;
  cell QUIET,LMETH,DB_RATIO;
  no ENGINE_ATTR,PORT;
}
*limit;

#define MAXSBUF (1<<18L)

/* 1<<18 seems to be the max for stack allocated local arrays see bus error on os X */
#define MAXNBUF ((MAXSBUF)>>2)

#define QLEVEL() OUTPUT_INT(max.QUIET)

/* RESTRICTED SET OF WAM INSTRUCTIONS
an easy optimisation is to specialize them for lists*/

#define MAXOP 256

#define END 0

#define UNIFY_VARIABLE 1
#define WRITE_VARIABLE 2

#define UNIFY_VALUE 3
#define WRITE_VALUE 4

#define UNIFY_CONSTANT 5
#define WRITE_CONSTANT 6

#define GET_CONSTANT 7
#define GET_STRUCTURE 8

#define PUT_CONSTANT 9
#define PUT_STRUCTURE 10

#define MOVE_REG 11

#define PUT_VARIABLE 12
#define GET_VALUE 13

#define PUSH_CUT 14
#define PUT_CUT 15
#define GET_CUT 16

#define EXECUTE 17
#define PROCEED 18

#define TRY_ME_ELSE 19
#define RETRY_ME_ELSE 20
#define TRUST_ME_ELSE 21

#define TRY_ME_ONLY 22
#define NONDET 23

#if JUMP_COMPRESS
#define EXEC_SWITCH 24



#endif

#define SWITCH 25

#if JUMP_COMPRESS
#define EXEC_JUMP_IF 26
#endif

#define JUMP_IF 27

#define LOAD_CONSTANT 28
#define LOAD_VALUE 29

#if STRUCT_COMPRESS
#define GET_UNIFY_VAR_VAR 30
#define UNIFY_VAR_VAR 31
#define PUT_WRITE_VAR_VAR 32
#define WRITE_VAR_VAR 33

#define GET_UNIFY_VAL_VAL 34
#define UNIFY_VAL_VAL 35
#define PUT_WRITE_VAL_VAL 36
#define WRITE_VAL_VAL 37

#define GET_UNIFY_VAR_VAL 38
#define UNIFY_VAR_VAL 39
#define PUT_WRITE_VAR_VAL 40
#define WRITE_VAR_VAL 41

#define GET_UNIFY_VAL_VAR 42
#define UNIFY_VAL_VAR 43
#define PUT_WRITE_VAL_VAR 44
#define WRITE_VAL_VAR 45

#define MOVE_REGx2 46
#define LOAD_VALUEx2 47
#define LOAD_VAL_SHORT 48
#endif

#if JUMP_COMPRESS
#define EXEC_TRY 49
#endif

#define LOAD_VARIABLE 50

#define PUSH_STRUCTURE 51
#define PUSH_VARIABLE 52

#if STRUCT_COMPRESS
#define PUSH_VAR_VAR 53
#define CONSTANT_VAR_VAR 54
#define PUSH_VAL_VAR 55
#define CONSTANT_VAL_VAR 56
#define PUSH_VAL_VAL 57
#define CONSTANT_VAL_VAL 58
#define PUSH_VAR_VAL 59
#define CONSTANT_VAR_VAL 60
#endif

#define UNIFY_VOID 61
#define WRITE_VOID 62

#define BEGIN_C_CHUNK 63
#define END_C_CHUNK 64
/* normally the last hardwired instruction: generated by load.c */
#define APPLY 65

/* non-executable pseudo-operations */
#define NOP LAST_BUILTIN+1
#define CLAUSE NOP+1
#define FIRSTARG NOP+2
#define OPERATOR NOP+3
/* currently unused: NEWPRED */
#define NEWPRED NOP+4

#define HeapStk 0
#define TrailStk 1
#define ChoiceStk 2
#define MesStk 3
#define MarkStk 4

#define MaxStk 5

/* shared stacks */

#define InstrStk 0
#define BBoardStk 1

typedef struct stack
  {
    bp_long size;
    bp_long over;
    term *top;
    term *oldtop;
    term *base;
    term *margin;
    term *end;
    term *maxused;
    string name;
  }
  *stack;

#define cbase (g.shared[InstrStk].base)
#define ctop (g.shared[InstrStk].top)
#define bak_ctop (g.shared[InstrStk].oldtop)

/* WAM registers: placed at the beginning of the heap of an engine */

#define MAKEREGS(Base) (regs=(term)(Base)+TEMPARGS,regs+MAXREG)
#define LOCATEREGS() ((term)(wam[HeapStk].base+TEMPARGS))
#define HEAPSTART() (LOCATEREGS()+MAXREG)
#define DCGSTART() (HEAPSTART()-MAXDCG)

/* base->regs[0]..regs[MAXREG-MAXDCG]->DCGSTART()->HEAPSTART()->margin->end */

/* GLOBAL REGISTERS */

#define SAVED_An(Arity,No) (*(A-4-((Arity)-(No))))
#define SAVED_Cont (*(A-3))
#define SAVED_H (*(A-2))
#define SAVED_TR (*(A-1))
#define SAVED_P (*A)
#define SAVED_lastP(Arity) (A-4-Arity)

#define TR_TOP wam[TrailStk].top

/* HASING TABLE ENTRY */

typedef struct hentry
{
  no pred;
  no fun;
  no val;
}
*hentry;

/* DATA AREA FORMATS & INSTRUCTION FORMATS */

#define ONE ((no)1)
#define ZERO ((no)0)

#define LSCALE ((int)(sizeof(no)/sizeof(int)))
#define WBITS (sizeof(no)*8)

#define TAGBITS (1+LSCALE)
/* bug/unfelicity: ARITYBITS larger than 8 break, 7 seems ok */
#define ARITYBITS 8 /* do not change this */

#define REGBITS 11 /* do not change this */
#define ARGBITS REGBITS
#define OPBITS (WBITS-REGBITS-ARGBITS)

/* if OPBITS > 10 it will be slow on SPARCs */

#define MAXARITY (ONE<<ARITYBITS)
#define MAXREG ((ONE<<ARGBITS)-ONE)

/**********************LOW-LEVEL TERM OPERATIONS*****************/

#define LBITS ARITYBITS
#define RBITS TAGBITS
#define MBITS (WBITS-LBITS-RBITS)

#define RMASK ((ONE<<RBITS)-ONE)
#define LMASK ((no)~(ZERO)<<(MBITS+RBITS))
#define MMASK (~LMASK&~RMASK)

#define LGET(W) ((no)(W)>>(MBITS+RBITS))
#define LPUT(W,Val) (((no)(W)&~LMASK) | (no)(Val)<<(MBITS+RBITS))
#define LSET(W,Val)     ((no)(W)= LPUT(W,Val))

#define MGET(W) ((no)(W)<<LBITS>>(LBITS+RBITS))
#define MPUT(W,Val) (((no)(W)&~MMASK) |((no)(Val)<<(LBITS+RBITS)>>LBITS))
#define MSET(W,Val) ((no)(W)=MPUT(W,Val))

#define RGET(W) ((no)(W)&RMASK)
#define RPUT(W,Val) (((no)(W) & ~RMASK) | (no)(Val))
#define RSET(W,Val) ((no)(W)=RPUT(W,Val))


/*******************INTERFACE TERM OPERATIONS ******************/

#define SETTAG(W,Val) RSET(W,Val)
#define PUTTAG(W,Val) RPUT(W,Val)
#define GETTAG(W) RGET(W)

#define SETSYMNO(W,Val) MSET(W,Val)
#define PUTSYMNO(W,Val) MPUT(W,Val)
#define GETSYMNO(W) MGET(W)

#define SYMBITS MBITS

/* #define SETARITY(W,Val) LSET(W,Val) */
/* NOTE that PUTARITY returns a COPY of a cell with the new arity */
#define PUTARITY(W,Val) LPUT(W,Val)

/*************** HIGH-LEVEL TERM OPERATIONS************************/

#define SAFETY -1

#if SAFETY > 1
#define Comment(C) fprintf(STD_err,"%s:%d %s\n",__FILE__,__LINE__,C)
#else
#define Comment(C)
#endif

#define T2T(t) C2T(T2C(t))
#define T2C(t) ((cell)(t))
#define C2C(c) (c)
#define C2T(c) ((term)(c))

#define GETREF(t) (C2C(*(T2T(t))))
#define GETCELL(t) (C2C(*(C2T(t))))
#define GETARITY(W) LGET(C2C(W))

#define SETREF(t,Ref) (*(T2T(t)) = T2C(Ref))
#define SETCELL(t,Cell) (*(T2T(t)) = C2C(Cell))

#define INITREF(t,Ref) SETREF(t,Ref)
#define INITCELL(t,Ref) SETCELL(t,Ref)

#define NONVARREF(t) (NONVAR(GETREF(t)))
#define NAME(s) (atomtable[GETSYMNO(C2C(s))])

#define INPUT_INT(Extval) ((no)((bp_long)(Extval)<<TAGBITS) | INTTAG)
#define OUTPUT_INT(Intval) ((bp_long)(C2C(Intval))>>TAGBITS)

#define NONVAR(ACell) GETTAG(C2C(ACell))
#define VAR(ACell) (!GETTAG(C2C(ACell)))
#define INTEGER(ACell) (GETTAG(C2C(ACell)) == INTTAG)
#define IDENTIFIER(ACell) (GETTAG(C2C(ACell)) == FUNTAG)
#define SYMCONST(ACell) (IDENTIFIER(ACell) && !GETARITY(ACell))
#define COMPOUND(ACell) (IDENTIFIER(ACell) && GETARITY(ACell))
#define SIMPLE(ACell) (!COMPOUND(ACell))
#define BP_FLOAT(Cell) (g.bp_float==(C2C(Cell)))
#define NUMERIC(Cell) (INTEGER(Cell) || BP_FLOAT(Cell))

#define ATOMIC(ACell) \
(NONVAR(ACell) && (INTEGER(ACell) || !GETARITY(ACell)))

#define FREEVAR(ACell) (VAR(ACell) && (ACell)==GETCELL(ACell))


/*************** LOW-LEVEL INSTRUCTION OPERATIONS *****************/

#define LCBITS REGBITS
#define MCBITS ARGBITS
#define RCBITS OPBITS

#define RCMASK (( ONE <<RCBITS)-ONE)
#define LCMASK ( ((no)~(ZERO)) <<(MCBITS+RCBITS))
#define MCMASK (~LCMASK&~RCMASK)

/* Scale must be 0 or TAGBITS */

#define LCGET(W,Scale) ((no)(W)>>(MCBITS+RCBITS+Scale))
#define LCPUT(W,Val) ((no)(W)&~LCMASK) | (no)(Val)<<(MCBITS+RCBITS+TAGBITS)
#define LCSET(W,Val)    ((W)= LCPUT(W,Val))

#define MCGET(W,Scale) ((no)(W)<<(LCBITS)>>(LCBITS+RCBITS+Scale))
#define MCPUT(W,Val) \
  (((no)(W)&~MCMASK)|((no)(Val)<<(LCBITS+RCBITS+TAGBITS)>>LCBITS))
#define MCSET(W,Val) ((W)=MCPUT(W,Val))

#define RCGET(W) ((no)(W)&RCMASK)
#define RCPUT(W,Val) (((no)(W) & ~RCMASK) | (no)(Val))
#define RCSET(W,Val) ((W)=RCPUT(W,Val))

/*******INTERFACE INSTRUCTION OPERATIONS ********************/

#define SETOPCELL(W,Val) RCSET(W,Val)
#define PUTOPCELL(W,Val) RCPUT(W,Val)
#define GETOPCELL(W) RCGET(W)

#define SETLEFTCELL(W,Val) MCSET(W,Val)
#define PUTLEFTCELL(W,Val) MCPUT(W,Val)
#define GETLEFTCELL(W) MCGET(W,TAGBITS)

#define SETREGCELL(W,Val) LCSET(W,Val)
#define PUTREGCELL(W,Val) LCPUT(W,Val)
#define GETREGCELL(W) LCGET(W,TAGBITS)

/********* HIGH LEVEL INSTRUCTION OPERATIONS*****************/

#define REGFIELD GETREGCELL(fields)
#define LEFTFIELD GETLEFTCELL(fields)
#define OPFIELD GETOPCELL(fields)

#define On REGFIELD
#define Oi LEFTFIELD

#define An (*(term)((no)regs+ LCGET(fields,0) )) /* cell */
#define Ai (*(term)((no)regs+ MCGET(fields,0) )) /* cell */

#define GETFIELDS(Step) fields=((term)P)[Step]

/* cell instr; An, Ai, Op */

#define GETOP(Ip) GETOPCELL(*(instr)(Ip))
#define SETOP(Ip,Val) SETOPCELL(*(term)(Ip),(term)(Val))

#define GETREG(Ip) GETREGCELL(*(instr)(Ip))
#define SETREG(Ip,Val) SETREGCELL(*(term)(Ip),(term)(Val))

#define GETLEFT(Ip) GETLEFTCELL(*(instr)(Ip))
#define SETLEFT(Ip,Val) SETLEFTCELL(*(term)(Ip),(term)(Val))

#define GETFUN(Ip) (*(term)((Ip)+1))
#define SETFUN(Ip,Val) (*(term)(++(Ip)))=(Val)

#define GETLABEL(Ip) (*(instr *)((Ip)+1))
#define SETLABEL(Ip,Val) (*(instr *)((Ip)+1))=(Val)


#if 1
#define COPY_CELLS(To,From,Arity) \
{ register no k; /* for k=1 to Arity */ \
  for(k=(Arity)<<(TAGBITS); k; k-= 1<<(TAGBITS) ) \
    { ASSERT2(!COMPOUND(GETREF((term)((no)(From)+k))),\
        GETREF((term)((no)(From)+k)));\
      SETCELL((term)((no)(To)+k),GETREF((term)((no)(From)+k)) ); \
    } \
}
#else
#define COPY_CELLS(To,From,Arity) \
memcpy((void *)(To+1),(void *)(From+1),((Arity)<<TAGBITS))
/* this will be MUCH slower, it here just for semantics... */
#endif

#define COPY_ARGS(To,From,Arity)  \
 { COPY_CELLS(To,From,(Arity-1)); \
   SETREF(To+Arity,From+Arity);   \
 }

#define COPY_REGS()\
if(fields)\
{ COPY_CELLS(regs,xref,fields-1);\
  xval=xref[fields];\
  if(COMPOUND(xval)) xval=T2C(xref+fields);\
  regs[fields]=xval;\
}

#define CUT2INT(CutPoint) (INTTAG+(cell)(CutPoint))
#define INT2CUT(IntVal) ((cell)(IntVal)-INTTAG)

#if 1
#define PTR2INT(PtrVal) CUT2INT(PtrVal)
#define INT2PTR(IntVal) ((void *)INT2CUT(IntVal))
#else
#define PTR2INT(PtrVal) INPUT_INT(PtrVal)
#define INT2PTR(IntVal) OUTPUT_INT(IntVal)
#endif

/* OTHER MACROS */
#define NEWLN() fprintf(g.tellfile,"\n"); fflush(g.tellfile)

#define INSTR_LEN(Instr) instr_len[GETOP(Instr)]
#define ON(stk,Ob) \
  (wam[stk].base<=(term*)(Ob) && (term*)(Ob)<wam[stk].base+wam[stk].size)

#define ONSTACK(Stack,Ob) \
  ((Stack).base<=(term*)(Ob) && (term*)(Ob)<(Stack).base+(Stack).size)

#define INSPACE(Term,Low,High) \
        ((Low) <= (Term) && (Term) < (High))

#define MOD(x,y) ((x) & ((y)-1))

#define SYSTIME 0
#define LOADTIME 1
#define RUNTIME 2
#define BBOARDTIME 3
#define VARTIME 4

typedef struct specsyms
{
  instr call,/*prolog_main,*/ prolog_load,prolog_init,prolog_run,
        metacall,metatrue,member_entry,for_entry;
  cell NIL,DOT,DIF,DIV,VAR,INT,empty,
       fx, fy, xf, yf, xfx, xfy, yfx,prefixop,infixop,postfixop,
       true,
       predmark,ucount,seemark,tellmark,end_of_file,opmark,
#if JUMP_COMPRESS
       jcompressmark,
#endif
       user,closed_file,seefunc,tellfunc,
       bp_float,c,bp,wam,h,pro,pl,mem,
       compile,reconsult,
       current_db,
       current_user_file,
       bp_state,source,target,implem,inC,peval_io,bp_virtual,
       startup_file_name,

	   query,

	   answer,

	   /* String to String C-interface */

	   callback;
  bp_long self;
  cell linking,gc,bbgc,bbhi,compare_vals[3];


  no bak_newatom,bak_hcount,bak_opcount;
  string bak_newlex;
  bp_long stime,rtime,total_gctime,gctime;
  FILE *seefile, *tellfile, *user_input, *user_output;
  bp_long lineno;
  bp_long argc;
  string *argv;
  string sbuf,stop; /* character buffer for IO */
  stack *engines;
  stack *lastengine;
  struct err_struct {
    no id;            /* errno */
    cell mes;         /* message */
    cell arg1,arg2;   /* bad value */
    stack wam;        /* wam */
  } err;

  byte timestamp;
  struct stack shared[2];
  bp_long stopper;
 } *specsyms;

#define INPUT_STRING(Name) new_func((Name),0)

#define CLEAR_BP_ERROR() \
{ g.err.id=INPUT_INT(0);\
  g.err.mes=g.empty;\
  g.err.arg1=g.empty;\
  g.err.arg2=g.empty;\
  g.err.wam=NULL;\
}

#define BP_ERROR(No,Mes,Arg1,Arg2,Ret) \
{ \
  if(INPUT_INT(0)!=g.err.id) \
    {warnmes("irrecoverable error in error handler"); bp_halt(No);} \
  g.err.id=INPUT_INT(No);\
  g.err.mes=INPUT_STRING(Mes);\
  g.err.arg1=(cell)(Arg1);\
  g.err.arg2=(cell)(Arg2);\
  g.err.wam=wam;\
  if(!Ret) return (Ret);\
}

#define BB_OVERFLOW 101
#define HEAP_OVERFLOW 102
#define BAD_QUEUE 103

#define NEWVAR(R) SETREF(H,H); (R)=T2C(H++)

#define NEWVAR2(R1,R2) SETREF(H,H); (R1)=(R2)=(cell)(H++)

#define HNEWVAR(N,Val) xref=H+(N); (Val)=T2C(xref); SETREF(xref,xref)
#define HNEWCELL(N,Val) SETCELL((H+N),(Val))
#define HNEWFUN(P) HNEWCELL(0,GETFUN(P))

#if EAGER_DEREF>0
#define HNEWVAL(N,Val) \
  FDEREF(Val); \
  if(COMPOUND(xval) ) {HNEWCELL(N,(cell)xref);} else {HNEWCELL(N,xval);}
#define PUSHDERVAL(Val) \
  FDEREF(Val); \
  if(COMPOUND(xval) ) {PUSHVAL((cell)xref);} else {PUSHVAL(xval);}
#else
#define HNEWVAL(N,Val) HNEWCELL(N,(Val))
#define PUSHDERVAL(Val) PUSHVAL(Val)
#endif

#if EAGER_DEREF>1
#define HW_VAL(N,Val) HNEWCELL(N,Val)
#else
#define HW_VAL(N,Val) HNEWVAL(N,Val)
#endif

#define W_VAL(Val) PUSHDERVAL(Val)

#define HNEXT(N) H += (N)

#define PUSH_TO_REG() \
      xref=C2T(An); \
      SETREF(xref,H)

#define IS_LIST(Term) (g.DOT==(Term))

#define PUSHVAL(Val) SETCELL(H++,(Val))
#define MAKE_LIST() PUSHVAL(g.DOT)

#define PUSH_LIST(Elem) {MAKE_LIST(); PUSHVAL((Elem)); }

#define PUSH_NIL() PUSHVAL(g.NIL)

#define DEREF2(x,v) \
{ while(VAR((v)=GETREF(x)) && (cell)(x)!=(v)) (x)=(term)(v); \
ASSERT1(((VAR(x) && NONVAR(v)) \
|| ((VAR(v) && VAR(x) && (cell)(x)==(v))))) \
}
/* at the end either v is nonvar or both a vars and x==v */

#define FDEREF3(Expr,Ref,Val) \
(Ref)=C2T(Expr); \
{if(NONVAR(T2C(Ref))) (Val)=T2C(Ref); \
else DEREF2((Ref),(Val));\
ASSERT1( (VAR(Ref) && NONVAR(Val)) || (cell)(Ref)==(Val) ) \
ASSERT1( ! COMPOUND((cell)Ref)  || (cell)(Ref)==(Val)    ) \
}

/* at the end either Ref is var and Val is compound or Ref==Val */

#define FDEREF(Expr) FDEREF3((Expr),xref,xval)

#if 0
#define DEREF1(Expr) \
{ register term xxref0; register cell xxval0; \
  ASSERT2( !COMPOUND(Expr),(Expr)) \
  FDEREF3((Expr),xxref0,xxval0); \
  (Expr)=COMPOUND(xxval0)?T2C(xxref0):xxval0; \
  ASSERT2(ATOMIC(Expr) || VAR(Expr),(Expr)) \
}
#else
#define DEREF1(Expr) \
{ register term xxref0; register cell xxval0; \
  ASSERT2( !COMPOUND(Expr),(Expr) ) \
  xxref0=C2T(Expr); \
  if(NONVAR(xxref0)) (Expr)=T2C(xxref0); \
  else \
    { DEREF2(xxref0,xxval0);\
     (Expr)=COMPOUND(xxval0)?T2C(xxref0):xxval0; \
    } \
  ASSERT2( !COMPOUND(Expr), (Expr) ) \
}
#endif

/* this gives the maximal arity of a builtin */
#define TEMPARGS 8

#define X(I) regs[-(I)]

#define RX(I) (C2T(X(I)))

#define TEMP(I) regs[-(I)]

#define IN_VALUE(I,Expr) \
{ ASSERT2(!COMPOUND(Expr),(Expr)) \
  xref=C2T(Expr); \
  if( NONVAR(T2C(xref)) ) X(I)=T2C(xref); \
  else \
  { DEREF2(xref,xval); \
    X(I)=(COMPOUND(xval))?T2C(xref):xval; \
  }\
  ASSERT2((ATOMIC(X(I)) || VAR(X(I))), X(I)) \
}

#define TRAIL_IT(V) \
{IF_OVER("trailing in unify",TR_TOP,TrailStk,bp_halt(12)); *TR_TOP++=(V);}

#define TRAIL_IF(V) if((V)<SAVED_H) TRAIL_IT(V)

#define SAFE_TRAIL_IT(V) *TR_TOP++=(V)

#define SAFE_TRAIL_IF(V) TRAIL_IF(V)


#ifndef NO_VALUE_TRAIL

/* Macro which tags value trailed cells
so that they can be uniformly un-trailed,
without requireing if-tests latter */

#define TR_VTAG 1
#define VTRAIL_IT(V,Val) \
{/*BUG("VTRAIL_IT called");*/ IF_OVER("value trailing",TR_TOP,TrailStk,bp_halt(14)); \
TR_TOP[0]=(term)(Val); TR_TOP[1]= (void *)(TR_VTAG+(cell)(V)); TR_TOP+=2;}

#define VTRAIL_IF(V,Val) if((V)<SAVED_H) VTRAIL_IT(V,Val)

#define FUSSY_VAR_CHECK(Xval,Xref) \
  if(Xval==(cell)Xref) \
    { LOCAL_ERR(Xval, \
      "Destructive assignment on unbound variables is unsafe"); \
      /* VTRAIL_TRACE(); */ \
    }
/* Macro for trailing only once by choice point. It allows builtins
to stamp the heap with a new cell so that it will not
trigger further trailing within the same segment */

#define SMART_VTRAIL_IF(Xref,Xval) \
  /* FUSSY_VAR_CHECK(Xval,Xref); */ \
  if((Xref<SAVED_H) && (NONVAR(Xval) || Xval<(cell)SAVED_H)) \
    VTRAIL_IT(Xref,Xval)
#endif

#define DEF_UCOUNT(Name,Val) \
  hdef(g.ucount,(Name),INPUT_INT(Val),g.timestamp)
#define GET_UCOUNT(Name) OUTPUT_INT(hget(g.ucount,(Name)))
#define SET_UCOUNT(Name,Val) hset(g.ucount,(Name),INPUT_INT(Val))
#define INC_UCOUNT(Name) SET_UCOUNT((Name),1+GET_UCOUNT(Name))

#if !defined(TRACE_EXEC) && GC==0
#define DEFPRED(Name,Val) hdef(g.predmark,(no)(Name),(no)(Val),g.timestamp)
#define SETPRED(Name,Val) hset(g.predmark,(no)(Name),(no)(Val))
#else
#if TRACE_EXEC<1
#define DEFPRED(Name,Val) \
  hdef(g.predmark,(Name),(Val),g.timestamp); \
  hdef((Val),g.predmark,(Name),g.timestamp)
#else
#define DEFPRED(Name,Val) \
  hdef(g.predmark,(Name),(Val),g.timestamp); \
  hdef((Val),g.predmark,(Name),g.timestamp); \
  DEF_UCOUNT(Name,0)
#endif
#define SETPRED(Name,Val) \
  hset(g.predmark,(Name),(Val)); \
  hdef((Val),g.predmark,(Name),g.timestamp)
#define ADDR2FUN(Addr) (cell)hget((cell)(Addr),g.predmark)
#endif

#define GETPRED(Name) (instr)hget(g.predmark,(cell)(Name))

#define ATOMIZE(Fun) if(!ATOMIC((Fun))) (Fun)=GETREF((term)(Fun))

#define NO() return 0

#define OVERSTACK(Culprit,Top,LimitNo,StackGroup,Action) {\
  overflow_by((Top),LimitNo,(StackGroup),(Culprit)); Action; \
}

#define IF_OVERSTACK(Culprit,Top,LimitNo,StackGroup,Action) \
if(((term *)(Top)) >= (StackGroup)[LimitNo].margin) \
   OVERSTACK(Culprit,Top,LimitNo,StackGroup,Action)

#define OVER(Culprit,Top,LimitNo,Action) \
  OVERSTACK(Culprit,Top,LimitNo,wam,Action)

#define IF_OVER(Culprit,Top,LimitNo,Action) \
  IF_OVERSTACK(Culprit,Top,LimitNo,wam,Action)

#define WARNING(errname) {warnmes(errname); FAILURE()}

#define ERREXIT(Mes) {warnmes(Mes); return 0;}

#define LOCAL_ERR(Sym,Msg) local_error((Sym),(Msg),wam)



#define FCALL(RetType,Fun,Args)  ( (RetType (*)())(Fun))Args



/* #define FUNCALL(Fun,Args)  ( (term (*)())(Fun))Args */



#define FUNCALL(Fun,Args) FCALL(term,Fun,Args)

#define SFUNCALL(Fun,Args) FCALL(int,Fun,Args)


/* primitive allocators */

#if TRACE > 0

#define ZALLOC(NbOfEls,TypeOfEls) \
((TypeOfEls *)calloc_trace((NbOfEls),sizeof(TypeOfEls)))

#define XALLOC(NbOfEls,TypeOfEls) \
((TypeOfEls *)malloc_trace((NbOfEls),sizeof(TypeOfEls)))

#define XFREE(Ptr) free_trace(Ptr)

#define RALLOC(OldStack,NbOfEls,TypeOfEls) \
((TypeOfEls *)realloc(OldStack,(NbOfEls)*sizeof(TypeOfEls)))

#else
/* this is the default case */

#define ZALLOC(NbOfEls,TypeOfEls) \
((TypeOfEls *)calloc((NbOfEls),sizeof(TypeOfEls)))

#define XALLOC(NbOfEls,TypeOfEls) \
((TypeOfEls *)malloc((NbOfEls)*sizeof(TypeOfEls)))

#define RALLOC(OldStack,NbOfEls,TypeOfEls) \
((TypeOfEls *)realloc(OldStack,(NbOfEls)*sizeof(TypeOfEls)))

#define XFREE(Ptr) free(Ptr)

#endif

/* derived allocators */

#define TALLOC(NbWords) XALLOC(NbWords,term)

/* C-ified instruction records */

struct bp_instr {
  byte op; bp_short reg,arity;
  char *name;
};

#define CWRITE(Arg) fout((Arg),wam,g.tellfile)

#define ERRMES(Mes,Arg) errmes((Mes),(Arg),wam)

#if JUMP_COMPRESS
#define IS_JCOMPRESSED(Fun) \
  (g.jcompressmark==hget(g.jcompressmark,(Fun)))
#define MARK_JCOMPRESSED(Fun) \
  if(!IS_JCOMPRESSED(Fun)) \
    hdef(g.jcompressmark,(Fun),g.jcompressmark,g.timestamp)
#endif

#endif

