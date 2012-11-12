/* tracing threads */
#define TTRACE 0

#include "defs.h"
#include "global.h"

extern struct specsyms g;
extern struct limit max;
extern string newlex;
extern no newatom;
extern bp_long opcount, hcount;

extern void seen_told(byte op), init_random(no seed);
extern no def(register term regs, stack wam, byte stamp), set(
    register term regs, stack wam, byte stamp), see_tell(register term regs,
    register stack wam);

extern cell hget(register no pred, register no fun), lval(register term regs,
    register stack wam, byte stamp), get_asserted(register term regs,
    register stack wam);

extern bp_long deep_hash(register cell xval, bp_long max, bp_long mod);

extern no is_engine(register stack wam), destroy_engine(register stack wam);
extern cell get_engine_prop(cell engine, bp_long propNo);
extern term list_engines(register term H), make_var_array(bp_long a_size);
extern void init_var_array(term handle, bp_long size);

extern stack create_engine(register stack oldwam, bp_long h, bp_long s,
    bp_long t);

extern term hlist(register term H, register term regs, stack wam);
extern void warnmes(string mes);
extern term local_error(cell xval, string Msg, register stack wam), functor(
    register term H, register term t, register stack wam, register term *A),
    string2list(register term H, register string name, register stack wam),
    string2list_with_length(register term H,register string name,register stack wam,bp_long count),
    name2list(register term H, register term t, register term l,
        register stack wam, register term *A), term_store_op(register term H,
        register term regs, register stack wam), new_builtin(register term H,
        register term regs, register term *A, register instr P,
        register stack wam), if0(register term regs, register term H,
        register instr P, register term *A, register stack wam);
extern no list2buf(register term l, register cell vl, register string name,
    bp_long len);
extern term
    det_append0(register term H, register term regs, register stack wam),
    call_external(register term H, register term regs, register stack wam),
    term2list(register term H, register term regs, register stack wam),
    list2term(register term H, register term regs, register stack wam),
    term_append(register term H, register term regs, register stack wam),
    setarg(register term regs, register stack wam, register term *A),
    copy_term(register term t0, register term from, register term to,
        register stack wam),
    copy_to_engine(register stack wam, register term t), apply(register term H,
        register term regs, register cell fun, register stack wam),
    strip_cont0(register term H, register term regs, register stack wam),
    dcg_connect(register term H, register term regs, register stack wam,
        register term *A), tval(register term H, register cell xval,
        register bp_long num), tlet(register term regs),

    array_ref(register cell base, register cell index, register stack wam),

    float_op(register term H, byte opcode, term t1, term t2), float_fun(
        register term H, register term regs), float_fun2(register term H,
        register term regs), input_float(register term H, register term regs,
        register stack wam),

    bb_op();

extern bp_long new_client(char *host, bp_long port);
extern bp_long new_server(bp_long port);
extern bp_long new_service(bp_long sock, bp_long t);
extern char *peer_addr(bp_long sock);
extern bp_long peer_port(bp_long sock);
extern bp_long sock_read(bp_long sock, char *buf, bp_long max);
extern bp_long sock_write(bp_long sock, char *buf, bp_long l0);
extern bp_long sock_readln(bp_long sock, char *buf, no max);
extern bp_long sock_writeln(bp_long sock, char *buf, bp_long l0);
extern bp_long file2sock(FILE *f, bp_long sock, char *buf);
extern bp_long sock2file(bp_long sock, FILE *f, char *buf);
extern bp_long close_socket(bp_long sock);
extern char *host2ip(char *hostname);

extern bp_long fsize(FILE *f);

extern void overflow_by(term *Top, bp_long LimitNo, stack wam, string culprit),
    link_halt(void), warnmes(string mes), bp_halt(bp_long i);

extern cell unix_argv(register term regs, register stack wam), unix_getenv(
    register term regs, register stack wam), unix_access(register term regs,
    register stack wam), unix_cd(register term regs, register stack wam),
    untval(register term xref);

extern int unix_fork(), unix_pid();
extern int unix_sleep(int n);

extern term
    open_stream(register term H, register term regs, register stack wam);
extern no close_stream(register term regs, register stack wam);

extern no addq0(register stack wam, register term queue, register cell elem);
extern no pushq0(register stack wam, register term queue, register cell elem);
extern term popq0(no newstk, register stack wam, register term queue);
extern term member_scan(register cell x, register cell xval,
    register stack wam, register term *A), cmember_scan(register term H,
    register cell x, register cell xval, register stack wam, register term *A,
    no *ok), cdel_scan(register term H, register cell x, register cell xval,
    register stack wam, register term *A, no *ok);

extern bp_long cputime(void);
extern void hbak(byte stamp), hcommit(byte stamp);
extern void atombak(byte stamp), atomcommit(byte stamp);
extern no make_stack(stack s, bp_long n, bp_long check, string name,
    term *area, no notzeroed);
extern cell input_fun(string name, no arity);
extern void quietmes(string format, string mes);
extern cell system0(term regs, register stack wam), new_name(cell x,
    register stack wam);
extern no load_1(register term regs, register stack wam);
extern bp_long stats0(register term H, register term regs, register stack wam,
    register term *A);

extern void fout(cell xval, stack wam, FILE *f);
extern bp_long compare(register term l, register term r);

extern no change_arg(register term regs, register stack wam, register term *A),
    older_file(register term regs, register stack wam), see_tell_at(
        register term regs, register stack wam), bb_reset(register term regs,
        register stack wam);

extern cell symcat(register term regs, register stack wam), namecat(
    register term regs, register stack wam), seeing_telling_at(
    register term regs), sout(cell xval, stack wam), unix_kill(
    register term regs, register stack wam);
extern string qsout(cell xval, stack wam);
extern bp_long add_instr(register term regs, register stack wam), op0(
    register term regs, stack wam);

extern int dcg_tell(register term regs, register stack wam, register term *A);
extern int get_engine_thread(register stack wam);
void set_engine_prop(cell engine, bp_long propNo, cell valNo);
extern cell new_func(string name, no argctr);
void qprint(cell xval, stack wam, FILE *f);

#ifdef VIVO
extern bp_long unobfuscate(bp_long i);
#endif

#if GC>0
extern term trigger_gc(register term H, register term regs, register term *A,
    register instr P, register stack wam);
#endif

#if TRACE>0 || PROF
extern cell list_asm (cell atom, cell arity, cell maxi, register stack wam);
extern void profiler (byte mes, instr P, term *A, term H, register stack wam);
extern void *calloc_trace(no nb,no size),
*malloc_trace(no nb,no size),
free_trace(void *ptr);
#endif

#if HAS_OVERRIDE
extern no override();
#endif

#ifdef BUILTIN_MEMBER
extern term member_entry(register term H, register term regs,
    register stack wam, register term *A), for_entry(register term H,
    register term regs, register stack wam, register term *A);
#endif

#ifdef BEGIN_END

#define C_CHUNK_TRACE(Mes) \
  fprintf(STD_err, \
      "%s: A=%ld, H=%ld cutb=%ld BH=%ld\n",(Mes), \
      ((bp_long)A-(bp_long)wam[ChoiceStk].base)/sizeof(bp_long), \
      ((bp_long)H-(bp_long)wam[HeapStk].base)/sizeof(bp_long), \
      INPUT_INT(cutB-(cell)wam[ChoiceStk].base), \
      ((bp_long)SAVED_H-(bp_long)wam[HeapStk].base)/sizeof(bp_long) \
  )

#else

#if TRACE>0
#define C_CHUNK_TRACE(Mes) {if(!g.inC) bp_halt(7);}
#else
#define C_CHUNK_TRACE(Mes)
#endif

#endif

static void set_bak(register stack wam) {
  bak_ctop = ctop;
  g.bak_opcount = opcount;
  g.bak_newlex = newlex;

  g.bak_newatom = newatom;
  g.bak_hcount = hcount;
}

static void push_code(register stack wam) {
  hbak(RUNTIME);
  atombak(RUNTIME);
  atomcommit(LOADTIME);
  hcommit(LOADTIME);
  set_bak(wam);
  g.shared[BBoardStk].top = g.shared[BBoardStk].base;
}

void init_orig_wam(register stack wam) {
  set_bak(wam);

  g.rtime = cputime();
  g.total_gctime = ZERO;
  g.gctime = ZERO;
}

void restart_orig(register stack wam) {
  ctop = bak_ctop;
  opcount = g.bak_opcount;
  newlex = g.bak_newlex;
  hbak(LOADTIME);
  atombak(LOADTIME);

  ASSERT2(newatom==g.bak_newatom,g.true); ASSERT2(hcount==(bp_long)g.bak_hcount,g.true);

  seen_told(0);
  seen_told(1);

  g.shared[BBoardStk].top = g.shared[BBoardStk].base;
}

term heap_or_bp_halt(register term H, register term regs, stack wam) {
  cell xval;
  bp_long ires;
  xval = X(1);
  if (!INTEGER(xval)) {
    warnmes("integer expected in halt/1");
    ires = 1;
    xval = INPUT_INT(ires);
  }
  else ires = OUTPUT_INT(xval);

  if (ires > 999) {
    bp_halt(ires);
    /* FORCED halt. bp_halt should be called,
     given that exit(N) is caught, to ensure
     foreign modules cannot kill the system
     */
  }

  SET_ENGINE_ERROR(ires);
  g.err.id = xval;

  /*fprintf(STD_err,"$$ heap_or_bp_halt(%d)\n",ires); */

  if (ires > 0) return NULL;
  return H;
}

#ifdef NO_VALUE_TRAIL
term *unwind_trail(new0,old)
register term *new0,*old;
{
  while(old<new0)
  {
    new0--;
    SETREF(*new0,*new0);
  }
  return new0;
}
#else
term *unwind_trail(register term *new0, register term *old) {
  register term adr;
  register cell tag;

  while (old < new0) {
    new0--;
    adr = *new0;
    tag = /*!!*/((cell) adr & TR_VTAG);
    adr = (term) ((cell) adr - tag);
    *adr = (cell) (new0[-(bp_long) tag]);
    new0 -= tag;
  }
  return new0;
}
#endif

#if (defined NO_VALUE_TRAIL) && (defined TIDY)
/* this is an optional operation saving lot of trail space,
 but it can cost a small amount of extra time
 Not implementted in case of value trail (It would become
 too tricky as blocks on the trail have length 1 or 2)
 */

#define TIDY_TRAIL() \
TR_TOP=tidy_trail(SAVED_TR,TR_TOP,SAVED_H)

term *tidy_trail(from,to,hb)
register term *from,*to,hb;
{
  while(from<to)
  if(*from < hb)
  from++;
  else
  *from = *(--to);
  return to;
}
#else
#define TIDY_TRAIL()
#endif

/* WAM macros */

#define SN(I) (S+((I)*sizeof(cell)))

#define SNEXT(I) S=SN(I)

#ifndef NOOFFSETS

#define SREADARG(I,R) (R)=SN(I)
#define SREADVAL(I,R) UNIFAIL((R),SN(I))

#define GETARG() S
#define NEXTARG() SNEXT(1)
#define READARG(R) (R)=GETARG();NEXTARG()
#define READVAL(R) UNIFAIL((R),GETARG()); NEXTARG()

#endif

#define MAKE_CHOICE_POINT(BPoint,Arity) \
  COPY_CELLS(A,regs,Arity);         \
  A += (Arity+3);                   \
  SAVED_H=H;                  \
  SAVED_TR=(term)TR_TOP;            \
  SAVED_P=(term)(BPoint)

#define RESTORE_STATE(Arity)    \
  TR_TOP=unwind_trail(TR_TOP,(term *)SAVED_TR);  \
  H=SAVED_H;  \
  A-=Arity+3; \
  COPY_CELLS(regs,A,Arity); \
  cutB=(cell)A

#define FAILURE() {P=SAVED_P; continue;}
/* fprintf(STD_err,
 "A=%lu: FAILING TO => code[%lu]\n",
 A-wam[ChoiceStk].base,SAVED_P-cbase); */

#define UNIFAIL(Term1,Term2) if(!unify((Term1),(Term2),wam,A)) FAILURE()
#define NEXT(Isize) P+=Isize; continue
#define CONT(Cont) regs[1]=regs[Cont]; P++; continue
#define WARFUN(Sym,Msg) {LOCAL_ERR(Sym,Msg); FAILURE()}

#define GETOPCODE() GETOPCELL(GETFIELDS(0))

#define OPCODE() GETOPCELL(fields)

#define NOT_INTEGERS(Arg1,Arg2) \
(INTTAG|(INTTAG<<TAGBITS)) != (GETTAG(Arg1)|(GETTAG(Arg2)<<TAGBITS))

#define IF_NOT_INTEGER_OP() \
ires=(bp_long)X(1); xval=X(2); \
if(NOT_INTEGERS(ires,xval))

#define FLOAT_TEST(A1,A2,Res) \
{ \
 (Res)=H; H=float_op(H,(byte)OPCODE(),C2T(A1),C2T(A2)); \
 if(!H) BFAIL() \
}

#define FLOAT_COMP(Arg1,Arg2) \
{ FLOAT_TEST((Arg1),(Arg2),xref) \
  OUT_RESULT(xref); \
}

#define FLOAT_OP_ESCAPE() \
IF_NOT_INTEGER_OP() \
FLOAT_COMP(ires,xval)

#define FLOAT_REL_ESCAPE() \
IF_NOT_INTEGER_OP() \
{ if( ! float_op(H,(byte)(OPCODE()),(term)ires,C2T(xval) )) BFAIL() \
  BNEXT(1); \
}

#define INT_COMP(Op,Arg1,Arg2) \
ires=INPUT_INT(OUTPUT_INT((bp_long)(Arg1) ) Op OUTPUT_INT((bp_long)(Arg2) )); \
OUT_RESULT(ires)

#define COMPUTE(Op) \
FLOAT_OP_ESCAPE(); \
INT_COMP(Op,ires,xval)

#define COMPUTE_DIV(Op) \
if(INTEGER(X(2)) && 0==OUTPUT_INT(X(2))) \
  BWARFUN(X(1),"divided by 0"); \
FLOAT_OP_ESCAPE(); \
INT_COMP(Op,ires,xval)
/*      ^^ to get around BC5 bug, unhappy with COMPUTE(/) */

#define COMPUTE_MOD(Op) \
if(INTEGER(X(2)) && 0==OUTPUT_INT(X(2))) \
  BWARFUN(X(1),"mod 0 invalid"); \
FLOAT_OP_ESCAPE(); \
INT_COMP(Op,ires,xval)
/*      ^^ to get around BC5 bug, unhappy with COMPUTE(/) */

#define INT_ONLY(Op) \
IF_NOT_INTEGER_OP() BFAIL() \
INT_COMP(Op,X(1),X(2))

#define MUST_BE(Relop) \
FLOAT_REL_ESCAPE(); \
if(! ( ires Relop (bp_long)xval ) ) BFAIL() \
BNEXT(1)

#define COPY_TERM(Cursor,Margin,Error) \
{\
if(!(xref=copy_term(\
                 C2T(xval), (term)(Cursor), (term)(Margin), wam))) \
   Error; \
   xval=(cell)(Cursor);\
   Cursor=(void *)xref;\
}

void bad_instr(instr P) {
  fprintf(STD_err, "*** bad instruction: [%lu] ***\n", GETOP(P));
  bp_halt(8);
}

#define BUNIFAIL(Term1,Term2) if(!unify((Term1),(Term2),wam,A)) BFAIL()
#define SAFE_UNIFAIL(Term1,Term2) \
  if(!unify_to((Term1),(Term2),wam,A)) BFAIL()
#define BNEXT(Isize) P+=Isize; BCONTINUE()
#define BWARFUN(Sym,Msg) {LOCAL_ERR(Sym,Msg); BFAIL()}
/*#define BCONT(Cont) regs[1]=regs[Cont]; P++; BCONTINUE()*/

#define SAFE_OUT(Expr) \
if(LEFTFIELD) \
  {SAFE_UNIFAIL((cell)(Expr),An)} \
else \
  An=(cell)(Expr); \
BNEXT(1)

#define OUT_RESULT(Expr) \
if(LEFTFIELD) \
  {BUNIFAIL((cell)(Expr),An)} \
else \
  An=(cell)(Expr); \
BNEXT(1)

#if TRACE_EXEC>0
void trace_exec0(P)
instr P;
{
  cell fun=ADDR2FUN(P);
  INC_UCOUNT(fun);

#if TRACE_EXEC>1
  if(fun)
  fprintf(STD_err, "EXECUTING: %s/%d\n",NAME(fun),GETARITY(fun));
  else
  fprintf(STD_err, "EXECUTING_AT: %ld\n",(cell)P);
#endif
}
#else
#define trace_exec0(Instr)
#endif

#define trace_exec(Instr) {trace_exec0(Instr);}

#if ERROR_HANDLER
#define CHECK_STOPPER()\
if(g.stopper) {\
  /*fprintf(STD_err,"stopper!!!=%ld\n",g.stopper);*/ \
  return NULL;\
}
#else
#define CHECK_STOPPER()
#endif

#if GC>0

#define CHECK_HEAP_OVERFLOW() \
if(H >= (term)wam[HeapStk].margin) \
{ \
  H=trigger_gc(H,regs,A,P,wam); \
  if(!H) { RECOVER(); } \
}
#else
#define CHECK_HEAP_OVERFLOW() \
IF_OVER("Please enlarge heap with option -h",(term*)H,HeapStk,RECOVER())
#endif

#define CUT_AND_CHECK_HEAP() \
trace_exec(P); \
CHECK_STOPPER();\
cutB=(cell)A;  \
CHECK_HEAP_OVERFLOW()

#define EXEC0() P=GETLABEL(P); CUT_AND_CHECK_HEAP()
#define EXEC() P=GETLABEL(P); CHECK_P(); CUT_AND_CHECK_HEAP()

#define COLLAPSE_FUN() H -= (H<xref+2);

#define GSTR(Advance,Rm1,Rm2,IncS,Wm1,Wm2,IncH,Steps) \
      FDEREF(An); \
      if(VAR(xval)) \
        { S=0; \
          SAFE_TRAIL_IF(xref); \
          SETREF(xref,H); \
          COLLAPSE_FUN() \
          SETCELL(H,GETFUN(P)); \
          Advance;Wm1;Wm2; IncH; \
          NEXT(Steps); \
        } \
      if(xval!=GETFUN(P)) FAILURE() \
      S=T2C(xref);Advance;Rm1;Rm2;IncS;NEXT(Steps)

#define GSTR3(Rm1,Rm2,Wm1,Wm2) \
  GSTR(GETFIELDS(2),Rm1,Rm2,SNEXT(3),Wm1,Wm2,HNEXT(3),3)

#define PSTR() \
       HNEWFUN(P); \
      An=T2C(H)

/* WAM-code emulator */

/********************
 CHOICE POINT:

 /\ <-----------* A register
 || P
 || TR
 || H
 || regs(1..arity)
 .....
 *********************/

/*****************************************************************
 initially P points to the first put_structure in the last clause
 that is INTERPRETED AS A GOAL. The clause must be of the form:

 answer:-pred(_).

 where 'pred' is a unary predicate defined previously
 ******************************************************************/

/* unique engine id, always incremented by add_engine */
static no engine_unique_id = ZERO;

#define INIT_NEW_INTERP() \
  A=wam[ChoiceStk].base;     \
  TR_TOP=wam[TrailStk].base; \
  H=MAKEREGS(wam[HeapStk].base);\
  { term g_connect=DCGSTART(); \
    init_var_array(g_connect,MAXDCG);\
    SETREF(g_connect,g_connect+1); \
    SETREF(g_connect+1,g_connect+1); \
  }\
  set_engine_prop(PTR2INT(wam),ENGINE_ID,INPUT_INT(engine_unique_id++));

#define START_INTERP() \
  INIT_NEW_INTERP();   \
  NEWVAR(regs[1]);           \
  regs[2]=g.true;            \
  MAKE_CHOICE_POINT((cbase),2)

#define PREP_CALL(Goal) \
regs[1]=(cell)(Goal); \
regs[2]=g.true; \
P=g.call; \
MAKE_CHOICE_POINT(cbase,2)

static term bp(register term regs, register term H, register instr P,
    register term *A, register stack wam);

term bp_prolog_call(register term goal, register term regs, register term H,
    register instr P, register term *A, register stack wam)
{
  PREP_CALL(goal);
  return bp(regs, H, P, A, wam);
}

#define SAVE_HAP() \
  wam[HeapStk].top=(term *)H; \
  /* TR_TOP is already there */ \
  wam[ChoiceStk].top=A; \
  SET_ENGINE(ENGINE_P,P)

#define RESTORE_HAP() \
  H=(term)wam[HeapStk].top; \
  A=wam[ChoiceStk].top; \
  P=GET_ENGINE(ENGINE_P)

term bp_abort(register term H, register instr P, register term *A,
    register stack wam, string mes)
{
  if (mes) warnmes(mes);
  link_halt();
  SET_ENGINE_ERROR(ENGINE_ABORT);
  SAVE_HAP();
  return NULL;
}

#define ABORT(Mes) return bp_abort(H,P,A,wam,Mes)
#define RECOVER() return bp_abort(H,P,A,wam,NULL)

#if 1
instr exec_link(register term H, register instr P, register term *A,
    register stack wam)
{
  instr NewP;
  if (!(NewP = GETPRED(P))) {
    (void) LOCAL_ERR((cell)P,"undefined in EXEC");
    (void) bp_abort(H, P, A, wam, NULL);
  }
  return NewP;
}
#define CHECK_P() \
  if( NONVAR((cell)P) && !(P=exec_link(H,P,A,wam))) return NULL;
#else
#define CHECK_P()
#endif

term load_engine(register stack wam, cell goal, cell answer) {
  register term regs;
  register term H;
  register term *A;
  register instr P;
  if (!is_engine(wam)) {
    warnmes("load_engine: bad engine");
    return NULL;
  }

  INIT_NEW_INTERP();

  {
    term t, ct;
    t = H;
    H[0] = g.DOT;
    H[1] = goal;
    H[2] = answer;
    H += 3;
    wam[HeapStk].top = (term *) H;
    ct = copy_to_engine(wam, t);
    H = (term) wam[HeapStk].top;
    {
      cell xval;
      DEREF2(ct,xval);
    }
    if (g.DOT != ct[0]) {
      warnmes("load engine: error in copy_to_engine");
      return NULL;
    }
    goal = (cell) (ct + 1);
    answer = (cell) (ct + 2);
  }

  PREP_CALL(goal);
  SET_ENGINE_TYPE(LOADED_ENGINE);
  SET_ENGINE(ENGINE_GOAL,goal);
  SET_ENGINE(ENGINE_ANSWER,answer);
  SAVE_HAP();
  return H;
}

term load_engine0(register term regs) {
  return load_engine(INT2PTR(X(1)), /* handle i.e. struct wam */
  X(2), /* goal */
  X(3) /* answer variable */
  );
}

term call_engine(register stack wam) {
  register term regs;
  register term H;
  register term *A;
  register instr P;
  term bp(register term regs, register term H, register instr P,
      register term *A, register stack wam);
  if (!is_engine(wam)) return NULL; TRACE_ENGINE("==> entering call_engine");
  RESTORE_HAP();
  regs = LOCATEREGS();
  TRACE_ENGINE("==> entering new bp in call_engine");
  H = bp(regs, H, P, A, wam);
  if (!H) SET_ENGINE_TYPE(DEAD_ENGINE);
  /*warnmes("dead_engine");*/
  TRACE_ENGINE("<== exiting call_engine");
  return H;
}

term ask_engine(register stack wam) {
  bp_long curr_type;
  TRACE_ENGINE("ask_engine");
  curr_type = GET_ENGINE_TYPE();
  switch (curr_type) {
    case LOADED_ENGINE:
    case RUNNING_ENGINE:
    case HALTED_ENGINE:
      if (call_engine(wam)) {
        if (SUSPENDED_ENGINE == GET_ENGINE_TYPE()) {
          SET_ENGINE_TYPE(curr_type);
          return (term) GET_ENGINE(ENGINE_RETURN);
        }
        return (term) GET_ENGINE(ENGINE_ANSWER);
      }
    break;

    case DEAD_ENGINE:
#ifdef AUTO_DESTR
      (void)destroy_engine(wam);
#endif
    break;

    default:
      (void) local_error(T2C(GET_ENGINE(ENGINE_TYPE)),
          "ask_engine: bad engine type", wam);
      bp_halt(18);
  }
  return NULL;
}

/* executed by PROCEED */
term answer(register stack wam, register term H, register term *A,
    register instr P)
{
  TRACE_ENGINE("PROCEED->answer")
  switch (GET_ENGINE_TYPE()) {
    case LOADED_ENGINE:
      SET_ENGINE_TYPE(HALTED_ENGINE);
    case ROOT_ENGINE:
      SAVE_HAP()
      ;
      return H;

    case HALTED_ENGINE:
      SET_ENGINE_TYPE(LOADED_ENGINE);
    break;

    case DEAD_ENGINE:
      warnmes("answer requested from DEAD_ENGINE");
      /*break;*/

    default:
      (void) local_error(T2C(GET_ENGINE(ENGINE_TYPE)),
          "answer: bad engine type", wam);
      bp_halt(16);
  }
  return NULL;
}

/* THREADS */

#if 1==THREADS

void FORCE_STDCALL ask_thread(register stack wam) {
  if (!is_engine(wam)) {
    warnmes("ask_thread: bad engine");
    return;
  }
  init_random(0);
  ask_engine(wam);
}

#if VCC>0
/* begin WIN32 THREADS */
#include <windows.h>
/*
 #include <process.h>
 #include <string.h>*/

static string nothreads="stub, THREADS on the way to be implemented for WIN32";
#define NOTHREADS() {warnmes(nothreads);}

/* ask_thread already defined */

cell FORCE_CDECL _beginthreadex(
    void *security, unsigned stack_size, unsigned ( __stdcall *start_address )( void * ),
    void *arglist, unsigned initflag, unsigned *thrdaddr );

void FORCE_CDECL _endthreadex( unsigned retval );

bp_long tcreate(void *start_func, void *arg) {
  HANDLE tid; unsigned int taddr;
  tid = (HANDLE) _beginthreadex(
      NULL,
      0,
      start_func,
      arg,
      0, /*CREATE_SUSPENDED*/
      &taddr
  );

  return (bp_long)tid;
}

bp_long current_thread(stack wam) {
  return get_engine_thread(wam);
}

void thread_exit(stack wam) {
  /*warnmes("entering thread_exit: this does not close HANDLE");*/
  set_engine_prop(PTR2INT(wam),ENGINE_TYPE,INPUT_INT(DEAD_ENGINE));
  _endthreadex(0);
}

bp_long thread_destroy(bp_long tid) {
  TerminateThread((HANDLE)tid,0);
  CloseHandle((HANDLE)tid);
  return 1;
}

void thread_join(bp_long tid) {
  WaitForSingleObject((HANDLE)tid,INFINITE);
  CloseHandle((HANDLE)tid);
}

typedef struct tsync_data {
  /* Critical Section mp;*/
  HANDLE mp;
  /* pthread_cond_t cvp; */
}*tsync_data;

static struct tsync_data all_tsyncs[MAXTSYNC];
static bp_long tsync_initialized=0;

void init_mutexes() {
  if(!tsync_initialized) {
    bp_long k;
    for(k=0;k<MAXTSYNC;k++) {
      all_tsyncs[k].mp = CreateMutex( NULL, FALSE, NULL );
    }
    tsync_initialized=1;
  }
}

void release_mutexes() {
  if(tsync_initialized) {
    bp_long k;
    for(k=0;k<MAXTSYNC;k++) {
      ReleaseMutex(all_tsyncs[k].mp);
      CloseHandle(all_tsyncs[k].mp);
    }
    tsync_initialized=0;
  }
}

bp_long tsync_op(bp_long i,bp_long ts_no,bp_long arg) {
  tsync_data ts; bp_long result=1;
  /* init_mutexes();
   happens in ru.c
   */

#if TTRACE>0
  fprintf(STD_err,
      "entering tsync op: i=%d: ts_no=%d arg=%d\n",
      i,ts_no,arg);
#endif

  if(i<6 && (ts_no<0 || ts_no>=MAXTSYNC))
  result=0;
  else {
    ts=all_tsyncs+ts_no;

    switch(i) {
      case 0: /* not intended to be used */
      warnmes("should happen once each: initialising/releasing mutexes");
      /* if(ts_no==0) release_mutexes();
       else init_mutexes();
       */
      break;

      case 1:
      { DWORD code;
        if(arg==0) arg=INFINITE;

#if TTRACE>0
        fprintf(STD_err,"thread waits for: %d\n",ts_no);
#endif
        code=WaitForSingleObject(ts->mp, arg);
        result=(WAIT_TIMEOUT!=code);
        /* EnterCriticalSection(&(ts->mp)); */
#if TTRACE>0
        fprintf(STD_err,"thread locks: %d\n",ts_no);
#endif
      }
      break;

      case 2:
#if TTRACE>0
      fprintf(STD_err,"thread: unlocks: %d\n",ts_no);
#endif
      /* LeaveCriticalSection(&(ts->mp)); */
      ReleaseMutex(ts->mp);
      break;

      case 3:
      case 4:
      case 5:
      warnmes("3,4,5 unimplemented");
      result=0;
      break;

      case 6:
      if(ts_no==0) {
        /* warnmes("request to destroy main thread ignored"); */
      }
      else
      result=thread_destroy(ts_no); /* 1 if ok 0 if fails */
      break;

      case 7:
      if(ts_no==0)
      warnmes("request to resume main thread ignored");
      else
      ResumeThread((HANDLE)ts_no);
      break;

      case 8:
      if(ts_no==0)
      warnmes("request to suspend main thread ignored");
      else
      SuspendThread((HANDLE)ts_no);
      break;

      /*
       case 10:
       if(ts->done>0) {
       warnmes("deleting critical section");
       ts->done=0;
       DeleteCriticalSection(&(ts->mp));
       }
       break;

       case 11: {
       if(!ts->done) {
       ts->done++;
       InitializeCriticalSection(&(ts->mp));
       }
       EnterCriticalSection(&(ts->mp));
       }
       break;

       case 12:
       if(!ts->done) {
       warnmes("nonexisting critical section");
       }
       else {
       LeaveCriticalSection(&(ts->mp));
       }
       break;
       */

      default:
      fprintf(STD_err,"BAD tsync_op: %d\n",i);
      result=0;
    }
  }

  /* end else */

  if(result==0)
  fprintf(STD_err,"tsync/3 error: op=%d,ts_no=%d,arg=%d\n",i,ts,arg);

#if TTRACE>0
  fprintf(STD_err,
      "exiting tsync: %d guard?: %d arg: %d returns=> %d\n",
      i,ts_no,arg,result);
#endif

  return result;
}

/* end WIN32 THREADS */
#else
/* begin POSIX THREADS */
#if !defined __USE_GNU
#define __USE_GNU
#endif

#include <pthread.h>
#include <errno.h>

#if defined PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#define MUT_INIT PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP
#else
#define MUT_INIT PTHREAD_MUTEX_INITIALIZER
#endif

typedef struct tsync_data {
bp_long done;
pthread_mutex_t mp;
pthread_cond_t cvp;
pthread_t tid;
}*tsync_data;

#define MAXTGUARDS (MAXTSYNC<<1)

static struct tsync_data all_tsyncs[MAXTGUARDS];

static bp_long tsync_initialized = 0;

static pthread_mutex_t recmutex =
MUT_INIT;
static pthread_cond_t condinit =
PTHREAD_COND_INITIALIZER;

void init_mutexes() {
  if (!tsync_initialized) {
    bp_long k;
    for (k = 0; k < MAXTGUARDS; k++) {
      all_tsyncs[k].mp = recmutex;
      all_tsyncs[k].cvp = condinit;
      all_tsyncs[k].tid = (pthread_t) ZERO;
    }
    tsync_initialized = 1;
  }
}

void release_mutexes() {
  if (tsync_initialized) {
    bp_long k;
    for (k = 0; k < MAXTGUARDS; k++) {
      pthread_mutex_destroy(&(all_tsyncs[k].mp));
      pthread_cond_destroy(&(all_tsyncs[k].cvp));
      if ((pthread_t) ZERO != all_tsyncs[k].tid) pthread_cancel(
          all_tsyncs[k].tid);
      all_tsyncs[k].tid = (pthread_t) ZERO;
    }
    tsync_initialized = 0;
  }
}

bp_long tcreate(void *start_func, void *arg) {
  pthread_t tid;
  pthread_create(&tid, NULL, start_func, arg);
  return (bp_long) tid;
}

static bp_long put_tid_guard(pthread_t tid) {
  bp_long k;
  for (k = MAXTSYNC; k < MAXTGUARDS; k++) {
    if (tid > ZERO && (pthread_t) ZERO == all_tsyncs[k].tid) all_tsyncs[k].tid
        = tid;
    return k;
  }
  return 0;
}

static bp_long get_tid_guard(pthread_t tid) {
  bp_long k;
  for (k = MAXTSYNC; k < MAXTGUARDS; k++) {
    if (tid > ZERO && tid == all_tsyncs[k].tid) return k;
  }
  return 0;
}

/*
 static bp_long rm_tid_guard(pthread_t tid) {
 bp_long k=get_tid_guard(tid);
 if(0==k) return 0;
 all_tsyncs[k].tid=(pthread_t)ZERO;
 }
 */

static bp_long thread_suspend_self() {
  pthread_t tid = pthread_self();
  bp_long k = put_tid_guard(tid);
  /*fprintf(STD_err,"suspend: tid=%ld, k=%d\n",tid,k);*/
  if (0 == k) return 0;
  pthread_mutex_lock(&(all_tsyncs[k].mp));
  all_tsyncs[k].tid = pthread_self();
  while (all_tsyncs[k].tid != (pthread_t) ZERO) {
    pthread_cond_wait(&(all_tsyncs[k].cvp), &(all_tsyncs[k].mp));
  }
  pthread_mutex_unlock(&(all_tsyncs[k].mp));
  return 1;
}

static bp_long thread_resume_other(pthread_t tid) {
  bp_long k = get_tid_guard(tid);
  /*fprintf(STD_err,"resume: tid=%ld, k=%d\n",tid,k);*/
  if (0 == k) return 0;
  pthread_mutex_lock(&(all_tsyncs[k].mp));
  all_tsyncs[k].tid = (pthread_t) ZERO;
  pthread_cond_broadcast(&(all_tsyncs[k].cvp));
  pthread_mutex_unlock(&(all_tsyncs[k].mp));
  return 1;
}

bp_long current_thread(stack wam) { /* expects that pthread_t = bp_long */
  return (bp_long) pthread_self();
}

void thread_exit(stack wam) {
#if TTRACE>0
  fprintf(STD_err,"exiting thread %d\n",current_thread());
#endif
  set_engine_prop(PTR2INT(wam), ENGINE_TYPE, INPUT_INT(DEAD_ENGINE));
  pthread_exit(NULL);
}

void thread_join(bp_long tid) {
#if TTRACE>0
  fprintf(STD_err,"joining thread %d\n",tid);
#endif
  pthread_join((pthread_t) tid, NULL);
}

bp_long thread_destroy(bp_long tid) {
  bp_long ok = 1;
  /*if(tid==current_thread()) return ok;*/
#if TTRACE>0
  fprintf(STD_err,"cancelling_thread: %d\n",
      tid);
#endif
  ok = pthread_cancel((pthread_t) tid);
#if TTRACE>0
  fprintf(STD_err,
      "FAILED cancelling_thread: %d code=%d\n",
      tid,retcode);
#endif
  if (ESRCH == ok) ok = 0; /* no such thread anymore - ok */
  return 0 == ok;
}

bp_long tsync_op(bp_long i, bp_long ts_no, bp_long arg) {

  tsync_data ts;
  bp_long result = 1;
#if VCC==0
  // OS X fix
  return result;
#endif
  if (i < 6 && (ts_no < 0 || ts_no >= MAXTSYNC)) result = 0;
  else {

    init_mutexes();
    ts = all_tsyncs + ts_no;

    switch (i) {
      case 0: /* not intended to be used */
        pthread_mutex_destroy(&(ts->mp));
        pthread_cond_destroy(&(ts->cvp));
      break;

      case 1:
        pthread_mutex_lock(&(ts->mp));
#if TTRACE>0
        fprintf(STD_err,"thread: %d locks: %d\n",
            current_thread(),ts_no);
#endif
      break;

      case 2:
#if TTRACE>0
        fprintf(STD_err,"thread: %d unlocks: %d\n",
            current_thread(),ts_no);
#endif
        pthread_mutex_unlock(&(ts->mp));

      break;

      case 3:
        if (0 == arg) {
#if TTRACE>0
          fprintf(STD_err,"thread: %d entering_wait_on: %d\n",
              current_thread(),ts_no);
#endif
          pthread_cond_wait(&(ts->cvp), &(ts->mp));
#if TTRACE>0
          fprintf(STD_err,"thread: %d exiting_wait_on: %d\n",
              current_thread(),ts_no);
#endif
        }

        else if (arg > 0) { /* stop after timeout requested */
          bp_long retry = 1;
          bp_long ctr = 100;
          struct timespec abstime;
          abstime.tv_sec = time(NULL) + arg; /* arg=timout wait secs */
          abstime.tv_nsec = 0;
#if TTRACE>0
          fprintf(STD_err,
              "thread: %d entering_timed_wait_on: %d for %d secs\n",
              current_thread(),ts_no,arg);
#endif
          while (retry && ctr-- > 0) {
            /* illusion of wake_up once and get the mutex */
            /* relies on the fact that the condition cannot change */
            /* using abstime ensures this will eventually terminate */
            retry = pthread_cond_timedwait(&(ts->cvp), &(ts->mp), &abstime);
#if TTRACE>0
            fprintf(STD_err,
                "thread: %d timed_out_wait_on: %d errno=%d\n",
                current_thread(),ts_no,retry);
#endif
            if (ETIMEDOUT == retry || ETIME == retry) {
              result = 2; /*timed out*/
              break;
            }
            if (EINTR == retry) {
              result = 3; /* interrupted */
              break;
            }
          }
#if TTRACE>0
          fprintf(STD_err,"thread: %d exiting_wait_on: %d\n",
              current_thread(),ts_no);
#endif
        }
      break;

      case 4:
        pthread_mutex_lock(&(ts->mp));
#if TTRACE>0
        fprintf(STD_err,"thread: %d notifying_on: %d\n",
            current_thread(),ts_no);
#endif
        pthread_cond_signal(&(ts->cvp));
        pthread_mutex_unlock(&(ts->mp));
      break;
      case 5:
        pthread_mutex_lock(&(ts->mp));
#if TTRACE>0
        fprintf(STD_err,"thread: %d notifying_all_on: %d\n",
            current_thread(),ts_no);
#endif
        pthread_cond_broadcast(&(ts->cvp));
        pthread_mutex_unlock(&(ts->mp));
      break;

      case 6:
        result = thread_destroy(ts_no); /* 1 if ok 0 if fails */
      break;

      case 7: /*it is ok to resume if already running */
        thread_resume_other((pthread_t) ts_no);
      break;
      case 8:
        /* thread suspend: does nothing on POSIX */
        /* we really only need this to work for the
         case when the thread itself decides to suspend,
         to be able to emulate Linda operations on the blackboard
         - it looks that having a condition variable belonging to
         the thread itself would do it nicely
         */
        if (!thread_suspend_self()) fprintf(STD_err,
            "error in thread_suspend_self: arg=%ld\n", ts_no);
      break;
      default:
        fprintf(STD_err, "BAD tsync_op: %ld\n", i);
        result = 0;
    }
  } /* end else */

#if TTRACE>0
  fprintf(STD_err,
      "thread: %d op: %d guard?: %d arg: %d returns=> %d\n",
      current_thread(),i,ts_no,arg,result);
#endif

  return result;
}
/* end POSIX THREADS */
#endif
#else
/* begin NO THREADS */
/* THREADS not defined or 0: no threads make sense on this machine/OS */

static string nothreads="sorry, no threads implemented";
#define NOTHREADS() {warnmes(nothreads);}

void ask_thread(register stack wam) {
  ask_engine(wam); /* still a reasonable thing to do */
}

bp_long tcreate(void *start_func, void *arg) {
  NOTHREADS(); return 0;
}

void init_mutexes() {
#if TTRACE>0
  NOTHREADS();
#endif
}

void release_mutexes() {
#if TTRACE>0
  NOTHREADS();
#endif
}

/* pthread_t */
bp_long current_thread(stack wam) {
#if TTRACE>0
  NOTHREADS();
#endif
  return 0;
}

void thread_exit(stack wam) {
#if TTRACE>0
  NOTHREADS();
#endif
}

void thread_join(bp_long tid) {
#if TTRACE>0
  NOTHREADS();
#endif
}

/* called from ru.c, do nothing */

bp_long thread_destroy(bp_long tid) {
#if TTRACE>0
  NOTHREADS();
#endif
  return 0;
}

bp_long tsync_op(bp_long i,bp_long ts_no,bp_long arg) {
#if TTRACE>0
  NOTHREADS();
#endif
  return 1;
}
/* end NO THREADS */
#endif

#define PUSH(top,a) (*++(top)=(cell)(a))
#define POP(top) (*(top)--)
#define NOTEMPTY(top,base) ((top)>(term)(base))
#define BIND(Var,Ref,Val) \
{SETCELL(Var,COMPOUND(Val)?T2C(Ref):Val); TRAIL_IF(Var);}

#define BIND_IT(Var,Ref,Val) \
{SETCELL(Var,COMPOUND(Val)?T2C(Ref):Val); TRAIL_IT(Var);}

/*
 always binds from second to first, except for var to nonvar
 always trails to avoid heap-bboard order dependencies
 */

no unify_to(register cell v1, register cell v2, register stack wam,
    register term *A)
{
  register term U = (term) A;

  PUSH(U,v1);
  PUSH(U,v2); /* push v1 _then_ v2 */

  while (NOTEMPTY(U,A)) {
    register term t2 = U--, t1 = U--; /* pop ref t2 _then_ ref t1 */
    DEREF2(t1,v1);
    DEREF2(t2,v2);
    if (t1 != t2) {
      if (VAR(v1)) /* unb. var. v1 */
      {
        if (VAR(v2)) /*unb. var. v2 */
        {
          SETCELL(C2T(v2),v1);
          TRAIL_IT(C2T(v2));
        }
else          BIND_IT(C2T(v1),t2,v2)
        }
        else if(VAR(v2)) /* v1 is NONVAR */
        BIND_IT((term)v2,t1,v1)
        else if(v1!=v2) /* both are NONVAR and diff */
        return FALSE;
        else /* NONVAR and equal */
        { register no arity=GETARITY(v1);

          if(IDENTIFIER(v1) && arity)
          { /* they have the same FUNCTOR, v1==v2 */
            IF_OVER("unify",(term *)U,ChoiceStk,bp_halt(10));
            PUSH(U,t1+arity); PUSH(U,t2+arity);
            while(--arity)
            {
              PUSH(U,t1[arity]);
              PUSH(U,t2[arity]);
            }
#if (/*0 && $$$*/ !defined NO_VALUE_TRAIL)
            /* BUG("unify_to: VTRAIL_IT called"); */
            VTRAIL_IT(t2,v2);
            SETREF(t2,t1);
#endif
          }
        }
      }
    }
    return TRUE;
  }

no unify(register cell v1, register cell v2, register stack wam,
    register term *A)
{
  register term U = (term) A;
  /* top of push down list:  therefore the OR-stack should have
   MAXARITY or more left after the margin */

  PUSH(U,v1);
  PUSH(U,v2); /* push v1 _then_ v2 */

  while (NOTEMPTY(U,A)) {
    register term t2 = U--, t1 = U--; /* pop ref t2 _then_ ref t1 */
    DEREF2(t1,v1);
    DEREF2(t2,v2);
    if (t1 != t2) {
      if (VAR(v1)) /* unb. var. v1 */
      {
        if (VAR(v2) && v2 > v1) /*unb. var. v2 */
        {
          SETCELL(C2T(v2),v1);
          TRAIL_IF(C2T(v2));
        }
else          BIND(C2T(v1),t2,v2)
        }
        else if(VAR(v2)) /* v1 is NONVAR */
        BIND((term)v2,t1,v1)
        else if(v1!=v2) /* both are NONVAR */
        return FALSE;
        else
        { register no arity=GETARITY(v1);
          if(IDENTIFIER(v1) && arity)
          { /* they have the same FUNCTOR, v1==v2 */
            IF_OVER("unify",(term *)U,ChoiceStk,bp_halt(10));
            PUSH(U,t1+arity); PUSH(U,t2+arity);
            while(--arity)
            {
              PUSH(U,t1[arity]);
              PUSH(U,t2[arity]);
            }
#if (!defined NO_VALUE_TRAIL)
            /* BUG("unify: VTRAIL_IF called - only if not in same segment"); */
            if(t2>t1)
            {
              VTRAIL_IF(t2,v2);
              SETREF(t2,t1);
            }
            else
            {
              VTRAIL_IF(t1,v1);
              SETREF(t1,t2);
            }
#endif
          }
        }
      }
    }
    return TRUE;
  }

static term bp(register term regs, register term H, register instr P,
    register term *A, register stack wam)
{
  register cell S = (cell) ZERO;
  register cell cutB = (cell) P;

  char sbuf[MAXSBUF];

  for (;;) {
    register term xref;
    register cell xval;
    register cell fields;
    register bp_long ires;

#if TRACE>1 || PROF
    profiler(1,P,A,H,wam);
#endif
    ASSERT2(RUNTIME==g.timestamp,H);

    /*if(g.stopper) {
     fprintf(STD_err,"OPCODE: %d->%d H=%d P=%d\n",
     GETOPCODE(),(GETOPCODE())>>g.stopper,H,P);
     }
     switch((GETOPCODE())>>g.stopper) 10% slowdown
     */
    switch (GETOPCODE()) {

      case END: /* assert(*cbase==END); */
        /*fprintf(STD_err,"END: reached%d\n",g.stopper);*/
        return NULL;

      case UNIFY_VARIABLE:
        if (S) {
          READARG(An);
          NEXT(1)
          ;
        }
      case WRITE_VARIABLE:
        NEWVAR(An)
        ;
        NEXT(1)
        ;

      case UNIFY_VALUE:
        if (S) {
          READVAL(An);
          NEXT(1)
          ;
        }
        PUSHDERVAL(An)
        ;
        NEXT(1)
        ;

      case WRITE_VALUE:
        W_VAL(An)
        ;
        NEXT(1)
        ;

      case GET_STRUCTURE:
        GSTR(EMPTY,EMPTY,EMPTY,SNEXT(1),EMPTY,EMPTY,HNEXT(1),2)
        ;

      case PUT_STRUCTURE:
        PSTR()
        ;
        HNEXT(1);
        NEXT(2)
        ;

      case MOVE_REG:
        An = Ai;
        NEXT(1)
        ;

      case PUT_VARIABLE:
        NEWVAR2(An,Ai)
        ;
        NEXT(1)
        ;

      case GET_VALUE:
        UNIFAIL(An,Ai)
        NEXT(1)
        ;

#if STRUCT_COMPRESS
      case MOVE_REGx2:
        An = Ai;
        GETFIELDS(1);
        An = Ai;
        NEXT(2)
        ;

      case UNIFY_VAR_VAR:
        if (S) {
          SREADARG(0,An);
          SREADARG(1,Ai);
          SNEXT(2);
          NEXT(1)
          ;
        }
      case WRITE_VAR_VAR:
        HNEWVAR(0,An)
        ;
        HNEWVAR(1,Ai)
        ;
        HNEXT(2);
        NEXT(1)
        ;

      case UNIFY_VAL_VAR:
        if (S) {
          SREADVAL(0,An);
          SREADARG(1,Ai);
          SNEXT(2);
          NEXT(1)
          ;
        }
#if EAGER_DEREF==0
        case WRITE_VAL_VAR:
#endif
        HNEWVAL(0,An)
        ;
        HNEWVAR(1,Ai)
        ;
        HNEXT(2);
        NEXT(1)
        ;

#if EAGER_DEREF>0
      case WRITE_VAL_VAR:
        HW_VAL(0,An)
        ;
        HNEWVAR(1,Ai)
        ;
        HNEXT(2);
        NEXT(1)
        ;
#endif

      case UNIFY_VAL_VAL:
        if (S) {
          SREADVAL(0,An);
          SREADVAL(1,Ai);
          SNEXT(2);
          NEXT(1)
          ;
        }
#if EAGER_DEREF==0
        case WRITE_VAL_VAL:
#endif
        HNEWVAL(0,An)
        ;
        HNEWVAL(1,Ai)
        ;
        HNEXT(2);
        NEXT(1)
        ;

#if EAGER_DEREF>0
      case WRITE_VAL_VAL:
        HW_VAL(0,An)
        ;
        HW_VAL(1,Ai)
        ;
        HNEXT(2);
        NEXT(1)
        ;
#endif

      case UNIFY_VAR_VAL:
        if (S) {
          SREADARG(0,An);
          SREADVAL(1,Ai);
          SNEXT(2);
          NEXT(1)
          ;
        }
#if EAGER_DEREF==0
        case WRITE_VAR_VAL:
#endif
        HNEWVAR(0,An)
        ;
        HNEWVAL(1,Ai)
        ;
        HNEXT(2);
        NEXT(1)
        ;

#if EAGER_DEREF>0
      case WRITE_VAR_VAL:
        HNEWVAR(0,An)
        ;
        HW_VAL(1,Ai)
        ;
        HNEXT(2);
        NEXT(1)
        ;
#endif

#endif

      case UNIFY_CONSTANT:
        if (S) {
          xref = (term) GETREF((term)GETARG());
          NEXTARG();
          FDEREF(xref);
          if (VAR(xval)) {
            SETCELL(xref,GETFUN(P));
            SAFE_TRAIL_IF(xref);
          }
          else if (xval != GETFUN(P))
          FAILURE()
          NEXT(2)
          ;
        }
        PUSHVAL(GETFUN(P));
        NEXT(2)
        ;

      case PUSH_STRUCTURE:
        PUSH_TO_REG()
        ;
      case WRITE_CONSTANT:
        PUSHVAL(GETFUN(P));
        NEXT(2)
        ;

      case PUSH_VARIABLE:
        ABORT("unexpected push_variable");

      case GET_CONSTANT:
        FDEREF(An)
        ;
        if (VAR(xval)) {
          SETCELL(xref, GETFUN(P)); /*fill the unb. VAR with a const*/
          SAFE_TRAIL_IF(xref);
        }
        else if (xval != GETFUN(P))
        FAILURE()
        NEXT(2)
        ;

      case PUT_CONSTANT:
        An = GETFUN(P);
        NEXT(2)
        ;

      case PUSH_CUT:
        PUSHVAL(CUT2INT(cutB));
        NEXT(1)
        ;

      case PUT_CUT:
        A = (term *) cutB;
        TIDY_TRAIL();
        NEXT(1)
        ;

      case GET_CUT:
        FDEREF(regs[1])
        ;
        A = (term *) INT2CUT(xval);
        TIDY_TRAIL();
        NEXT(1)
        ;

      case EXECUTE:
        EXEC()
        ;
        continue;

      case PROCEED:
        H = answer(wam, H, A, P);
        if (H) return H;
        FAILURE()
        ;

#if JUMP_COMPRESS
      case EXEC_SWITCH:
        EXEC0()
        ;
#endif
      case SWITCH:
        FDEREF(regs[1])
        ;
        if (VAR(xval)) {/* regs[1]=xval; */
          NEXT(2)
          ;
        }
        if (!(P = (instr) hget(GETFUN(P), xval)))
        FAILURE()
#if STRUCT_COMPRESS
        /* $$$ to check */
        S = T2C(xref+1);
        NEXT(2)
        ;
#else
        continue;
#endif

        /* the OFFSETS optimization replaces H++=.. with H[i]=.. and H+=N..*/

#if JUMP_COMPRESS
      case EXEC_JUMP_IF:
        EXEC0()
        ;
#endif
      case JUMP_IF:
        FDEREF(regs[1])
        ;
        if (VAR(xval)) {/* regs[1]=xval; */
          NEXT(2)
          ;
        }
        {
          register instr label = GETLABEL(P);
          if (xval != GETFUN(label)) {
            NEXT(4)
            ;
          }
#if STRUCT_COMPRESS
          S = T2C(xref);
          fields = *(term) (label + 2);
          SREADARG(1,An);
          SREADARG(2,Ai);
          SNEXT(3);
          P = label + 3;
#else
          P=label;
#endif
          continue;
        }

#if STRUCT_COMPRESS
      case GET_UNIFY_VAL_VAR:
        GSTR3(SREADVAL(1,An),SREADARG(2,Ai),HNEWVAL(1,An),HNEWVAR(2,Ai))
        ;

      case GET_UNIFY_VAL_VAL:
        GSTR3(SREADVAL(1,An),SREADVAL(2,Ai),HNEWVAL(1,An),HNEWVAL(2,Ai))
        ;

      case GET_UNIFY_VAR_VAR:
        GSTR3(SREADARG(1,An),SREADARG(2,Ai),HNEWVAR(1,An),HNEWVAR(2,Ai))
        ;

      case GET_UNIFY_VAR_VAL:
        GSTR3(SREADARG(1,An),SREADVAL(2,Ai),HNEWVAR(1,An),HNEWVAL(2,Ai))
        ;

      case PUT_WRITE_VAR_VAR:
        PSTR()
        ;
        GETFIELDS(2);
        HNEWVAR(1,An)
        ;
        HNEWVAR(2,Ai)
        ;
        HNEXT(3);
        NEXT(3)
        ;

      case PUT_WRITE_VAL_VAR:
        PSTR()
        ;
        GETFIELDS(2);
        HW_VAL(1,An)
        ;
        HNEWVAR(2,Ai)
        ;
        HNEXT(3);
        NEXT(3)
        ;

      case PUT_WRITE_VAL_VAL:
        PSTR()
        ;
        GETFIELDS(2);
        HW_VAL(1,An)
        ;
        HW_VAL(2,Ai)
        ;
        HNEXT(3);
        NEXT(3)
        ;

      case PUT_WRITE_VAR_VAL:
        PSTR()
        ;
        GETFIELDS(2);
        HNEWVAR(1,An)
        ;
        HW_VAL(2,Ai)
        ;
        HNEXT(3);
        NEXT(3)
        ;

      case PUSH_VAR_VAR:
        PUSH_TO_REG()
        ;
      case CONSTANT_VAR_VAR:
        HNEWFUN(P);
        GETFIELDS(2);
        HNEWVAR(1,An)
        ;
        HNEWVAR(2,Ai)
        ;
        HNEXT(3);
        NEXT(3)
        ;

      case PUSH_VAL_VAR:
        PUSH_TO_REG()
        ;
      case CONSTANT_VAL_VAR:
        HNEWFUN(P);
        GETFIELDS(2);
        HW_VAL(1,An)
        ;
        HNEWVAR(2,Ai)
        ;
        HNEXT(3);
        NEXT(3)
        ;

      case PUSH_VAL_VAL:
        PUSH_TO_REG()
        ;
      case CONSTANT_VAL_VAL:
        HNEWFUN(P);
        GETFIELDS(2);
        HW_VAL(1,An)
        ;
        HW_VAL(2,Ai)
        ;
        HNEXT(3);
        NEXT(3)
        ;

      case PUSH_VAR_VAL:
        PUSH_TO_REG()
        ;
      case CONSTANT_VAR_VAL:
        HNEWFUN(P);
        GETFIELDS(2);
        HNEWVAR(1,An)
        ;
        HW_VAL(2,Ai)
        ;
        HNEXT(3);
        NEXT(3)
        ;
#endif

#if JUMP_COMPRESS
      case EXEC_TRY:
        EXEC0()
        ;
        GETFIELDS(0);
#endif

      case TRY_ME_ELSE:
        xval = REGFIELD;
        IF_OVER("TRY_ME_ELSE",A,ChoiceStk,RECOVER())
        ;
        MAKE_CHOICE_POINT(GETLABEL(P),xval)
        ;
        NEXT(2)
        ;

      case RETRY_ME_ELSE:
        xval = REGFIELD;
        SAVED_P = GETLABEL(P); /* also: SAVED_P=GETLABEL(SAVED_P); */
        RESTORE_STATE(xval)
        ;
        A += xval + 3;
        NEXT(2)
        ;

      case TRUST_ME_ELSE:
        xval = REGFIELD;
        RESTORE_STATE(xval)
        ;
        NEXT(2)
        ;

      case TRY_ME_ONLY: /* nop */
#if 1
        ABORT("unexpected TRY_ME_ONLY");
#else
        NEXT(2);

#endif
      case NONDET:
#if 1
        ABORT("unexpected NONDET");
#else
        NEXT(2);
#endif

      case UNIFY_VOID:
        if (S) {
          xval = On;
          SNEXT(xval);
          NEXT(1)
          ;
        }
      case WRITE_VOID:
        xval = On;
        xref = H;
        HNEXT(xval);
        while (xval--)
          xref[xval] = T2C(xref+xval);
        NEXT(1)
        ;

        /* INLINE ARGUMENT LOADERS */

      case LOAD_CONSTANT:
        xval = REGFIELD;
        X((bp_long)xval) = GETFUN(P);
        NEXT(2)
        ;

#if STRUCT_COMPRESS
      case LOAD_VAL_SHORT:
        xval = REGFIELD;
        X(2) = xval;
        IN_VALUE(1,Ai)
        ;
        NEXT(1)
        ;

      case LOAD_VALUEx2:
        IN_VALUE(1,Ai)
        ;
        IN_VALUE(2,An)
        ;
        NEXT(1)
        ;
#endif

      case LOAD_VALUE:
        ires = REGFIELD;
        IN_VALUE(ires,Ai)
        ;
        NEXT(1)
        ;

        /* needed to load floats X->$float(i1,I2,I3) */
      case LOAD_VARIABLE:
        ires = REGFIELD;
        NEWVAR2(X(ires),Ai)
        ;
        NEXT(1)
        ;

        /* OLD SPECIAL BUILTINS */

      case TRUE_0:
        FDEREF(regs[1])
        ;
        P = GETPRED(xval);
        fields = GETARITY(xval);
        if (!P) {
          P = g.metatrue;
          continue;
          /*
           WARFUN(PUTARITY(xval,fields-1),
           "undefined predicate as continuation")
           */
        }
        COPY_REGS()
        CUT_AND_CHECK_HEAP()
        ;
        continue;

      case CALL_1:
        FDEREF(regs[1])
        ;
        fields = GETARITY(xval);
        P = GETPRED(PUTARITY(xval,++fields));
        if (!P) {
          P = g.metacall;
          continue;
          /* WARFUN(xval,"metacall to unknown predicate")*/
        }
        SETCELL(regs+fields--,regs[2]);
        COPY_REGS()
        CUT_AND_CHECK_HEAP()
        ;
        continue;

      case APPLY:
        P = GETPRED((xval=GETFUN(P)));
        if (!P) /* setting up a metacall to bp_long. code */
        {
          H = apply(H, regs, xval, wam);
          P = g.metacall;
        }
        CUT_AND_CHECK_HEAP()
        ;
        continue;

#define BFAIL() FAILURE()
#define BEXIT(Mes) ABORT(Mes)
#define BCONTINUE() continue

        /* INLINE PREDICATES */

      case FAIL_0:
        BFAIL()

      case CWRITE_1:
        /* don't DEREF !!! ; regs may also be An */
        fout(TEMP(0), wam, g.tellfile);
        BNEXT(1)
        ;

      case CNL_0:
        NEWLN()
        ;
        BNEXT(1)
        ;

      case VAR_1:
        FDEREF(TEMP(0))
        ;
        if (NONVAR(xval))
        BFAIL()
        BNEXT(1)
        ;

      case NONVAR_1:
        FDEREF(TEMP(0))
        ;
        if (VAR(xval))
        BFAIL()
        BNEXT(1)
        ;

      case INTEGER_1:
        FDEREF(TEMP(0))
        ;
        if (INTEGER(xval)) {
          BNEXT(1)
          ;
        }
          else
          BFAIL()

      case ATOMIC_1:
        FDEREF(TEMP(0))
        ;
        if (ATOMIC(xval)) {
          BNEXT(1)
          ;
        }
          else
          BFAIL()

      case IS_COMPILED_1:
        FDEREF(TEMP(0))
        ;
        xval = PUTARITY(xval,1+GETARITY(xval));
        if (GETPRED(xval)) {
          BNEXT(1)
          ;
        }
          else
          BFAIL()

        /* INLINE ARITH */

      case PLUS_3:
        COMPUTE(+)
        ;
      case SUB_3:
        COMPUTE(-)
        ;
      case MUL_3:
        COMPUTE(*)
        ;
      case MOD_3:
        COMPUTE_MOD(%)
        ;

      case DIV_3:
        COMPUTE_DIV(/)
        ;

      case FDIV_3:
        FLOAT_COMP(X(1),X(2))
        ;

      case RANDOM_1:
        OUT_RESULT(INPUT_INT(((bp_long)((no)rand()>>TAGBITS))))
        ;

      case GET0_1:
        if (feof(g.seefile)) {
          clearerr(g.seefile);
          g.lineno = 0;
          /* warnmes("Read past EOF"); */ires = 0;
        }
        else {
          ires = (bp_long) getc(g.seefile);
#ifdef VIVO
          if (QLEVEL() == 1111 && g.seefile != STD_in && ires > 0) ires
              = unobfuscate(ires);
#endif
          if ('\n' == ires && g.seefile != STD_in) g.lineno++;
        }
        OUT_RESULT(INPUT_INT(ires))
        ;

      case PUT0_1:
        xval = X(1);
        if (!INTEGER(xval))
        BWARFUN(xval,"integer expected in put/1")
        ;
        putc(OUTPUT_INT(xval), g.tellfile);
        BNEXT(1)
        ;

      case LESS_2:
        MUST_BE(<)
        ;
      case GREATER_2:
        MUST_BE(>)
        ;
      case LESS_EQ_2:
        MUST_BE(<=)
        ;
      case GREATER_EQ_2:
        MUST_BE(>=)
        ;
      case ARITH_EQ_2:
        MUST_BE(==)
        ;
      case ARITH_DIF_2:
        MUST_BE(!=)
        ;

      case LSHIFT_3:
      INT_ONLY(<<)
        ;
      case RSHIFT_3:
      INT_ONLY(>>)
        ;
      case L_AND_3:
      INT_ONLY(&)
        ;
      case L_OR_3:
      INT_ONLY(|)
        ;
      case L_XOR_3:
      INT_ONLY(^)
        ;

      case L_NEG_3:
        IF_NOT_INTEGER_OP()
        BFAIL()
        xval = INPUT_INT(~OUTPUT_INT(X(2)));
        INT_COMP(|,X(1),xval)
        ;

      case COMPARE0_3:
        ires = compare(&X(1), &X(2));
        xval = g.compare_vals[ires + 1];
        OUT_RESULT(xval)
        ;

      case ARG_3:
        xval = X(1);
        if (!INTEGER(xval))
        BWARFUN(xval,"arg/3's 1st arg must be integer")
        ;
        ires = OUTPUT_INT(xval);

        xref = C2T(X(2));
        if (ATOMIC(T2C(xref)))
        BFAIL()
        ASSERT2(VAR(T2C(xref)),xref);
        xval = GETREF(xref);
        if (VAR(xval))
        BWARFUN(xval,"arg/3's 2nd arg must be nonvar")
        ;
        if (ires <= 0 || (no) ires > GETARITY(xval))
        BWARFUN(xval,"arg/3's 1st arg must be in 1..arity")
        ;
        xref += ires;
        OUT_RESULT(xref)
        ;

      case SETARG_3:
        BUG("setarg/3 called");
        if (!setarg(regs, wam, A))
        BFAIL()
        BNEXT(1)
        ;

        /* BEXIT("%%unimplemented: setarg/3"); */

      case CHANGE_ARG_3:
        if (!change_arg(regs, wam, A))
        BFAIL()
        BNEXT(1)
        ;

      case DEF_3:
        if (!def(regs, wam, BBOARDTIME))
        BFAIL()
        BNEXT(1)
        ;

      case RM_2:
        X(3) = g.empty;
      case SET_3:
        if (!set(regs, wam, BBOARDTIME))
        BFAIL()
        BNEXT(1)
        ;

      case VAL_3:
        ATOMIZE(X(1));
        ATOMIZE(X(2));
        xval = hget(X(1), X(2));
        if ((0 == xval) || (g.empty == xval))
        BFAIL()
        /*fprintf(STD_err,"VAL_3: %ld$$$\n\n",xval);*/
        SAFE_OUT(xval)
        ;

      case LVAL_3: /* now checks with SAFE_HASH_OP */
        BUG("lval called");
        if (!(xval = lval(regs, wam, VARTIME)))
        BFAIL()
        SAFE_OUT(xval)
        ;

      case SYMCAT_3:
        xval = symcat(regs, wam);
        if (!xval)
        BFAIL()
        OUT_RESULT(xval)
        ;

        /* C-ification of builtins ends here. To be continued... */

      case NAMECAT_4:
        xval = namecat(regs, wam);
        if (!xval)
        BFAIL()
        OUT_RESULT(xval)
        ;

      case DEEP_HASH_4: {
        xval = X(2);
        ATOMIZE(xval);
        if (!INTEGER(xval))
        BWARFUN(xval,"deep_hash's 2nd arg (rec. level) must be integer");
        ires = OUTPUT_INT(xval);

        xval = X(3);
        ATOMIZE(xval);
        if (!INTEGER(xval))
        BWARFUN(xval,"deep_hash's 3nd arg (mod) must be integer");
        ires = deep_hash(X(1), ires, OUTPUT_INT(xval)); /* do not atomize X(1)! */
        /* key, max recursion, mod */
      }
        if (-2 == ires)
        BWARFUN(xval,"large or cyclic 1st arg in deep_hash: recursion depth error")
        ;
        if (ires < 0)
        BFAIL()
        xval = INPUT_INT(ires);
        OUT_RESULT(xval)
        ;

      case GVAL_2:
        xref = H;
        H = tval(H, X(1), 0); /* var or fun->name/0 */
        OUT_RESULT(xref)
        ;

      case HVAL_2:
        xref = H;
        H = tval(H, X(1), 1); /* fun->symno/arity */
        OUT_RESULT(xref)
        ;

      case TVAL_3:
        ATOMIZE(X(1));
        ATOMIZE(X(2));
        xval = hget(X(1), X(2));
        if (!xval)
        BFAIL()
        H = tval((xref = H), xval, 0); /* OK! 0=> F/N *???*/
        OUT_RESULT(xref)
        ;

      case TLET_3:
        if (!tlet(regs))
        BFAIL()
        BNEXT(1)
        ;

      case GET_ASSERTED_2:
        if (!(xval = get_asserted(regs, wam)))
        BFAIL()
        SAFE_OUT(xval)
        ;

      case ARRAY_SET_3:
        if (!(xref = array_ref(X(1), X(2), wam)))
        BFAIL()
        xval = untval(C2T(X(3)));
        SETREF(xref,xval);
        BNEXT(1)
        ;

      case ARRAY_GET0_3:
        if (!(xref = array_ref(X(1), X(2), wam)))
        BFAIL()
        xval = GETREF(xref);
        H = tval((xref = H), xval, 0);
        OUT_RESULT(xref)
        ;

      case ARRAY_GET_3:
        if (!(xref = array_ref(X(1), X(2), wam)))
        BFAIL()
        xval = GETREF(xref);
        DEREF1(xval)
        ;
        H = tval((xref = H), xval, 0);
        OUT_RESULT(xref)
        ;

      case MAKE_ARRAY_2:
        xval = X(1);
        if (!INTEGER(xval))
        BFAIL()
        if (!(xref = make_var_array(xval)))
        BFAIL()
        xval = PTR2INT(xref);
        OUT_RESULT(xval)
        ;

      case DESTROY_ARRAY_1:
        xval = X(1);
        if (!INTEGER(xval))
        BFAIL()
        xref = (term) INT2PTR(xval);
        XFREE(xref);
        BNEXT(1)
        ;

      case VGET_INT0_2:
        if (!(xref = array_ref(X(1), INPUT_INT(0), wam)))
        BFAIL()
        xval = INPUT_INT(xref[0]);
        OUT_RESULT(xval)
        ;

      case VSET_INT0_2:
        xval = X(2);
        if (!INTEGER(xval))
        BFAIL()
        if (!(xref = array_ref(X(1), INPUT_INT(0), wam)))
        BFAIL()
        xref[0] = OUTPUT_INT(xval);
        BNEXT(1)
        ;

      case ADDQ0_2:
        if (!addq0(wam, RX(1), X(2)))
        BFAIL()
        BNEXT(1)
        ;

      case PUSHQ0_2:
        if (!pushq0(wam, RX(1), X(2)))
        BFAIL()
        BNEXT(1)
        ;

      case POPQ0_2:
        wam[HeapStk].top = (term*) H;
        xref = popq0(HeapStk, wam, RX(1));
        if (!xref)
        BFAIL()
        H = (term) wam[HeapStk].top;
        SAFE_OUT(xref)
        ;

      case DCG_CONNECT_1:
        if (!(H = dcg_connect(H, regs, wam, A)))
        BFAIL()
        OUT_RESULT(X(0))
        ;

      case LIST2TERM_2:
        xref = H;
        if (!(H = list2term(H, regs, wam)))
        BFAIL()
        OUT_RESULT(xref)
        ;

      case TERM2LIST_4:
        xref = H;
        if (!(H = term2list(H, regs, wam)))
        BFAIL()
        OUT_RESULT(xref)
        ;
#if 0
        case SELF_INFO_1:
        BEXIT("unexpected SELF_INFO");
        xval=INPUT_INT(LEFTFIELD);
        /* return the arity of the (lexically speaking)
         `current predicate' */
        OUT_RESULT(xval);
#endif
      case CALL_EXTERNAL_3:
        xref = H;
        H = call_external(H, regs, wam);
        if (NULL == H) {
          /*BEXIT("failed external C function call");*/
          BFAIL()
        }
        OUT_RESULT(xref)
        ;

      case ADD_INSTR_4:
        if (!add_instr(regs, wam))
        BFAIL()
        BNEXT(1)
        ;

      case OLDER_FILE_2:
        if (!older_file(regs, wam))
        BFAIL()
        BNEXT(1)
        ;

      case SEEING_TELLING_2:
        xval = (!OUTPUT_INT(X(1))) ? g.seefunc : g.tellfunc;
        OUT_RESULT(xval)
        ;

      case SEE_TELL_2:
        if (!see_tell(regs, wam))
        BFAIL()
        BNEXT(1)
        ;

      case SEEN_TOLD_1:
        seen_told((byte) OUTPUT_INT(X(1)));
        /*used also in restart*/
        BNEXT(1)
        ;

      case SEE_TELL_AT_2:
        if (!see_tell_at(regs, wam))
        BFAIL()
        BNEXT(1)
        ;

      case SEEING_TELLING_AT_2:
        xval = seeing_telling_at(regs);
        OUT_RESULT(xval)
        ;

      case STRING_OP_3:
        ires = OUTPUT_INT(X(1));
        if (0 == ires) {
#ifdef OLD_SREAD
          xval=X(2);
          if(!SYMCONST(xval)) BFAIL()
          xval=sread(H,xval);
          if(!xval) BFAIL()
          H=(term)(xval)+1;
#else
          BEXIT("old sread not supported anymore");
#endif
        }
        else if (1 == ires) { /*plain*/
          xval = sout(X(2), wam);
        }
        else if (2 == ires) {
          /*warnmes("entering STRING_OP(2,..");*/
          if (g.query == g.empty)
          BFAIL()
          { /*string to chars built on the heap*/
            /*string s=INT2PTR(X(2));*/
            string s = INT2PTR(g.query);
            xref = H;
            H = string2list(xref, s, wam);
            if (!H) BEXIT("fatal error in STRING_OP(2,...)");
            xval = T2C(xref);
          }
        }
        else if (3 == ires) { /*quoted for read back, returns list of chars*/
          string s = qsout(X(2), wam); /* s==g.sbuf */
          xref = H;
          H = string2list(xref, s, wam);
          if (!H) BEXIT("fatal error in STRING_OP(3,...)");
          xval = T2C(xref);
        }
        else if (4 == ires) { /*puts a list of chars directly into a C string, without internalizing it*/
          /* string s=INT2PTR(X(2)); ignore for now */
          /*warnmes("entering STRING_OP(4,..");*/
          FDEREF(X(2));
          if (g.NIL == xval) g.answer = g.empty;
          else {
            if (!list2buf(xref, xval, sbuf, MAXSBUF)) BEXIT("fatal error in STRING_OP(4,..)");
            /*g.answer=INPUT_STRING(g.sbuf);*/
            g.answer = PTR2INT(sbuf);
            xval = INPUT_INT(0);
          }
        }
else        BEXIT("bad string op");
        OUT_RESULT(xval);

        case OP0_3:
        if(!op0(regs,wam)) BFAIL()
        BNEXT(1);

        case DET_APPEND0_3:
        xref=H;
        if(!(H=det_append0(H,regs,wam))) BFAIL()
        OUT_RESULT(xref);

        case COPY_TERM_3:
        xval=X(2); /* the term to be copied */
        if(!ATOMIC(xval)) {
          if(!OUTPUT_INT(X(1))) {
            COPY_TERM(
                H, /* from */
                wam[HeapStk].margin, /* to */
                BEXIT("heap_overflow in copy_term/2"));
          }
          else {
            COPY_TERM(
                g.shared[BBoardStk].top, /* from */
                g.shared[BBoardStk].margin, /* to */
                BFAIL() );
          }
        }
        /* else, if atomic, do nothing - return original X(2) */
        SAFE_OUT(xval);

        case UNIFY_TO_2:
        xval=X(1);
        SAFE_OUT(xval);

        case TERM_APPEND_3:
        xref=H;
        if(!(H=term_append(H,regs,wam))) BFAIL()
        OUT_RESULT(xref);

        case STRIP_CONT0_2:
        xref=H;
        if(!(H=strip_cont0(H,regs,wam))) BFAIL()
        OUT_RESULT(xref);

        case DCG_VAL_1:
        DCG_TRACE();
        xref=DCGSTART();
        xval=GETCELL(GETREF(xref));
        OUT_RESULT(xval);

        case DCG_DEF_1:
        DCG_TRACE();
        xref=DCGSTART();
        xref=C2T(GETREF(xref));
        ASSERT2(VAR(xref),"VAR expected in dcg_def/1");
#if defined(NO_VALUE_TRAIL)
        TRAIL_IF(xref);
#else
        xval=GETCELL(xref);
        SMART_VTRAIL_IF(xref,xval);
#endif
        SETREF(xref,X(1));
        BNEXT(1);

        case DCG_TELL_1:
        DCG_TRACE();
        if(!dcg_tell(regs,wam,A)) BFAIL()
        BNEXT(1);

        case DCG_TELLING_1:
        DCG_TRACE();
        { term g_connect=DCGSTART();
          xref=C2T(GETREF(g_connect));
          xval=INPUT_INT(xref-(term)g_connect);
        }
        OUT_RESULT(xval);

        case FLOAT_FUN_3:
        xref=H;
        if(!(H=float_fun(H, regs )))
        BFAIL()
        OUT_RESULT(xref);

        case FLOAT_FUN2_4:
        xref=H;
        if(!(H=float_fun2(H,regs))) BFAIL()
        OUT_RESULT(xref);

        case INPUT_FLOAT_4:
        xref=H;
        if(!(H=input_float(H,regs,wam))) BFAIL()
        OUT_RESULT(xref);

        case BB_LIST0_3:
        xref=H;
        H=hlist(H,regs,wam);
        if(!H) BEXIT("error in bb_list/2");
        OUT_RESULT(xref);

        case UNIX_ARGC_1:
        xval=INPUT_INT(g.argc);
        OUT_RESULT(xval);

        case UNIX_ARGV_2:
        if(!(xval=unix_argv(regs,wam))) BFAIL()
        OUT_RESULT(xval);

        case UNIX_GETENV_2:
        if(!(xval=unix_getenv(regs,wam))) BFAIL()
        OUT_RESULT(xval);

        case UNIX_ACCESS_2:
        if(!unix_access(regs,wam)) BFAIL()
        BNEXT(1);

        case UNIX_CD_1:
        if(!unix_cd(regs,wam)) BFAIL()
        BNEXT(1);

        case UNIX_FORK_1:
        ires=unix_fork();
        if(ires<0)
        BEXIT("error in unix_fork/1");
        xval=INPUT_INT(ires);
        OUT_RESULT(xval);

        case UNIX_PID_1:
        ires=unix_pid();
        if(ires<0)
        BEXIT("error in unix_pid/1");
        xval=INPUT_INT(ires);
        OUT_RESULT(xval);

        case UNIX_KILL_2:
        if(!unix_kill(regs,wam)) BFAIL()
        BNEXT(1);

        case CREATE_ENGINE_4:
        xval=(cell)create_engine(wam,
            OUTPUT_INT(X(1)),
            OUTPUT_INT(X(2)),
            OUTPUT_INT(X(3))
        );
#if TRACE>1
        fprintf(STD_err,"CREATE_ENGINE: %ld\n",xval);
#endif
        if(!xval) BFAIL()

        xval=PTR2INT(xval);
        OUT_RESULT(xval);

        case DESTROY_ENGINE_1:
#if TRACE>1
        fprintf(STD_err,"DESTROY_ENGINE: %ld\n",INT2PTR(X(1)));
#endif
        xval=X(1);
        if(xval==PTR2INT(wam) ||
            !destroy_engine(INT2PTR(xval))) /* do not cast! */
        BFAIL()
        BNEXT(1);

        case LOAD_ENGINE_3:
        if(!load_engine0(regs)) BFAIL()
        BNEXT(1);

        case ASK_ENGINE_2:
        xval=X(1);
        if(!INTEGER(xval))
        BEXIT("bad engine in ask_engine/2");

        xref=INT2PTR(xval);
        xref=ask_engine((stack)xref);

        if(xref)
        {
          wam[HeapStk].top=(term*)H;
          xref=copy_to_engine(wam,xref);
          if(xref) H=(term)wam[HeapStk].top;
        }

        if(!xref) BFAIL()
        OUT_RESULT(xref);

#if THREADS>0
        case ASK_THREAD_2:

        xval=X(1);
        if(!INTEGER(xval))
        BEXIT("bad engine in ask_thread/2");

        xref=INT2PTR(xval);
        ires=tcreate(ask_thread,xref);
        set_engine_prop(X(1),ENGINE_TYPE,INPUT_INT(RUNNING_ENGINE));
        xval=INPUT_INT(ires);
        set_engine_prop(X(1),ENGINE_THREAD,xval);

        OUT_RESULT(xval);

        case THREAD_EXIT_1: /* arg ignored */
        thread_exit(wam);
        BNEXT(1);

        case THREAD_JOIN_1:
        xval=X(1);
        if( !INTEGER(xval) )
        BEXIT("integer arg expected in join_thread/1");
        thread_join( OUTPUT_INT(xval) );
        BNEXT(1);

        case CURRENT_THREAD_1:
        ires=current_thread(wam);
        xval=INPUT_INT(ires);
        OUT_RESULT(xval);

        case TSYNC_OP_3:
        ires=0;
        if(INTEGER(X(1)) && INTEGER(X(2)) && INTEGER(X(3)) ) {
          ires=tsync_op(
              OUTPUT_INT(X(1)), OUTPUT_INT(X(2)),
              OUTPUT_INT(X(3))
          );
          if(0==ires) BEXIT("error in tsync_op/3");
          if(ires>1) BFAIL();
        }
        else {
          fprintf(STD_err, "ARGS: %ld %ld %ld\n",
              OUTPUT_INT(X(1)), OUTPUT_INT(X(2)),
              OUTPUT_INT(X(3))
          );
          BEXIT("bad input in tsync_op/3");
        }
        BNEXT(1);
#else
        case ASK_THREAD_2:
        case THREAD_EXIT_1:
        case THREAD_JOIN_1:
        case CURRENT_THREAD_1:
        case TSYNC_OP_3:
        fprintf(STD_err,"unimplemented thread operation: %ld\n",GETOPCODE());
        /*BEXIT("error in thread operation: no threads implemented");*/
        BFAIL();
#endif
        case LIST_ENGINES_1:
        xref=H;
        H=list_engines(H);
        if(!H) BEXIT("error in list_engines/1");
        OUT_RESULT(xref);

        case CURRENT_ENGINE_ADDR_1:
        xval=PTR2INT(wam);
        OUT_RESULT(xval);

        case GET_ENGINE_PROP_3:

        if(!INTEGER(X(1)) || !INTEGER(X(2))) {
          BEXIT("bad args in get_engine_prop(No,Attr,Val)");
          /* xval=INPUT_INT(0);*/
        }
        else {
          xval=get_engine_prop(X(1),OUTPUT_INT(X(2)));
        }
        OUT_RESULT(xval);

        case UNTRAIL_TO_1:
        xval=X(1);
        if(!INTEGER(xval))
        BEXIT("arg 1 of untrail_to/1 should be integer");
        xval=INT2CUT(xval);
        A=(term*)xval;
        TR_TOP=unwind_trail(TR_TOP,(term*)SAVED_TR);
        BNEXT(1);

        case GET_NECK_CUT_1:
        xval=CUT2INT(cutB);
        OUT_RESULT(xval);

        case OVERRIDE_3:
#if HAS_OVERRIDE
        BUG("override/2 clled");
        if(!override(regs,wam))
        BFAIL();
        BNEXT(1);
#else
        BEXIT("OVERRIDE_2 not implemented");
#endif

        case RANDOM_SEED_1:
        xval=X(1);
        if(!INTEGER(xval)) BFAIL()
        init_random(OUTPUT_INT(xval));
        BNEXT(1);

        case MEMBER_SCAN_3:
        if(!(xref=member_scan(X(1),X(2),wam,A))) BFAIL()
        SAFE_OUT(xref);

        case CMEMBER_SCAN_3:
        { no ok=TRUE;
          xref=H;
          if(!(H=cmember_scan(H,X(1),X(2),wam,A,&ok)))
          { if(ok) BFAIL()
            BEXIT("bad data or heap_overflow in cmember_scan/3");
          }
        }
        OUT_RESULT(xref);

        case CDEL_SCAN_3:
        { no ok=TRUE;
          xref=H;
          if(!(H=cdel_scan(H,X(1),X(2),wam,A,&ok)))
          { if(ok) BFAIL()
            BEXIT("bad data or heap_overflow in cdel_scan/3");
          }
        }
        OUT_RESULT(xref);

        case PUSH_CODE_1:
        xval=PTR2INT(ctop);
        push_code(wam);
        OUT_RESULT(xval);

        case OPEN_STREAM_4:
        xref=H;
        if(!(H=open_stream(H,regs,wam))) BFAIL()
        OUT_RESULT(xref);

        case CLOSE_STREAM_2:
        if(!close_stream(regs,wam)) BFAIL()
        BNEXT(1);

        case FGETC_2:
        xval=X(1);
        if(!INTEGER(xval))
        BWARFUN(xval,"bad first arg in fgetc/2")
        xref=INT2PTR(xval);
        ires=(bp_long)getc((FILE *)xref);
        OUT_RESULT(INPUT_INT(ires));

        case FPUTC_2:
        if(!INTEGER(X(1)))
        BWARFUN(X(1),"bad first arg in fputc/2")
        xval=X(2);
        if(!(INTEGER(xval) &&
                0<=(ires=OUTPUT_INT(xval)) && ires<256))
        BWARFUN(xval,"byte in 0..255 expected in fputc/1");
        xref=INT2PTR(X(1));
        if(ires!=putc(ires,(FILE*)xref))
        BWARFUN(xval,"file error in fputc/1");
        BNEXT(1);

        case FFLUSH_1:
        xval=X(1);
        if(!INTEGER(xval))
        BWARFUN(xval,"bad first arg in fflush/1")
        xref=INT2PTR(xval);
        fflush((FILE *)xref);
        BNEXT(1);

        case FSIZE_2:
        xval=X(1);
        if(!INTEGER(xval))
        BWARFUN(xval,"bad first arg in fsize/1")
        xref=INT2PTR(xval);
        ires=(bp_long)fsize((FILE *)xref);
        OUT_RESULT(INPUT_INT(ires));

        case SYSTEM0_2:
        xval=system0(regs,wam);
        OUT_RESULT(xval);

        case NEW_NAME_2:
        if(!(xval=new_name(X(1),wam))) BFAIL()
        OUT_RESULT(xval);

        /* socket operations for client/server crafting */

        case NEW_CLIENT_3: {
          string host;
          if(!SYMCONST(X(1)))
          BWARFUN(X(1),"bad host name in arg 1 of new_server/2")
          host=NAME(X(1));
          if(!INTEGER(X(2)))
          BWARFUN(X(2),"bad second arg in new_client/3")
          ires=OUTPUT_INT(X(2));
          if((ires=new_client(host,ires))<=0) BFAIL()
          xval=INPUT_INT(ires);
        }
        OUT_RESULT(xval);

        case NEW_SERVER_2:
        if(!INTEGER(X(1)))
        BWARFUN(X(1),"bad first arg in new_server/2")
        ires=OUTPUT_INT(X(1));
        if((ires=new_server(ires))<=0) BFAIL()
        xval=INPUT_INT(ires);
        OUT_RESULT(xval);

        case NEW_SERVICE_3: {
          bp_long timeout;
          if(!INTEGER(X(1)))
          BWARFUN(X(1),"bad first arg in new_service/3")
          ires=OUTPUT_INT(X(1));
          if(!INTEGER(X(2)))
          BWARFUN(X(2),"bad second arg in new_service/3")
          timeout=OUTPUT_INT(X(2));
          if((ires=new_service(ires,timeout))<=0) BFAIL()
          xval=INPUT_INT(ires);
        }
        OUT_RESULT(xval);

        case PEER_ADDR_2:
        xval=X(1);
        if(!INTEGER(xval))
        BWARFUN(xval,"bad first arg in peer_addr/2")
        ires=OUTPUT_INT(xval);
        { string s;
          if(!(s=peer_addr(ires))) BFAIL()
          xval=INPUT_STRING(s);
        }
        OUT_RESULT(xval);

        case PEER_PORT_2:
        xval=X(1);
        if(!INTEGER(xval))
        BWARFUN(xval,"bad first arg in peer_port/2")
        ires=OUTPUT_INT(xval);
        if((ires=peer_port(ires))<0) BFAIL()
        xval=INPUT_INT(ires);
        OUT_RESULT(xval);

        case CLOSE_SOCKET_1:
        xval=X(1);
        if(!INTEGER(xval))
        BWARFUN(xval,"bad first arg in close_socket/1")
        ires=OUTPUT_INT(xval);
        close_socket(ires);
        BNEXT(1);

        case SOCK_READ_3: case SOCK_READLN_3:
        xval=X(1);
        if(!INTEGER(xval))
        BWARFUN(xval,"bad first arg in sock_read(ln)/2")
        ires=OUTPUT_INT(xval);

        xval=X(2);
        if(!INTEGER(xval))
        BWARFUN(xval,"bad first arg in sock_read(ln)/2")
        { bp_long max=OUTPUT_INT(xval);
          bp_long count=0;
          /* warnmes("blocking sock_read");*/
          if(SOCK_READ_3==GETOPCODE()) {
            if((count=sock_read(ires,sbuf,max))<=0) BFAIL();
          }
          else { /* SOCK_READLN_3 */
            if((count=sock_readln(ires,sbuf,max))<=0) BFAIL();
          }
          /* warnmes("unblocked sock_read"); */
          xref=H;
          if(!(H=string2list_with_length(H,sbuf,wam,count)))
          if(!H) BEXIT("fatal error string2list in SOCK_READ(...)");
          /* was: BFAIL() */
        }
        OUT_RESULT(xref);

        case SOCK_WRITE_3: case SOCK_WRITELN_3:
        if(!INTEGER(X(3)))
        BWARFUN(X(3),"integer 3-rd arg expected in sock_write(ln)/2")

        xval=X(1);
        if(!INTEGER(xval))
        BWARFUN(xval,"integer first arg expected in sock_write(ln)/2")
        ires=OUTPUT_INT(xval);

        xref=RX(2);
        if((cell)xref==g.NIL)
        xval=g.NIL;
        else if(NONVAR(xref)) /* a ptr to a list of chars */
        BWARFUN(X(2),"unexpected arg 2 in sock_write(ln)/2")
        else
        xval=GETCELL(xref);

        {
          if(!list2buf(xref,xval,sbuf,MAXSBUF))
          BWARFUN(X(2),"bad non-list arg 2 in sock_write(ln)/2");

          if(SOCK_WRITE_3==GETOPCODE()) {

            if((sock_write(ires,sbuf,OUTPUT_INT(X(3))))<=0) BFAIL();
          }
          else {
            if((sock_writeln(ires,sbuf,OUTPUT_INT(X(3))))<=0) BFAIL();
          }
        }
        BNEXT(1);

        case FILE2SOCK_2:
        ATOMIZE(X(1)); ATOMIZE(X(2));
        if(!INTEGER(X(1)))
        BWARFUN(X(1),"bad first arg in file2sock/2")
        if(!INTEGER(X(2)))
        BWARFUN(X(2),"bad first arg in file2sock/2")
        xref=INT2PTR(X(1));
        ires=OUTPUT_INT(X(2));
        /*fprintf(STD_err,"file2sock: ires=%d\n",ires);*/
        {
          if(file2sock((FILE *)xref,ires,sbuf)<=0) BFAIL()
        }
        BNEXT(1);

        case SOCK2FILE_2:
        ATOMIZE(X(1)); ATOMIZE(X(2));
        if(!INTEGER(X(1)))
        BWARFUN(X(1),"bad first arg in sock2file/2")
        if(!INTEGER(X(2)))
        BWARFUN(X(2),"bad arg 2 in sock2file/2")
        ires=OUTPUT_INT(X(1));
        xref=INT2PTR(X(2));
        /*fprintf(STD_err,"sock2file: ires=%d\n",ires);*/
        {
          if(sock2file(ires,(FILE *)xref,sbuf)<=0) BFAIL()
        }
        BNEXT(1);

        case SLEEP_1:
        xval=X(1);
        if(!INTEGER(xval))
        BWARFUN(xval,"bad first arg in sleep/1")
        ires=OUTPUT_INT(xval);
        unix_sleep(ires);
        BNEXT(1);

        case HOST2IP_2:
        xval=X(1);
        if(!SYMCONST(xval))
        BWARFUN(xval,"bad first arg in host2ip/2")
        { string s=NAME(xval);
          s=host2ip(s);
          xval=INPUT_STRING(s);
        }
        OUT_RESULT(xval);

        case QPRINT_1:
        xval=X(1);
        /* don't DEREF !!! ; regs may also be An */
        qprint(xval,wam,g.tellfile);
        BNEXT(1);

        case TERM_STORE_OP_4:
        if(!(H=term_store_op(H,regs,wam))) BFAIL()
        xval=X(4);
        OUT_RESULT(xval);

        case NEW_BUILTIN_3:
        if(!(H=new_builtin(H,regs,A,P,wam))) BFAIL()
        xval=X(3);
        OUT_RESULT(xval);

        case HALT_1:
        SAVE_HAP();
        return heap_or_bp_halt(H,regs,wam);
        break;

        case RESTART0_0:
        restart_orig(wam);
        NEXT(1);

        case ABORT0_0:
        SAVE_HAP();
        SET_ENGINE_ERROR(ENGINE_ABORT);
        return NULL;

        case BEGIN_C_CHUNK:

        C_CHUNK_TRACE("BEGIN_C_CHUNK");

        xval=GETFUN(P);

        H = FUNCALL(xval,(H,regs,P+2,cutB,A,wam));
#if TRACE>0 || PROF
        if(g.inC)
        {
          if(wam[ChoiceStk].top>A
              || wam[ChoiceStk].top<wam[ChoiceStk].base)
          ABORT("unexpected A after C_CHUNK");
        }
        else
        ABORT("unexpected BEGIN_C_CHUNK");
#endif
        A=wam[ChoiceStk].top; /* SAVED_P depends on A */
        if(!H) FAILURE();
        P=(instr)wam[ChoiceStk].oldtop;
        continue;

        case END_C_CHUNK:
        C_CHUNK_TRACE("END_C_CHUNK");
        warnmes("END_C_CHUNK should never be executed");
        NEXT(1);

        case FUNCTOR_3:
        if(!(H=functor(H,regs+1,wam,A))) FAILURE()
        CONT(4);

        case NAME_2:
        if(!(H=name2list(H,regs+1,regs+2,wam,A))) FAILURE()
        CONT(3);

        case LOAD0_1: if(!load_1(regs,wam)) FAILURE();
        CONT(2);

        case STAT0_3:
        if(!stats0(H,regs,wam,A)) FAILURE()
        CONT(4);

        case LIST_ASM_3:
#if TRACE>0 || PROF
        if(!list_asm(regs[1],regs[2],regs[3],wam)) FAILURE()
#endif
        CONT(4);

        case BB_RESET_1:
        if(!bb_reset(regs,wam)) FAILURE()
        CONT(2);

        case GARBAGE_COLLECT_0:
#if GC>0
        if(!(H=trigger_gc(H, regs, A, P, wam)))
        {
          RECOVER();
        }
        CONT(1);
#else
        ABORT("garbage_collect/0 not implemented");
#endif

        case PROFILE_0:
#if TRACE>1 || PROF
        profiler(2,P,A,H,wam);
#endif
#if TRACE_EXEC>0
        show_ucount();
#endif
        CONT(1);

#ifdef BUILTIN_MEMBER
        case MEMBER_ENTRY_2:
        H=member_entry(H,regs,wam,A);
        if(!H) {A=wam[ChoiceStk].top; FAILURE()}
        NEXT(1);

        case FOR_ENTRY_3:
        H=for_entry(H,regs,wam,A);
        if(!H) {A=wam[ChoiceStk].top; FAILURE()}
        NEXT(1);

#else
        case MEMBER_ENTRY_2:
        ABORT("member_entry/2 builtin not implemented");
        case FOR_ENTRY_3:
        ABORT("for_entry/3 builtin not implemented");
#endif

        case RETURN0_1:
        xval=INPUT_INT(1);
        SET_ENGINE_TYPE(SUSPENDED_ENGINE);
        SET_ENGINE(ENGINE_RETURN,regs[1]);
        regs[1]=regs[2]; P++;
        SAVE_HAP();
        return H;

        case FCALL_3:
        ABORT("fcall/3 builtin not implemented");

        case IF0_3:
        if(!(H=if0(regs,H,P,A,wam))) FAILURE();
        P=g.call;
        continue;

        default: /* xval=fields; makes gcc reg allocator happy */
        bad_instr(P);

      } /* switch */
    } /* for    */
  } /* bp0     */

term interp_from(instr P, register stack wam) {

  register term regs;
  register term H;
  register term *A;

  START_INTERP();
  return bp(regs, H, P, A, wam); /* ret H to become exit code 0 */
}

bp_long interp(register stack wam, instr entry) {
  register term H;
  for (;;) {
    g.stopper = 0;
    SET_ENGINE_ERROR(ENGINE_OK);
    H = interp_from(entry, wam); /* ignoring returned H */
    /*fprintf(STD_err,"$$ <<ENGINE_ERROR: %d g.stopper=%d,H=%ld\n",
     GET_ENGINE_ERROR(),g.stopper,H);
     */
    switch (GET_ENGINE_ERROR()) {
      case ENGINE_ABORT:
        hbak(BBOARDTIME);
        if (0 == g.stopper) continue;
      break;

      case ENGINE_OK:
        g.stopper = 0;
      break;

      case ENGINE_FORCE_HALT:
        g.stopper = 0;
        bp_halt(0);
      break;

      default: {
        char mes[80];
        sprintf(mes,"%s: %ld",
            "unexpected engine error in interp(wam)",
            GET_ENGINE_ERROR()
        );
        warnmes(mes);
      }
    }
    if (0 == g.stopper) break;
  }
  /* fprintf(STD_err,"$$ >>EXITING interp: %d\n",GET_ENGINE_ERROR());*/
  return GET_ENGINE_ERROR();
}
