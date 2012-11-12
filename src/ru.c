#include <signal.h>
#include <string.h>
#include "defs.h"
#include "global.h"

extern cell new_func(string name, no argctr);
extern no load_kernel(string fname, register stack wam),
          hget(register no pred, register no fun);
extern void init_orig_wam(register stack wam);
extern char *c_interface,*get_host(),*get_ip_addr();

extern no hdef (register no pred,
                register no fun, register no val, byte stamp);
extern void hbak(byte stamp);
extern no make_symtable (void);
extern void init_instr_len (void);
extern bp_long hinit (void);
extern void restart_orig (register stack wam);
extern bp_long interp (register stack wam,instr entry);
extern int current_thread(register stack wam);
extern int thread_destroy(int tid);
extern string get_host(),get_ip_addr();

extern void init_mutexes();
extern void release_mutexes();

#if TRACE>0 || PROF
extern void *calloc_trace(no nb,no size),
            *malloc_trace(no nb,no size),
            free_trace(void *ptr);
#endif

#if TRACE>1 || PROF
extern void profiler (byte mes, instr P,
               term *A, term H, register stack wam);
#endif

#define W2Ks(No) (long)((No)>>(10L-TAGBITS))
#define K2Ws(No) (long)((No)<<(10L-TAGBITS))
#define ScaleToWords(Unit) Unit=K2Ws(Unit)

struct specsyms g;

struct limit max =
       {
#ifndef DWARF
          (ONE<<21)-64, /* HEAP */
          ONE<<18, /* CHOICE*/
          ONE<<18, /* TRAIL */
          ONE<<18, /* CODE  */
          ONE<<18, /* BBOARD */
          ONE<<18, /* DICT - hashtable */
          ONE<<18, /* ATOMS */
#else
          ONE<<15, /* HEAP  */
          ONE<<13, /* CHOICE*/
          ONE<<13, /* TRAIL */
          ONE<<17, /* CODE  */
          ZERO, /* BBOARD */
          ONE<<15, /* DICT - hashtable */
          ONE<<15, /* ATOMS */
#endif
          MAXSBUF, /* SBUF , IO buffer, changes with -i switch; default val. MAXBUF directly used in some place */
          1024*1024*20,   /* bytes MAX_CORE - only for Solaris - mbinpro */
          INPUT_INT(2),  /* QUIETNESS max.QUIET */
          INPUT_INT(1), /* LMETH - load method */
          INPUT_INT(50), /* DB_RATIO which triggers dynamic recomp. */
          MAX_ENGINE_PROP,   /* ENGINE_ATTR */
          INPUT_INT(0) /* SERVER PORT NUMBER, WORKING AS A SERVER IF > 0*/
       };

extern bp_long wam_bp_size;

#ifdef REUSE
extern struct bp_instr wam_bp[];
#endif

void quietmes(string format, string mes)
{
  if(QLEVEL()<5) fprintf(STD_err,format,mes);
}

/* quiet(1) will suppress Prolog-level messages, quiet(0) also C-level */
void debugmes(string format, string mes)
{
  if(QLEVEL()<1) fprintf(STD_err,format,mes);
}

string bp_version(void)
{
  static char mes[32];
  static char* name="BinProlog";
  sprintf(mes,"%s, #%d.%02d",name,VERSION/100, VERSION % 100);
  return mes;
}

string startup_mes(void)
{  string copyright,ftp,email,chat,c_ified,
          gc,th,x86,host,ip;
     static char mes[512];


copyright="Copyright (C) Paul Tarau 1992-2012.\n";
ftp=  "Open-sourced under GPL v.3 licence at\n";
email="http://www.gnu.org/licenses/gpl-3.0.txt.";
chat="\n";
     c_ified = (g.inC ? "(C-ified standalone)\n" : "") ;


#if GC==0
     gc="";
#else
     gc="(with heap GC enabled) ";
#endif

#if THREADS==0
     th="";
#else
#if VCC>0
	 th="(with Windows threads) ";
#else
	 th="(with POSIX threads) ";
#endif
#endif

	 x86 = (8 == sizeof(long)) ? "(64 bits)" : "32 bits";

     if(10==OUTPUT_INT(max.PORT)) {
       host=get_host();
	   ip=get_ip_addr();
	 }
	 else {
	   host="undetected";
	   ip="type bp -p10 to detect host";
	 };
     sprintf(mes,
           "\n%s %s%s%s%s%s%s%s%s%s\nDetected hostname: %s (%s)",
      bp_version(),
      copyright,
      ftp,
      email,
      chat,
      c_ified,gc,th,x86,
      c_interface,
      host,
      ip
     );
      return mes;
}


#include <time.h>

bp_long cputime()

{

  bp_long t=clock(),s=CLOCKS_PER_SEC;

  if (s<0) t=0;

  else if(s<1000) t=(t*100)/(s/10);

  else t=100*t/(s/10);

  return t;

}

#define BEGIN_RTIME ((no)((25*365+6)*(24*3600)))

#define GET_RTIME() ((no)(time(NULL)-BEGIN_RTIME))

bp_long realtime(int todo)
{
#if 0
  return cputime();
#else
  /* Solaris,HP,DEC.MIPS probably fine for all Unixes */

  static no startval; no rtime;

  if(0==todo) { /* use this to initialize time */
    startval=GET_RTIME();
    rtime=startval;
  }
  else if(1==todo) { /* gets last measured time */
    rtime=startval;
  }
  else if(2==todo) { /* use for time past since start */
    rtime=GET_RTIME()-startval;
  }
  else {
    rtime=GET_RTIME(); /* plain current time */
  }

  /*fprintf(STD_err,"realtime(%d)=%d\n",todo,rtime);*/
  return rtime;
#endif
}

/* ERROR HANDELRS */

void bp_exit_mes(string mes, bp_long i) {
  bp_long t=cputime()-g.stime; char buf[255];
  sprintf(buf,
 "\nProlog execution %s(%ld). CPU time = %ld.%lds\n",mes,i,t/1000,t%1000);
  quietmes("%s\n",buf);
}

void bp_exit(string mes, bp_long i) {
  release_mutexes();
  bp_exit_mes(mes,i);
  exit(i);
}

void bp_halt(int i) {
  bp_exit("halted",i);
}

void warnmes(string mes)
{
  fprintf(STD_err,"warning: *** %s ***\n",mes);
}

void fatal_error(string mes)
{
  fprintf(STD_err,"fatal error: *** %s ***\n",mes);
  bp_halt(1);
}

term local_error(cell xval, string Msg, register stack wam)
{
if(QLEVEL()<3) {
#if TRACE>1
  extern string smartref(cell x, register stack wam);
  fprintf(STD_err,"%s ??? ",smartref(xval,wam));
#else
  if(VAR(xval))
    {  bp_long a=(term *)xval-wam[HeapStk].base;
       if(a>=0)
         fprintf(STD_err,"_%ld ??? ",a);
       else
         fprintf(STD_err, "mem[%lu] ??? ",xval);
    }
  else if(INTEGER(xval)) fprintf(STD_err,"%ld ??? ",OUTPUT_INT(xval));
  else fprintf(STD_err,"%s/%ld ??? ",NAME(xval),GETARITY(xval));
#endif
  warnmes(Msg);
}
  return NULL;
}

void link_halt(void)
{
  if(OUTPUT_INT(g.linking)) bp_halt(3);
}


void overflow_by(term *Top, bp_long LimitNo, stack wam, string culprit)
{ bp_long delta=((char *)Top-(char *)wam[LimitNo].margin);
  if(QLEVEL()<6)
  {
    fprintf(STD_err,
    "*** %s overflow by %ld bytes (safety left: %ld bytes)\nculprit=>%s\n",
         wam[LimitNo].name,
         delta, wam[LimitNo].over*sizeof(term)-delta,culprit);
   }
   link_halt();
}

/* initializes specsyms */

static no hdef_string(string k1, string k2, string val)
{
  return hdef(INPUT_STRING(k1),INPUT_STRING(k2),INPUT_STRING(val),g.timestamp);
}

static no hdef_int(string k1, string k2, bp_long val)
{
  return hdef(INPUT_STRING(k1),INPUT_STRING(k2),INPUT_INT(val),g.timestamp);
}

static no hdef_ptr(string k2, bp_long val)
{
#if 0
  fprintf(STD_err,"hdef_ptr: g.bp_state=%ld\n",g.bp_state);
  fprintf(STD_err,"hdef_ptr: g.true=%ld\n",g.true);
#endif
  return hdef(g.bp_state,INPUT_STRING(k2),PTR2INT(val),g.timestamp);
}

#define HDEFS(Var,Val) hdef_string("bp_state",(Var),(Val))
#define HDEFI(Var,Val) hdef_int("bp_state",(Var),(Val))

#define VSHARE(PVar,CVar) \
if(!hdef_ptr((PVar),((cell)(&(CVar))))) \
fatal_error("sharing C-variable")

#define BP_VAL(K1,K2,Val) hdef(K1,K2,(Val),g.timestamp)

/* happens before load */
static void make_constants(int argc, char **argv)
{
  g.NIL=INPUT_STRING("[]");
  g.DOT=new_func(".",2);
  g.DIF=new_func("-",2);
  g.DIV=new_func("/",2);
  g.VAR=new_func("var",1);
  g.INT=new_func("int",1);
  g.empty=INPUT_STRING("$empty");
  g.query=g.empty;
  g.answer=g.empty;
  g.callback=g.empty;

  g.compare_vals[0]=INPUT_STRING("<");
  g.compare_vals[1]=INPUT_STRING("=");
  g.compare_vals[2]=INPUT_STRING(">");

  g.predmark=INPUT_STRING("predmark");
  g.ucount=INPUT_STRING("ucount");
  g.seemark=INPUT_STRING("seemark");
  g.tellmark=INPUT_STRING("tellmark");


#if JUMP_COMPRESS
  g.jcompressmark=INPUT_STRING("jcompressmark");
#endif

  g.end_of_file=INPUT_STRING("end_of_file");
  g.opmark=INPUT_STRING("opmark");

  g.xfx=INPUT_STRING("xfx");
  g.xfy=INPUT_STRING("xfy");
  g.yfx=INPUT_STRING("yfx");

  g.fx=INPUT_STRING("fx");
  g.fy=INPUT_STRING("fy");
  g.xf=INPUT_STRING("xf");
  g.yf=INPUT_STRING("yf");

  g.prefixop=INPUT_STRING("prefixop");
  g.infixop=INPUT_STRING("infixop");
  g.postfixop=INPUT_STRING("postfixop");

  g.seefunc=g.tellfunc=g.user=INPUT_STRING("user");
  g.seefile=STD_in;
  g.tellfile=STD_out;
  g.closed_file=INPUT_STRING("closed_file");

  g.bp_state=INPUT_STRING("bp_state");
  g.bp_virtual=INPUT_STRING("bp_virtual");

  hdef(g.seemark,g.user,(cell)STD_in,g.timestamp);
  hdef(g.tellmark,g.user,(cell)STD_out,g.timestamp);

  g.bp_float=new_func("$float",3);
  g.c=INPUT_STRING("c");
  g.bp=INPUT_STRING("bp");
  g.wam=INPUT_STRING("wam");
  g.h=INPUT_STRING("h");
  g.pro=INPUT_STRING("pro");
  g.pl=INPUT_STRING("pl");
  g.mem=INPUT_STRING("mem");

  g.compile=INPUT_STRING("mcompile");
  g.reconsult=INPUT_STRING("dconsult");
  g.current_db=INPUT_STRING("user");
  g.current_user_file=INPUT_STRING("noname.pl");

  g.source=g.pl;
  g.target=g.mem;
  g.implem=INPUT_INT(0);

  g.linking=INPUT_INT(1);
  g.self=FALSE;

  g.gc=INPUT_INT(1);

  g.peval_io=INPUT_INT(0);

  /* see g.bbgc and g.bbhi in define_bb*/

  g.argc=argc;
  g.argv=argv;

  g.true=INPUT_STRING("true");

  MAKE_BP_VALS();
}

#define NAME2PRED(Name,Arity) GETPRED(new_func((Name),(Arity)+1))

#define INIT_PRED(Pred,Name,Arity) \
if(!(Pred=NAME2PRED(Name,Arity))) \
  { fprintf(STD_err,format_mes,Name,Arity); \
    fatal_error("inconsistent runtime system"); \
  }

/* happens before load */
static bp_long hdefaults(void)
{
  return
    HDEFS("startup_mes",startup_mes()) &&
    HDEFS("hostname",get_host()) &&
    HDEFS("ip_addr",get_ip_addr()) &&
    HDEFI("version",VERSION) &&
    HDEFI("inC",g.inC) &&
    HDEFI("heap_size",W2Ks(max.HEAP)) &&
    HDEFI("stack_size",W2Ks(max.CHOICE)) &&
    HDEFI("trail_size",W2Ks(max.TRAIL)) &&
    HDEFI("jump_compress",JUMP_COMPRESS) &&
    HDEFI("struct_compress",STRUCT_COMPRESS) &&
    HDEFI("eager_deref",EAGER_DEREF) &&
    HDEFI("gc_level",GC) &&
    HDEFI("var",VARTAG) &&
    HDEFI("int",INTTAG) &&
    HDEFI("/",FUNTAG) &&
    HDEFI("?",BADTAG) &&
    HDEFI("tagbits",TAGBITS) &&
    HDEFI("wordsize",(sizeof(bp_long)<<3)) &&
    HDEFI("threads",THREADS) &&
    HDEFI("x86",VCC)
  ;
}

/* happens after load !!! */
static void init_const_instr(void)
{ static string format_mes="No definition for: %s/%d.\n";
  INIT_PRED(g.call,"call",1);
  /*INIT_PRED(g.prolog_main,"main",1);*/
  INIT_PRED(g.prolog_load,"prolog_load",1);
  INIT_PRED(g.prolog_init,"prolog_init",1);
  INIT_PRED(g.prolog_run,"prolog_run",1);
  INIT_PRED(g.metacall,"metacall",1);
  INIT_PRED(g.metatrue,"metatrue",0);
  INIT_PRED(g.member_entry,"member_entry",2);
  INIT_PRED(g.for_entry,"for_entry",3);

  VSHARE("peval_io",g.peval_io);

  VSHARE("gc",g.gc);
  VSHARE("bbgc",g.bbgc);
  VSHARE("bbhi",g.bbhi);

  VSHARE("quiet",max.QUIET);
  VSHARE("this_port",max.PORT);
  VSHARE("load_method",max.LMETH);
  VSHARE("db_ratio",max.DB_RATIO);
  VSHARE("seefile",g.seefile);
  VSHARE("lineno",g.lineno);
  VSHARE("tellfile",g.tellfile);
  VSHARE("seefunc",g.seefunc);
  VSHARE("tellfunc",g.tellfunc);
  VSHARE("user_input",g.user_input);
  VSHARE("user_output",g.user_output);
  VSHARE("linking",g.linking);
  VSHARE("source",g.source);
  VSHARE("target",g.target);
  VSHARE("implem",g.implem);

  VSHARE("current_db",g.current_db);
  VSHARE("current_user_file",g.current_user_file);
  VSHARE("startup_file_name",g.startup_file_name);
  VSHARE("compile",g.compile);
  VSHARE("reconsult",g.reconsult);

  VSHARE("query",g.query);
  VSHARE("answer",g.answer);
  VSHARE("callback",g.callback);
}

/* MAKE & INIT data objects */


byte *make_byte_array(bp_long a_size)
{
  return ZALLOC(a_size,byte);
}

char *make_char_array(bp_long a_size)
{
  return ZALLOC(a_size,char);
}

hentry make_hentry(bp_long a_size)
{
  return ZALLOC(a_size,struct hentry);
}

string *make_atomtable(bp_long a_size)
{
  return ZALLOC(a_size,string);
}

static void fill_var_array(term t,bp_long a_size)
{ bp_long i;
  for(i=0; i<a_size; i++)
    SETREF(t+i,t+i);
}

term make_var_array(bp_long a_size)
{ term t;
  t=(term)TALLOC(a_size);
  fill_var_array(t,a_size);
  return t;
}

/*
static term new_var_array(bp_long size)
{ term handle;
  handle=make_var_array(size);
  SETREF(handle,handle+1);
  return handle;
}
*/

void init_var_array(term handle,bp_long size)
{
  fill_var_array(handle,size);
  SETREF(handle,handle+1);
}

no make_stack(stack s, bp_long n, bp_long check, string name, term *area, no notzeroed)
{
  s->size=n;
  s->over=check;
  if(n<check) return FALSE;

  if(!area) {
      area = (notzeroed ? TALLOC(n) : ZALLOC(n,term));
  }

  s->base=area;
  s->end=s->base+s->size;
  s->margin=s->end-s->over;
  s->top=s->base;
  s->oldtop=s->base;
  s->maxused=0;
  s->name=name;
  return !!s->base;
}

/** $$$ TODO
no expand_stack(stack s) {
  term *base0=s->base;
  term *top0=s->top;
  term *oldtop0=s->oldtop;
  s->size=s->size<<1;
  s->over=s->over<<1;
  s->base=RALLOC(s->base,s->size,term);
  s->end=s->base+s->size;
  s->margin=s->end-s->over;
  s->oldtop=s->oldtop-base0+s->base;
  s->top=s->top-base0+s->base;
  return !!s->base;
}
*/

#define MAKE_BBOARD(Size,Where) \
make_stack(&g.shared[BBoardStk],(Size),(MAXARITY<<1)+1,"board",(Where),1)

no make_bboard(register stack wam)
{
#if defined REUSE
  if(max.BOARD>wam_bp_size)
    return MAKE_BBOARD(max.BOARD,NULL);
  else
    return MAKE_BBOARD(wam_bp_size/sizeof(term),(term *)wam_bp);
#else
  return MAKE_BBOARD(max.BOARD,NULL);
#endif
}

bp_long init_common_mem(register stack wam)
{
  no errctr=0;

  errctr+=!make_stack(&g.shared[InstrStk],max.CODE,4,"code",NULL,1);
  errctr+=!make_symtable();

  init_instr_len();

  g.stop=g.sbuf=make_char_array(max.SBUF);

  errctr += !g.sbuf;

  /* this makes bboard older than htable (to look more permanent) */

  if(!OUTPUT_INT(g.bbhi)) errctr+=!make_bboard(wam);

  /* should be older than heap (to look more permanent) */
  /* errctr+=!(g.connect=new_var_array(MAXDCG)); GC BUG ->local to engine */

  errctr+=!(g.engines=ZALLOC(MAXENGINES,stack));
  g.lastengine=g.engines-1;
  errctr+=!(hinit() && hdefaults());

  if(errctr)
    ERREXIT("not enough common memory")
  else
    return TRUE;
}

no is_engine(register stack wam)
{ register stack *p;
  if(NULL==wam) return FALSE;
  for(p=g.engines;p<=g.lastengine;p++)
    {
       if(*p==wam)
         {
           return TRUE;
         }
    }
  return FALSE;
}

void set_engine_prop(cell engine,int propNo,cell valNo) {
  stack wam=INT2PTR(engine);
  if(!is_engine(wam)) {
    warnmes("trying to set engine property on a non-engine"); /*$$*/
    return;
  }
  SET_ENGINE(propNo,valNo);
#if (TRACE>0)
  fprintf(STD_err,"TRACE set_engine_prop: %ld %ld %ld\n",
    OUTPUT_INT(engine),propNo,valNo);
#endif
}

cell get_engine_prop(cell engine,int propNo) {
  if((no)propNo>=max.ENGINE_ATTR) return INPUT_INT(0);
  { stack wam=INT2PTR(engine);
    if(!is_engine(wam)) {
      warnmes("trying to get engine property on a non-engine");
      return INPUT_INT(0); /*$$*/
    }
    return (cell)GET_ENGINE(propNo);
  }
}

int get_engine_thread(register stack wam) {
  return OUTPUT_INT(get_engine_prop(PTR2INT(wam),ENGINE_THREAD));
}

static no add_engine(register stack wam, bp_long engine_type, stack parent)
{ no i;
  if(g.lastengine>=g.engines+MAXENGINES-1)
    {
      warnmes("add_engine: engine table full");
      return FALSE;
    }
  g.lastengine++;
  *g.lastengine=wam;
  for(i=0; i<max.ENGINE_ATTR; i++) {
    set_engine_prop(PTR2INT(wam),i,INPUT_INT(0));
  }
  SET_ENGINE_TYPE(engine_type);
  SET_ENGINE_PARENT(parent);
  SET_ENGINE_ERROR(ENGINE_OK);
  set_engine_prop(PTR2INT(wam),ENGINE_THREAD,INPUT_INT(current_thread(wam)));
  TRACE_ENGINE("add_engine")
  return TRUE;
}

static no del_engine(register stack wam)
{ register stack *p;
  for(p=g.engines;p<=g.lastengine;p++)
    {
       if(*p==wam)
         { *p = *g.lastengine;
           g.lastengine--;
           TRACE_ENGINE("del_engine")
           return TRUE;
         }
    }
  warnmes("del_engine: engine notfound");
  return FALSE;
}


bp_long heap_safe_margin(bp_long heap_size)
{
   bp_long at_least=MAXREG<<1;
   bp_long at_most=MAXREG<<3;
   bp_long margin=heap_size>>6;
#if GC>1
   margin=at_least;
#else
   if (margin<at_least) margin=at_least;
   if (margin>at_most) margin=at_most;
#endif
   return margin;
}

bp_long init_engine_mem(stack wam,
  bp_long heap_size, bp_long stack_size,
  bp_long trail_size, bp_long engine_type,
  stack parent)
{

  no errctr=0;

  /* normally heap_safe_margin(..) must be the max size of what
     can go to the heap between 2 EXECUTEs */

  errctr+=!make_stack(&wam[MesStk],max.ENGINE_ATTR,4,
                      "engine_attributes",NULL,1);

#if GC>0
  wam[MarkStk].size=ZERO; /* Lazy allocation: on the first use of GC*/
  wam[MarkStk].base=NULL;
#endif

  errctr+=!make_stack(&wam[ChoiceStk],stack_size,MAXARITY+3,"choice",
          NULL,1);


  errctr+=!make_stack(&wam[TrailStk],trail_size,MAXARITY+1,"trail",
          NULL,1);


  errctr+= !make_stack(&wam[HeapStk],heap_size,
                       heap_safe_margin(heap_size),"heap",NULL,1);

  /* starting from 5.84 dcg streams are local to each wam */

  if(errctr)
    ERREXIT("not enough engine memory")

/* moved to engine
  { term g_connect=DCGSTART();
    init_var_array(g_connect,MAXDCG);
  }
*/
  if(!add_engine(wam,engine_type,parent))
    ERREXIT("add_engine failed");

  return TRUE;
}


stack create_engine(register stack oldwam, bp_long h, bp_long s, bp_long t)
{ register stack wam;
  wam=ZALLOC(MaxStk,struct stack);
  if(!wam) return NULL;
  ScaleToWords(h);
  ScaleToWords(s);
  ScaleToWords(t);
  {bp_long i; for(i=0;i<MaxStk;i++) wam[i]=oldwam[i];}
  if(!init_engine_mem(wam,h,s,t,EMPTY_ENGINE,oldwam)) return NULL;
  return wam;
}

no destroy_engine(register stack wam)
{
  /* if(!is_engine(wam)) return FALSE; */
#if THREADS>0
  if(is_engine(wam) && RUNNING_ENGINE==GET_ENGINE_TYPE()) {
    int t=get_engine_thread(wam);
    /*fprintf(STD_err,"!!!!thread=%d\n",t);*/
    (void)thread_destroy(t);
  }
#endif
  if(!del_engine(wam)) return FALSE;
  SET_ENGINE_TYPE(DEAD_ENGINE);
  XFREE(wam[HeapStk].base);
  XFREE(wam[ChoiceStk].base);
  XFREE(wam[TrailStk].base);
#if GC>0
  if(NULL!=wam[MarkStk].base)
    XFREE(wam[MarkStk].base);
#endif
  XFREE(wam[MesStk].base); /* contains also props of the engine */
  XFREE(wam);
  return TRUE;
}

term list_engines(register term H)
{ register stack wam,*p;
  for(p=g.engines+0;p<=g.lastengine;p++)
    {      wam=*p;
           PUSH_LIST(PTR2INT(wam));
#if (TRACE > 0)
           fprintf(STD_err,
             "engine [%d]=[%lu], type=%ld\n",
              p-g.engines,(no)wam,GET_ENGINE_TYPE()
           );
#endif
    }
  PUSH_NIL();
  return H;
}


/* random */

void init_random(no seed)
{ if(0==seed) seed=17+(unsigned)realtime(3);
  /*fprintf(STD_err,"seed=%d\n",seed);*/
  srand(seed);
  rand();
}

void start_profiler(register stack wam)
{
#if TRACE>1 || PROF
  profiler(0,cbase,wam[ChoiceStk].base,(term)wam[HeapStk].base,wam);
#endif

#if TRACE>0 || PROF
  fprintf(STD_err,"BEGIN EXECUTION PROF=%d TRACE=%d\n\n",PROF,TRACE);
#endif
}

void end_profiler(register stack wam)
{

#if TRACE>1 || PROF==3
  profiler(2,cbase,wam[ChoiceStk].base,(term)wam[HeapStk].base,wam);
     /*mallocmap();*/
#endif

#if TRACE==3
  show_regions(wam[ChoiceStk].base,(term)wam[HeapStk].base,wam);
#endif
}
/* TOPLEVEL */


#if 0
#define OPTINT(IntVar) (sscanf(optarg,"%ul",&(IntVar)))
#else
#define OPTINT(IntVar) ((IntVar=atol(optarg))>=0 \
|| !fprintf(STD_err,"*** BAD OPTION=%s\n",optarg) \
)
#endif

#define STACKOPT(Key,Var,Max,Type) \
case Key: \
  if(OPTINT(Var)) Max=(Var)*1024/sizeof(Type); \
  else errflag++; \
break


#define DICTOPT(Key,Var,Max,A,B) \
   case Key:\
      if(OPTINT(Var) && (Var)>=(A) && (Var)<=((B) * sizeof(cell)/sizeof(int)) ) \
         Max=ONE<<(Var);\
      else errflag++;\
    break

#define PLAINOPT(Key,Var,Max,Scale) \
   case Key: \
      if(OPTINT(Var)) \
         Max=(Var)*(Scale); \
      else errflag++;  \
    break

#define INTOPT(Key,Var,Max) \
   case Key: \
      if(OPTINT(Var)) \
         Max=INPUT_INT(Var); \
      else errflag++;  \
    break

#define SWITCHOPTS() \
    STACKOPT('h',h,max.HEAP,term);\
    STACKOPT('s',s,max.CHOICE,term);\
    STACKOPT('t',t,max.TRAIL,term);\
    STACKOPT('c',c,max.CODE,cell);\
    STACKOPT('b',b,max.BOARD,term);\
     DICTOPT('d',d,max.DICT,15,27);\
     DICTOPT('a',a,max.ATOMS,15,SYMBITS);\
    PLAINOPT('i',i,max.SBUF,1024);\
    INTOPT('q',q,max.QUIET);\
    INTOPT('l',l,max.LMETH);\
    INTOPT('r',r,max.DB_RATIO);\
    INTOPT('p',p,max.PORT);



static void opt_help() {
  (void) fprintf(STD_err, "cellsize=%u termsize=%u symbits=%u\n",
      (unsigned)sizeof(cell), (unsigned)sizeof(term), (unsigned)SYMBITS);
  (void) fprintf(
      STD_err,
      "%s %s%ld %s%ld %s%ld %s%ld %s%ld %s%ld %s%ld %s%ld %s%s%s\n\n",
      "\nBinProlog command line:\n\nOPTIONS:\n",

      "\n-h ==> HEAP SIZE in Kbytes,  default:",
      (long)W2Ks(max.HEAP),
      "\n-s ==> STACK SIZE in Kbytes, default:",
      (long)W2Ks(max.CHOICE),
      "\n-t ==> TRAIL SIZE in Kbytes, default:",
      (long)W2Ks(max.TRAIL),
      "\n-c ==> CODE SIZE in Kbytes,  default:",
      (long)W2Ks(max.CODE),
      "\n-a ==> MAX. ATOMS, give exponent of 2, 2**default=",
      (long)max.ATOMS,
      "\n-d ==> HASH DICT. entries, give exponent of 2,  2**default=",
      (long)max.DICT,
      "\n-i ==> IOBUFFER, in Kbytes, default:",
      (long)max.SBUF >> 10,
      "\n-q ==> QUIETNESS level, default:",
      (long)OUTPUT_INT(max.QUIET),

      "\n\nARGUMENTS:\n",
      "\n  STARTUP FILE: *.bp, *.pl, *.wam or\n  GOAL: pred(args),\n  default: ",
      "wam.bp");
}

static void state_info() {
(void)fprintf(STD_err,"MAXARITY:%d MAXREG:%d TEMPARGS:%d MAXDCG:%d\n\n",
                       (int)MAXARITY,   (int)MAXREG,   (int)TEMPARGS,   (int)MAXDCG);
}


#define BADOPTS() {\
  opt_help();\
  bp_halt(2);\
}

#define DECLOPTS() bp_long h,s,t,c,b,d,a,i,q,l,r,p


#if defined NOGETOPT

#include <string.h>

#define ISOPT() (strlen(argv[nth])>=2 && '-'==argv[nth][0])

#define OPTKEY() (argv[nth][1])

#define OPTVAL() (argv[nth]+2)

#define NONOPTVAL() (argv[nth])

string get_cmd_line_options(int argc, char *argv[])
{
  int nth, errflag=0;
  char *fname=NULL;
  char option,*optarg;
  DECLOPTS();

  for(nth=1; nth<argc; nth++)
   {
     if(ISOPT())
       {
        option=OPTKEY();
        optarg=OPTVAL();

        switch(option)
          {

            SWITCHOPTS()

            default:
              fprintf(STD_err,"bad option: -%c%s\n",option,optarg);
              BADOPTS()
            break;
          }

        }
     else
       if(!fname) fname=NONOPTVAL();
   }

  return fname;
}

#else

#if 0
extern int getopt(int argc, char **argv, char *optstring);
#endif

extern char *optarg;
extern int optind;

string get_cmd_line_options(int argc, char **argv)
{
  bp_long argno,ctr,errflag=0;
  char option,*fname; DECLOPTS();

  argno=ctr=argc;

  while(ctr-- &&
     (option = getopt(argc, argv, "h:s:t:c:b:d:a:i:q:l:r:p:")) != EOF)
  switch (option)
  {

    SWITCHOPTS()

    default:
#if !defined SGI && !defined AIX
      if(ctr) errflag++;
#endif
    break;
  }

  if (errflag) BADOPTS()
  argno=optind;

  if(argno < argc)
    fname=argv[argno];
  else
    fname=NULL;
  return fname;
}
#endif

static void configure_bb()
{
  if(0==max.BOARD)
    {
      max.BOARD=1<<12;     /* small initial bboard size*/
      g.bbgc=INPUT_INT(2); /* dynamic gc-able bboard */
      g.bbhi=INPUT_INT(1); /* high bboard */
    }
  else
    {
      g.bbgc=INPUT_INT(1); /* static gc-able bboard*/
      g.bbhi=INPUT_INT(0); /* low bboard */
    }
}

#if TRACE==0
static
#endif
  struct stack root_wam[MaxStk];

#if ERROR_HANDLER
void install_interrupt_handler();

void FORCE_CDECL bp_interrupt_handler(int i) {
  static int ctr=0;
  register stack wam=root_wam;
  int errcode=OUTPUT_INT(g.err.id);
  static bp_long last_interrupt_time=ZERO;
  bp_long this_time=realtime(2);
  if(this_time-last_interrupt_time<=1)
     errcode=1; /* exit if ^C is pressed twice */
  fprintf(STD_err,"\nPrevious interrupt caught %ld seconds ago!\n",
    this_time-last_interrupt_time);
  last_interrupt_time=this_time;
  ctr++;
  if(errcode || g.stopper) {
    fprintf(STD_err,
	"\nprogram interrupted(times=%d,error=%d)...exiting\n",
      ctr,errcode);
    bp_halt(errcode);
  }
  else {
	/* g.stopper tries to NOTIFY all running engines
	   to return NULL at the next SAFE return point
	*/
	g.stopper=(sizeof(cell)<<3)-1; /* ensures: (x >> g.stopper) == 0 */

	if(OUTPUT_INT(g.linking)) {
      restart_orig(wam);
	   fprintf(STD_err,
	    "\ninterrupted during link phase(times=%d,code=%d)...rebooting\n",ctr,i);
	}
    else {
      hbak(BBOARDTIME);
      fprintf(STD_err,
	    "\ninterrupted(times=%d,code=%d)...reinitializing\n",ctr,i);
    }
  }
  install_interrupt_handler();
}


void install_interrupt_handler() {
#if 0 && (VCC==0)
  signal(SIGQUIT,bp_halt);
  signal(SIGINT,bp_halt);
  signal(SIGPIPE,SIG_IGN);
#if THREADS>0 && SOLARIS>0
  sigignore(SIGUSR1);
  sigignore(SIGUSR2);
#endif
#else
  // OS X turn off for dbugging with gdb
  signal(SIGINT,bp_interrupt_handler);
#endif
  /*
  fprintf(STD_err,"Installed interrupt handler\n");
  */
}
#endif

static stack new_bp(int argc, char **argv,FILE *bp_stdin,FILE *bp_stdout) {
  string fname;
  int quiet=FALSE;
  register stack wam=root_wam;

  g.stopper=0;

  g.inC=!!wam_bp_size;

  g.user_input=bp_stdin;
  g.user_output=bp_stdout; /* DO NOT CHANGE ! */

  g.lineno=0;

  init_mutexes();

  g.stime=cputime();

  fname=get_cmd_line_options(argc,argv);

  /* Allows quiet execution of CGI scripts in cgi-bin
     from 9.58 only suppresses initial message - for
     better debugging of scripts.

     If a file ends with *.wam *.pro, *.txt or starts with $
     no banner message is printed.
  */

  if(fname) {
	  if('$'== fname[0]) {
	    fname++;
	    quiet=TRUE;
	  }
	  else {
		  int l=strlen(fname);
		  if(l>4 && (
		    0==strcmp(".wam",fname+l-4) ||
		    (0==strcmp(".pro",fname+l-4) && QLEVEL()<3) ||
		    0==strcmp(".txt",fname+l-4)
		    )
		  )
		  quiet=TRUE;
	  }
  }

  if(quiet) max.QUIET=INPUT_INT(5); /* toplevel resets to 2 */
  else quietmes("%s\n",startup_mes());

  g.timestamp=SYSTIME;

  configure_bb();

  if(!init_common_mem(wam)) return NULL;

  if(QLEVEL()<2) opt_help();
  if(QLEVEL()<1) state_info();

#ifdef CPARSER
  inittokenizer(); /* added by kdb */
#endif

  make_constants(argc,argv); /* after h&sym table */

  g.timestamp=LOADTIME;

  if(!quiet) {
    char *where=(NULL==fname)? "internal code array":fname;
    quietmes("Start loading system code from <%s>.\n",where);
  }

  if(!load_kernel(fname,wam)) {
    return NULL;
  }

  if(!quiet) quietmes("%s\n","Finished loading system code.");

  init_const_instr();

  /* FOR A ROOT ENGINE DO ONLY THIS */


#if 1
  if(!init_engine_mem(wam,max.HEAP,max.CHOICE,max.TRAIL,ROOT_ENGINE,wam))
    return NULL;
#else
  if(!init_engine_mem(wam,K2Ws(64),K2Ws(8),K2Ws(8),ROOT_ENGINE,wam))
    return NULL;
#endif

/* the bboard is usually at a higher address in this case than the heap */

if(OUTPUT_INT(g.bbhi)) {
  if(!make_bboard(wam)) return NULL;
}

  VSHARE("code",g.shared[InstrStk].base);
  VSHARE("code_oldtop",g.shared[InstrStk].oldtop);
  VSHARE("code_top",g.shared[InstrStk].top);
  VSHARE("code_margin",g.shared[InstrStk].margin);

  VSHARE("bboard",g.shared[BBoardStk].base);
  VSHARE("bboard_top",g.shared[BBoardStk].top);
  VSHARE("bboard_margin",g.shared[BBoardStk].margin);

  VSHARE("heap_base",wam[HeapStk].base);
  VSHARE("heap_margin",wam[HeapStk].margin);
  VSHARE("heap",*HEAPSTART()); /* use directly val(bp_state,heap,X) to get this */
  VSHARE("regs",*LOCATEREGS()); /* addr of regs[0]: val(bp_state,regs,R) */
  VSHARE("dcgs",*DCGSTART());
  VSHARE("trail",wam[TrailStk].base);
  VSHARE("trail_margin",wam[TrailStk].margin);
  VSHARE("stack",wam[ChoiceStk].base);
  VSHARE("stack_margin",wam[ChoiceStk].margin);

  VSHARE("mes",wam[MesStk].base);
  VSHARE("mes_margin",wam[MesStk].margin);

  VSHARE("err_id",g.err.id);
  VSHARE("err_mes",g.err.mes);
  VSHARE("err_arg1",g.err.arg1);
  VSHARE("err_arg2",g.err.arg2);
  VSHARE("err_wam",g.err.wam);

  init_random(0);

 g.timestamp=RUNTIME;

  CLEAR_BP_ERROR();

/*
  #include <sys/resource.h>
  { struct rlimit rlp;

    getrlimit(RLIMIT_CORE, &rlp);
    rlp.rlim_cur=(rlim_t)0;
    setrlimit(RLIMIT_CORE,&rlp);

    getrlimit(RLIMIT_DATA, &rlp);
    rlp.rlim_cur=(rlim_t)max.MAX_CORE;
    setrlimit(RLIMIT_DATA,&rlp);

    getrlimit(RLIMIT_VMEM, &rlp);
    rlp.rlim_cur=(rlim_t)max.MAX_CORE;
    setrlimit(RLIMIT_VMEM,&rlp);
  }
*/

  init_orig_wam(wam);

#if ERROR_HANDLER
  install_interrupt_handler();
#endif

  return wam;
}


void *init_bp0(int argc, char **argv,FILE* bp_stdin,FILE* bp_stdout) {
  int i;
  stack wam=new_bp(argc,argv,bp_stdin,bp_stdout);
  if(NULL==wam) return NULL;
#if TRACE>0
  fprintf(STD_err,"entering interp(wam)\n");
#endif
  i=interp(wam,g.prolog_load);
  if(ENGINE_OK==i) /* run only if init is ok */
    interp(wam,g.prolog_init);
  return wam;
}

char *run_bp0(register stack wam,char *query,int *retcode) {

  int i=GET_ENGINE_ERROR();

  g.answer=g.empty;
  start_profiler(wam);
  if(ENGINE_OK==i) { /* run only if init_bp is ok */
    if(NULL!=query) {
    /*
      INPUT_STRING(query) can slowly result in a full symbol table
      do periodically ask_bp(restart,_) to clean it up
      note that this results in loosing content of the dynamic db
      among other unpredictables - should be fixed by avoiding
      to internalize "query" each time
     */
      g.query=PTR2INT(query); /*was INPUT_STRING(query);*/
    }
    i=interp(wam,g.prolog_run);
  }
  if(ENGINE_FORCE_HALT==i)
    i=ENGINE_OK; /* act like a normal halt */
  end_profiler(wam);

  *retcode=i;

  if(ENGINE_OK!=i) {
    if(NULL==query) query="default toplevel/0 or main/0";
    fprintf(STD_err,
      "*** fatal error in query: %s, code %d\n",query,i);
    /* returns NULL on error */
    return NULL;
  }

  /* return NULL on failure */
  if(g.answer==g.empty)
    return NULL;

  /* returns first answer if suceeding to find it */
  return INT2PTR(g.answer); /* was NAME(g.answer) */
}

