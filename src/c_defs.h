/*extern term call_builtin();*/
extern struct specsyms g;
extern term float_op(register term H, byte opcode, term t1, term t2);
extern term float_fun(register term H, register term regs);
extern term float_fun2(register term H, register term regs);
extern term input_float(register term H, register term regs,
                    register stack wam);
extern no 
  unify(register cell v1, register cell v2, 
     register stack wam, register term *A),
  unify_to(register cell v1, register cell v2, 
     register stack wam, register term *A);

extern term local_error(cell xval, string Msg, register stack wam);
extern void 
   overflow_by(term *Top, bp_long LimitNo, stack wam, string culprit),
   bp_halt(bp_long i);

extern void fout(cell xval, stack wam, FILE *f);
extern bp_long compare(register term l, register term r);

extern cell hget(register no pred, register no fun),
       lval(register term regs, register stack wam, byte stamp);

extern no 
  def(register term regs, stack wam, byte stamp),
  set(register term regs, stack wam, byte stamp)
;

extern no 
  change_arg(register term regs, register stack wam, register term *A),
  setarg(register term regs, register stack wam, register term *A)
;

#define NOT_INTEGERS(Arg1,Arg2) \
(INTTAG|(INTTAG<<TAGBITS)) != (GETTAG(Arg1)|(GETTAG(Arg2)<<TAGBITS))

#define IF_NOT_INTEGER_OP() \
ires=(bp_long)X(1); xval=X(2); \
if(NOT_INTEGERS(ires,xval))

#define FLOAT_TEST(OpCode,A1,A2,Res,Offset) \
{ H += Offset;\
 (Res)=H; H=float_op(H,OpCode,(term)(A1),(term)(A2)); \
/* fprintf(STD_err,"FLOAT_TEST op=%d A1=%ld A2=%ld Res=%ld H=%ld\n", \
     OpCode,(A1),(A2),(cell)(Res),(cell)H); */ \
 if(!H) BFAIL() \
 H -= Offset; \
}

#define FLOAT_COMP(OpCode,Arg1,Arg2,RegNo,IsBound,Offset) \
{ FLOAT_TEST(OpCode,(Arg1),(Arg2),xref,Offset) \
  BOUT(xref,RegNo,IsBound); \
}

#define INT_COMP(Op,Arg1,Arg2,RegNo,IsBound) \
{ ires=INPUT_INT(OUTPUT_INT((bp_long)(Arg1) ) Op OUTPUT_INT((bp_long)(Arg2) )); \
  BOUT(ires,RegNo,IsBound) \
}

#define BCOMPUTE(OpName,OpCode,RegNo,IsBound,Offset) \
IF_NOT_INTEGER_OP() \
FLOAT_COMP(OpCode,ires,xval,RegNo,IsBound,Offset) \
else INT_COMP(OpName,ires,xval,RegNo,IsBound)

#define MUST_BE(Relop,OpCode) \
IF_NOT_INTEGER_OP() \
  { if(!float_op(H,OpCode,(term)ires,(term)xval )) BFAIL()} \
else \
  {if(!( ires Relop (bp_long)xval ) ) BFAIL()}

#define INT_ONLY(OpName,RegNo,IsBound) \
IF_NOT_INTEGER_OP() BFAIL() \
INT_COMP(OpName,ires,xval,RegNo,IsBound)

#define arith_op(OpName,OpCode,RegNo,IsBound,Offset) \
{ register bp_long ires; register term xref; register cell xval; \
  BCOMPUTE(OpName,OpCode,RegNo,IsBound,Offset); \
}


#define div_3(OpCode,RegNo,IsBound) \
{ register bp_long ires; register term xref; register cell xval; \
  if(INTEGER(X(2)) && 0==OUTPUT_INT(X(2))) \
                BWARFUN(X(1),"divised by 0"); \
    BCOMPUTE(/,OpCode,RegNo,IsBound,0); \
}

#define fdiv_3(OpCode,RegNo,IsBound,Offset) \
{ register term xref; \
  FLOAT_COMP(OpCode,X(1),X(2),RegNo,IsBound,Offset); \
}

#define random_1(RegNo,IsBound) \
  BOUT(INPUT_INT(((bp_long)((no)rand()>>TAGBITS))),RegNo,IsBound);

#define get0_1(RegNo,IsBound) \
{ register bp_long ires; \
            if(feof(g.seefile)) \
            {  clearerr(g.seefile); \
               /* warnmes("Read past EOF"); */ ires=0; \
            } \
            else \
              ires=(bp_long)getc(g.seefile); \
            BOUT(INPUT_INT(ires),RegNo,IsBound); \
}

#define put0_1() \
{ register cell xval=X(1); \
  if(!INTEGER(xval)) BWARFUN(xval,"integer expected in put/1");\
  putc(OUTPUT_INT(xval),g.tellfile);\
}

#define rel_op(OpName,OpCode) \
{ register bp_long ires; register cell xval; \
  MUST_BE(OpName,OpCode); \
}

#define int_only_op(OpName,RegNo,IsBound) \
{ register bp_long ires; register cell xval; \
  INT_ONLY(OpName,RegNo,IsBound); \
}

#define l_neg_3(RegNo,IsBound) \
{ register bp_long ires; register cell xval; \
  IF_NOT_INTEGER_OP() BFAIL() \
  xval=INPUT_INT(~OUTPUT_INT(X(2))); \
  INT_COMP(|,X(1),xval,RegNo,IsBound); \
}

#define compare0_3(RegNo,IsBound) \
{ register bp_long ires; register cell xval; \
  ires=compare(&X(1),&X(2)); \
  xval=g.compare_vals[ires+1]; \
  BOUT(xval,RegNo,IsBound); \
}

#define arg_3(RegNo,IsBound) \
{ register bp_long ires; register term xref; register cell xval; \
      xval=X(1);\
      if(!INTEGER(xval))\
	BWARFUN(xval,"arg/3's 1st arg must be integer");\
      ires=OUTPUT_INT(xval);\
      xref=C2T(X(2));\
      if(ATOMIC(T2C(xref)) ) BFAIL()\
      ASSERT2(VAR(T2C(xref)),xref);\
      xval=GETREF(xref);\
      if(VAR(xval))\
	BWARFUN(xval,"arg/3's 2nd arg must be nonvar");\
      if(ires<=0 || ires>(bp_long)(GETARITY(xval)))\
	BWARFUN(xval,"arg/3's 1st arg must be in 1..arity");\
      xref+=ires;\
      BOUT(xref,RegNo,IsBound);\
}

#define setarg_3() \
  if(!setarg(regs,wam,A)) BFAIL()

#define change_arg_3() \
  if(!change_arg(regs,wam,A)) BFAIL()

#define def_3() \
  if(!def(regs,wam,BBOARDTIME)) BFAIL()

#define set_3() \
  if(!set(regs,wam,BBOARDTIME)) BFAIL()

#define rm_2() \
  X(3)=g.empty; \
  set_3();

#define val_3(RegNo,IsBound) \
{ register cell xval; \
  ATOMIZE(X(1)); ATOMIZE(X(2)); \
  xval=hget(X(1),X(2)); \
  if(!xval || g.empty==xval) BFAIL() \
  SAFE_BOUT(xval,RegNo,IsBound); \
}
      
#define lval_3(RegNo,IsBound) \
{ register cell xval; \
  if(!(xval=lval(regs,wam,VARTIME))) BFAIL() \
  SAFE_BOUT(xval,RegNo,IsBound); \
}

#define symcat_3(RegNo,IsBound) \
{ register cell xval; \
  xval=symcat(regs,wam); if(!xval) BFAIL() \
  BOUT(xval,RegNo,IsBound); \
}

#define bad_instruction(II,Op,Reg,F,N)
#define BFAIL() {wam[ChoiceStk].top=A; return NULL;}
#define BUNIFAIL(Term1,Term2) if(!unify((Term1),(Term2),wam,A)) BFAIL()
#define SAFE_BUNIFAIL(Term1,Term2) if(!unify_to((Term1),(Term2),wam,A)) BFAIL()
#define SINC() S+=sizeof(cell)

#define BOUT(Expr,RegNo,IsBound) \
if(IsBound) \
  {BUNIFAIL((cell)(Expr),regs[RegNo])} \
else \
  regs[RegNo]=(cell)(Expr);

#define SAFE_BOUT(Expr,RegNo,IsBound) \
if(IsBound) \
  {SAFE_BUNIFAIL((cell)(Expr),regs[RegNo])} \
else \
  regs[RegNo]=(cell)(Expr);

#define BWARFUN(Sym,Msg) {LOCAL_ERR(Sym,Msg); BFAIL()}

#if 1
#define c_trace(Instr) \
fprintf(STD_err,"H=%ld P=%ld S=%ld CUT=%ld %s\n", \
  H-(term)wam[HeapStk].base,\
  (instr)P-(instr)g.shared[InstrStk].base,\
  S?(term)S-(term)wam[HeapStk].base:0, \
  (term *)cutB-wam[ChoiceStk].base,Instr);
#else
#define c_trace(Instr)
#endif


#define c_chunk_header(Name) \
static term Name(register term H, register term regs, \
            register instr P, cell cutB, \
            register term *A, register stack wam) \
{ register cell S=0;

#if TRACE>2
#define c_chunk_variable(Name) c_chunk_header(Name)
/* fprintf(STD_err,"Fun=%d H=%d P=%d\n",Name,H,P); */

#define c_chunk_value(Offset,No) \
   fprintf(STD_err,"H+O=%ld P+No=%ld A=%ld\n", \
     (H+Offset)-(term)wam[HeapStk].base, \
     (P+No)-(instr)g.shared.[InstrStk].base,\
     (term *)A-wam[ChoiceStk].base ); \
   wam[ChoiceStk].top=A; \
   wam[ChoiceStk].oldtop=(term *)(P+No); \
   return H+Offset; \
}
#else

#define c_chunk_variable(Name) c_chunk_header(Name)

#define c_chunk_value(Offset,No) \
   wam[ChoiceStk].top=A; \
   wam[ChoiceStk].oldtop=(term *)(P+No); \
   return H+Offset; \
}

#endif

/* instructions */

#if EAGER_DEREF>0
#define WRITEDERVAL(Offset,RegNo) \
        { register term xref; register cell xval; \
          FDEREF(regs[RegNo]); \
          H[Offset]=(cell)xref; \
        }
#else
#define WRITEDERVAL(Offset,Reg) H[Offset]=regs[Reg]
#endif

/* PURE TERM-CREATION (PUT) INSTRUCTION SUBSET */

#define put_structure(Offset,No,Reg) \
   regs[Reg]=(cell)(H+Offset); \
   H[Offset]=P[No];

#define put_constant(No,Reg) \
   regs[Reg]=P[No];

#define put_integer(No,Reg) \
   regs[Reg]=INPUT_INT(No);

#if EAGER_DEREF>1
#define write_value(Offset,Reg) \
    WRITEDERVAL(Offset,Reg)
#else
#define write_value(Offset,Reg) \
    H[Offset]=regs[Reg];
#endif

#define write_variable(Offset,Reg) \
  regs[Reg]=H[Offset]=(cell)(H+Offset);

#define write_constant(Offset,No) \
    H[Offset]=P[No];

#define write_integer(Offset,No) \
    H[Offset]=INPUT_INT(No);

#define push_variable()

#define push_structure(Offset,No,VarO) \
   H[VarO]=(cell)(H+Offset); \
   H[Offset]=P[No];

#define move_reg(RegN,RegI) \
    regs[RegN]=regs[RegI];

#define put_variable(Offset,RegN,RegI) \
  regs[RegN]=regs[RegI]=H[Offset]=(cell)(H+Offset);

#define write_void(Offset,Times) \
    {   register cell xval=Times; \
        register term xref=H+Offset; \
        while(xval--) \
          xref[xval]=(cell)(xref+xval); \
    }

/* saves cut point on the heap */
#define push_cut(Offset) \
    H[Offset]= CUT2INT(cutB);


/* % END OF pure PUT... instructions */
/* what follow usually requires A and wam as extra registers */

#define TIDY_TRAIL()

/* cuts to saved cut point */

#define get_cut() \
{ register cell xval; register term xref; \
      FDEREF(regs[1]); \
      A=(term *) INT2CUT(xval); \
      TIDY_TRAIL(); \
}

/* cuts right now */
#define put_cut() \
      A=(term *)cutB; \
      TIDY_TRAIL();

#define load_constant(Reg,No) \
   X(Reg)=P[No];

#define load_integer(Reg,No) \
   X(Reg)=INPUT_INT(No);

#define load_value(RegN,RegI) \
{ register cell xval; register term xref; \
  IN_VALUE(RegN,regs[RegI]); \
}
  
#define load_variable(Offset,RegN,RegI) \
{  X(RegN)=regs[RegI]=H[Offset]=(cell)(H+Offset); \
}


#define get_value(RegN,RegI) \
    BUNIFAIL(regs[RegN],regs[RegI]);

#define get_simple(Thing,RegNo) \
{ register term xref; register cell xval; \
      FDEREF(regs[RegNo]); \
      if(VAR(xval)) \
        { \
          SETCELL(xref,(Thing)); \
          SAFE_TRAIL_IF(xref); \
        } \
       else \
         { \
           if(xval!=(Thing)) BFAIL() \
         } \
}

#define get_constant(FunctorNo,RegNo) \
  get_simple(P[FunctorNo],RegNo)

#define get_integer(No,RegNo) \
  get_simple(INPUT_INT(No),RegNo)

#define COLLAPSE_FUN() H -= (H<xref+2);

#define get_structure(Offset,FunctorNo,RegNo) \
{ register term xref; register cell xval; \
      FDEREF(regs[RegNo]); \
      if(VAR(xval)) \
        { H+=Offset; \
	  S=0; \
          SAFE_TRAIL_IF(xref); \
          SETREF(xref,H); \
          COLLAPSE_FUN() \
          SETCELL(H,P[FunctorNo]); \
          H-=(Offset-1); \
        } \
       else \
         { \
           if(xval!=P[FunctorNo]) BFAIL() \
           S=T2C(xref+1);\
         } \
}

#define unify_variable(Offset,RegNo) \
      if(S) \
        {\
          regs[RegNo]=S; \
          SINC(); \
        }\
      else\
        {\
  	  H+=Offset;\
          regs[RegNo]=H[0]=(cell)H;\
  	  H-=(Offset-1); \
        }

#define unify_value(Offset,RegNo) \
      if(S) \
        {\
  	   BUNIFAIL(S,regs[RegNo]);\
	   SINC(); \
        }\
      else\
        { WRITEDERVAL(Offset,RegNo); \
          H++;\
        }

#define unify_simple(Offset,Thing) \
{ register term xref; register cell xval; \
      if(S) \
	  {  FDEREF(GETCELL(S)); SINC();\
             if(VAR(xval)) \
               { \
                  SETCELL(xref,(Thing));\
                  SAFE_TRAIL_IF(xref);\
               } \
               else if(xval!=(Thing)) BFAIL() \
          } \
      else \
        { \
           H[Offset]=(Thing); H++;\
        } \
}

#define unify_constant(Offset,FunctorNo) \
  unify_simple(Offset,P[FunctorNo])

#define unify_integer(Offset,IntVal) \
  unify_simple(Offset,INPUT_INT(IntVal))

#define unify_void(Offset,Times) \
if(S) S += Times*sizeof(cell); \
else \
    { \
        register cell xval=Times; \
        H+=Offset; \
        while(xval--) \
          H[xval]=(cell)(H+xval); \
        H-=(Offset-Times);\
    }

#define fail_0() BFAIL()

#define cwrite_1() fout(TEMP(0),wam,g.tellfile);

#define cnl_1() NEWLN();

#define var_1() \
{ register term xref; register cell xval; \
            FDEREF(TEMP(0)); \
            if(NONVAR(xval)) BFAIL() \
}

#define nonvar_1() \
{ register term xref; register cell xval; \
            FDEREF(TEMP(0)); \
            if(VAR(xval)) BFAIL() \
}

#define integer_1() \
{ register term xref; register cell xval; \
            FDEREF(TEMP(0)); \
            if(!INTEGER(xval)) BFAIL() \
}

#define atomic_1() \
{ register term xref; register cell xval; \
            FDEREF(TEMP(0)); \
            if(!ATOMIC(xval)) BFAIL() \
}

#define is_compiled_1() \
{ register term xref; register cell xval; \
            FDEREF(TEMP(0)); \
            xval=PUTARITY(xval,1+GETARITY(xval)); \
            if(!(GETPRED(xval))) BFAIL() \
}

