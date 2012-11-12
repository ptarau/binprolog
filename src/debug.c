#include "defs.h"
#include "global.h"
/* this if makes compilation of the whole file conditional */
#if TRACE>0 || PROF

extern string lextable,newlex,*atomtable;
extern no newatom,skey(string s),hget(register no pred, register no fun);
extern string smartref(cell x, register stack wam);
extern no hcount;
extern hentry htable;
extern byte *hstamp;
extern byte *instr_len;
extern struct specsyms g;
extern struct limit max; 
extern term local_error(cell xval, string Msg, register stack wam);
extern bp_long cputime(void);

void *calloc_trace(no nb,no size)
{ void *ptr=calloc(nb,size);
  fprintf(STD_err,
    "!!! calloc-ing %ld elements of size %ld staring at %ld\n",
    nb,size,(no)ptr);
  return ptr;
}

void *malloc_trace(no nb,no size)
{ void *ptr=malloc(nb*size);
  fprintf(STD_err,
    "!!! malloc-ing %ld elements of size %ld staring at %ld\n",
    nb,size,(no)ptr);
  return ptr;
}

void free_trace(void *ptr)
{
  fprintf(STD_err,"!!! free-ing memory starting at %ld\n",(no)ptr);
  free(ptr);
}

#include "prof.h"

/* loop check limits */

#define MAXITER 8000L

#if STRUCT_COMPRESS
#define CASE_DOUBLE_INSTR \
  case UNIFY_VAR_VAR:   \
  case WRITE_VAR_VAR:   \
  case UNIFY_VAL_VAL:     \
  case WRITE_VAL_VAL:     \
  case UNIFY_VAR_VAL:     \
  case WRITE_VAR_VAL:     \
  case UNIFY_VAL_VAR:     \
  case WRITE_VAL_VAR: \
  case MOVE_REGx2: \
  case LOAD_VALUEx2: \
  case LOAD_VAL_SHORT
#endif

#define CASE_MOVE_INSTR \
  case MOVE_REG:        \
  case PUT_VARIABLE:  \
  case GET_VALUE: \
  case LOAD_VALUE: \
  case LOAD_VARIABLE

#define CASE_TRY_INSTR  \
  case TRY_ME_ELSE:     \
  case RETRY_ME_ELSE:   \
  case TRUST_ME_ELSE

string instr_name(no c)
{       
  switch(c)
  {
    case GET_STRUCTURE:     return "GET_STRUCTURE";
    case PUT_STRUCTURE:     return "PUT_STRUCTURE";
    case PUSH_STRUCTURE:    return "PUSH_STRUCTURE";

    case PUSH_VARIABLE:       return "PUSH_VARIABLE";

    case UNIFY_VARIABLE:    return "UNIFY_VARIABLE";
    case UNIFY_VALUE:               return "UNIFY_VALUE";
    case UNIFY_CONSTANT:    return "UNIFY_CONSTANT";
    
    case WRITE_VARIABLE:    return "WRITE_VARIABLE";
    case WRITE_VALUE:               return "WRITE_VALUE";
    case WRITE_CONSTANT:    return "WRITE_CONSTANT";
    
    case UNIFY_VOID:    return "UNIFY_VOID";
    case WRITE_VOID:    return "WRITE_VOID";

    case BEGIN_C_CHUNK:    return "BEGIN_C_CHUNK";
    case END_C_CHUNK:    return "END_C_CHUNK";

    case GET_CONSTANT:      return "GET_CONSTANT";
    case PUT_CONSTANT:      return "PUT_CONSTANT";
    
    case MOVE_REG:          return "MOVE_REG";
    case PUT_VARIABLE:      return "PUT_VARIABLE";
    case GET_VALUE:         return "GET_VALUE";
    
    case EXECUTE:           return "EXECUTE";
    case PROCEED:           return "PROCEED";
    case APPLY:             return "APPLY";


    case PUSH_CUT:          return "PUSH_CUT";
    case PUT_CUT:           return "PUT_CUT";
    case GET_CUT:           return "GET_CUT";

    case END:               return "END";
    
    case TRY_ME_ELSE:       return "TRY_ME_ELSE";
    case RETRY_ME_ELSE:     return "RETRY_ME_ELSE";
    case TRUST_ME_ELSE:     return "TRUST_ME_ELSE";
    case TRY_ME_ONLY:       return "TRY_ME_ONLY";

    case NONDET:            return "NONDET";
    case CLAUSE:            return "CLAUSE";
    case SWITCH:            return "SWITCH";
    case FIRSTARG:          return "FIRSTARG";
    case LOAD_CONSTANT:     return "LOAD_CONSTANT";
    case LOAD_VALUE:        return "LOAD_VALUE";
    case JUMP_IF:           return "JUMP_IF";

#if JUMP_COMPRESS
    case EXEC_TRY:          return "EXEC_TRY";
    case EXEC_SWITCH:       return "EXEC_SWITCH";
    case EXEC_JUMP_IF:      return "EXEC_JUMP_IF";
#endif

#if STRUCT_COMPRESS
    case UNIFY_VAR_VAR:     return  "UNIFY_VAR_VAR @@";
    case WRITE_VAR_VAR:     return  "WRITE_VAR_VAR @@";

    case UNIFY_VAL_VAL:     return  "UNIFY_VAL_VAL @@";
    case WRITE_VAL_VAL:     return  "WRITE_VAL_VAL @@";

    case UNIFY_VAR_VAL:         return  "UNIFY_VAR_VAL @@";
    case WRITE_VAR_VAL:         return  "WRITE_VAR_VAL @@";

    case UNIFY_VAL_VAR:         return  "UNIFY_VAL_VAR @@";
    case WRITE_VAL_VAR:         return  "WRITE_VAL_VAR @@";
    
    case GET_UNIFY_VAR_VAR:         return  "GET_UNIFY_VAR_VAR @@@";
    case GET_UNIFY_VAL_VAL:         return  "GET_UNIFY_VAL_VAL @@@";
    case GET_UNIFY_VAR_VAL:         return  "GET_UNIFY_VAR_VAL @@@";
    case GET_UNIFY_VAL_VAR:         return  "GET_UNIFY_VAL_VAR @@@";

    case PUT_WRITE_VAR_VAR:         return  "PUT_WRITE_VAR_VAR @@@";
    case PUT_WRITE_VAL_VAL:         return  "PUT_WRITE_VAL_VAL @@@";
    case PUT_WRITE_VAR_VAL:         return  "PUT_WRITE_VAR_VAL @@@";
    case PUT_WRITE_VAL_VAR:         return  "PUT_WRITE_VAL_VAR @@@";

    case PUSH_VAR_VAR:         return  "PUSH_VAR_VAR @@@";
    case CONSTANT_VAR_VAR:         return  "CONSTANT_VAR_VAR @@@";
    case PUSH_VAL_VAR:         return  "PUSH_VAL_VAR @@@";
    case CONSTANT_VAL_VAR:         return  "CONSTANT_VAL_VAR @@@";

    case PUSH_VAL_VAL:         return  "PUSH_VAL_VAL @@@";
    case CONSTANT_VAL_VAL:         return  "CONSTANT_VAL_VAL @@@";
    case PUSH_VAR_VAL:         return  "PUSH_VAR_VAL @@@";
    case CONSTANT_VAR_VAL:         return  "CONSTANT_VAR_VAL @@@";

    case MOVE_REGx2:            return  "MOVE_REGx2 @@";
    case LOAD_VALUEx2:          return  "LOAD_VALUEx2 @@";
    case LOAD_VAL_SHORT:        return  "LOAD_VAL_SHORT @@";
#endif
    case LOAD_VARIABLE:         return  "LOAD_VARIABLE";

    case NOP:                                           return "NOP";

    default:

    if(c>=INLINE && c<=NOP)
      return bu_name[c-INLINE];
    else return "*** BAD INSTRUCTION CODE ***";
  }
}

void show_instr(bp_long i, instr iptr, FILE *outf, register stack wam)
{       no op=GETOP(iptr);
  string name=instr_name(op);
  switch(op)
    {   
  	case END:
    	fprintf(outf,".");
  	break;
  
      CASE_TRY_INSTR:
 	 fprintf(outf,"\n<%ld> [%d]: (%ld) %-16s */%lu, ->",
 	   i,iptr-(instr)cbase,op,name,GETREG(iptr));
 	 fprintf(outf,"[%d] ",(instr)(GETLABEL(iptr))-(instr)cbase);
      break;

      CASE_MOVE_INSTR:
#if STRUCT_COMPRESS
      CASE_DOUBLE_INSTR:
#endif
 	 fprintf(outf,
	    "<%ld> [%d]: (%ld) %-16s X%ld,",i,
		iptr-(instr)cbase,op,name,GETREG(iptr));
  	 fprintf(outf,"A%lu",GETLEFT(iptr));
      break;
      
      case NONDET: case SWITCH: 
      case JUMP_IF: 
#if JUMP_COMPRESS
      case EXEC_JUMP_IF: case EXEC_TRY:
      case EXEC_SWITCH:
#endif
        fprintf(outf,"\n");
      default:
 	 fprintf(outf,"<%ld> [%d]: (%ld) %-16s",i,iptr-(instr)cbase,op,name);
  	if(0!=GETREG(iptr))
   		 fprintf(outf," X%lu",GETREG(iptr));
        if(2==INSTR_LEN(iptr))
 	   fprintf(outf," %s",smartref(GETFUN(iptr),wam));
    }
      fprintf(outf,"\n");
}


void list_range(instr p, cell maxi, stack wam)
{ int i;
    for(i=0; i<(bp_long)maxi; i++)
      { 
        show_instr(i,p,g.tellfile,wam);        
        if(BEGIN_C_CHUNK!=GETOP(p))
          p+=INSTR_LEN(p); 
        else
          { cell xval;
	    p+=INSTR_LEN(p);
            for(;xval=(cell)*p,(ATOMIC(xval)||COMPOUND(xval));p++)
               fprintf(g.tellfile,"<%d> CHUNK_DATA: %s\n", 
                   p-(instr)cbase,smartref(xval,wam));
            if(END_C_CHUNK!=GETOP(p))
            {
              fprintf(STD_err,"EXPECTED end_c_chunk HERE:\n");
              show_instr(i,p,g.tellfile,wam);   
            }     
          }
      }
}

cell list_asm(cell atom, cell arity, cell maxi, register stack wam)
{  cell fun; instr p; term xref; cell xval;
	FDEREF(atom); atom=xval;
  if(!SYMCONST(xval))
    return (cell)local_error(atom,"list_asm: 1st arg must be a symbol",wam);
	FDEREF(arity); arity=xval;
  if(!INTEGER(arity))
    return (cell)local_error(arity,"list_asm: arg 2 must an arity",wam);
	FDEREF(maxi); maxi=xval;
  if(!INTEGER(maxi))
    return (cell)local_error(maxi,"list_asm: arg 3 must be an integer",wam);
  arity=OUTPUT_INT(arity)+1;
  maxi=OUTPUT_INT(maxi);
  fun=PUTARITY(atom,arity);
  p=GETPRED(fun);
  if(!p) return FALSE;
  fprintf(g.tellfile,"BINARY PREDICATE: %s/%ld @code[%d]\n",
    NAME(atom),arity,p-(instr)cbase);
  list_range(p,maxi,wam);
  return TRUE;
}

#if TRACE>1 || PROF

void show_wam_instr(int i, instr iptr, term *A, term H, register stack wam)
{
  show_instr(i,iptr,STD_err,wam);
  
#if TRACE==3
  if(H>(term)wam[HeapStk].base && A>wam[ChoiceStk].base)
    { 
      fprintf(STD_err,"\tst:%lu SAVED_H:%lu H:%lu TR_TOP:%lu ",
  A-wam[ChoiceStk].base,SAVED_H-(term)wam[HeapStk].base,
  H-(term)wam[HeapStk].base,TR_TOP-wam[TrailStk].base);
      if(GETREG(iptr)>0 && GETREG(iptr)<MAXREG)
  fprintf(STD_err,"X%lu",GETREG(iptr));
      if(GETLEFT(iptr)>0 && GETLEFT(iptr)<MAXREG)
  fprintf(STD_err,"A%lu",GETLEFT(iptr));
      fprintf(STD_err,"\n");
    }
#endif
}


void show_tags(void)
{ 
  fprintf(STD_err,"FUNTAG->%lX ~FUNTAG->%lX\nINTTAG->%lX ~INTTAG->%lX\n",
    FUNTAG,~FUNTAG,INTTAG,~INTTAG);
}

void hshow(bp_long t, register stack wam)
{       no i; bp_long maxi=50;
  fprintf(STD_err,"\nHTABLE %lu/%lu\n",hcount+1,HMAX);
  for(i=0; maxi>0 && i<HMAX; i++)
    if(htable[i].val && hstamp[i]>=t)
    {       maxi--;
      fprintf(STD_err,"[%lu] pred->%s",i,smartref(htable[i].pred,wam));
      fprintf(STD_err," fun->%s",smartref(htable[i].fun,wam));
      fprintf(STD_err," val->%s stamp=%d\n",
	smartref(htable[i].val,wam), hstamp[i]);
    }
  fprintf(STD_err,"\n");
}

void show_atoms(void)
{       no i; 
    fprintf(STD_err,"ATOMTABLE: %lu atoms\n\n", newatom);
  for(i=0; i<MAXATOM; i++)
    if(atomtable[i]) 
      fprintf(STD_err,"[%lu] <<%s>>->%lu\n",i,
         atomtable[i],
         skey(atomtable[i]));
  fprintf(STD_err,"\n");
}       

void show_code(instr from, instr to, register stack wam)
{ instr p;
  fprintf(STD_err,"SHOWING CODE\n");
  for(p=from; p<to && p<(instr)ctop; p+=INSTR_LEN(p))
    show_wam_instr(p-(instr)cbase,p,wam[ChoiceStk].base,(term)wam[HeapStk].base,wam);
  fprintf(STD_err,"\n");
}

/* shows heap, stack & trail_stack */

void show_regions(term *A, term H, register stack wam)
{
  term t, *tp;
  
  fprintf(STD_err,"\nHEAP => H:%lu\n",H-(term)wam[HeapStk].base);
  for(t=(term)wam[HeapStk].base; t<H; t++)
    { term x=t; cell v;
      fprintf(STD_err,"[_%lu] ",t-(term)wam[HeapStk].base);
      while(VAR((v)=GETREF(x)) && (x)!=(term)(v)) 
        {       fprintf(STD_err,"_%lu->",(term)(v)-(term)wam[HeapStk].base);
          (x)=(term)(v);
        }
      fprintf(STD_err,":: %s\n",smartref(v,wam));
    }

  fprintf(STD_err,"\nTRAIL => TR_TOP:%lu\n",TR_TOP-wam[TrailStk].base);
  for(tp=wam[TrailStk].base; tp<TR_TOP; tp++)
    fprintf(STD_err,"[%lu]->[%lu] %s\n",tp-wam[TrailStk].base,
    *tp-(term)wam[HeapStk].base,smartref((cell)(*tp),wam));

  fprintf(STD_err,"\nCHOICE => %lu\n",A-wam[ChoiceStk].base);
  for(tp=wam[ChoiceStk].base; tp<=A; tp++)
    fprintf(STD_err,"[%lu] %s\n",tp-wam[ChoiceStk].base,
       smartref((cell)(*tp),wam));

  fprintf(STD_err,"\nBlackBOARD => %lu\n",g.shared[BBoardStk].top-g.shared[BBoardStk].base);
  for(tp=g.shared[BBoardStk].base; tp<g.shared[BBoardStk].top; tp++)
    fprintf(STD_err,"[%lu] %s\n",tp-g.shared[BBoardStk].base,
      smartref((cell)(*tp),wam));
}

#define heapused wam[HeapStk].maxused
#define stackused wam[ChoiceStk].maxused
#define trailused wam[TrailStk].maxused

#if PROF==2
#define LUCKY 13
#define DICE() (!(rand()%LUCKY))
#endif

#if PROF==3
#define DICE() 1
#endif

void profiler(byte mes, instr P, term *A, term H, register stack wam)
{ no i; 
  static double optable[MAXOP],optime[MAXOP];
  static bp_long all;
#if PROF>1
  static no t,prev_i;
  static bp_long prev_time;
#endif
  switch(mes)
    {
      case 0:
	  all=0;
 
#if PROF>1
	  prev_i=GETOP(P);
	  prev_time=cputime();
#endif
	  for(i=0; i<MAXOP; i++) {optable[i]=0.0; optime[i]=0.0;}
	  heapused=wam[HeapStk].base;
	  stackused=wam[ChoiceStk].base;
	  trailused=wam[TrailStk].base;
      break;

      case 1: 
#if PROF>1
          if(prev_i)
		{  t=cputime();
		   optime[prev_i] += (t-prev_time); 
		   prev_time=t;
		}
#endif
	  all++; 
	  optable[GETOP(P)]++; 
	  if((term *)H>heapused) heapused=(term*)H;
	  if(A>stackused) stackused=A;
	  if(TR_TOP>trailused) trailused=TR_TOP;
	    
#if TRACE==3
	  show_wam_instr(all,P,A,H,wam); 
	  if(MAXITER<all)
	    fatal_error("MAXITER instructions: INFINITE LOOP?");
	  if((term*)A<=wam[ChoiceStk].base)
	    fatal_error("stack underflow");
#endif
	   
#if TRACE==2
        switch(GETOP(P))
          {         
#if JUMP_COMPRESS                            
             case EXEC_SWITCH: case EXEC_JUMP_IF:
#endif
            case TRUE_0: case CALL_1:
#if STRUCT_COMPRESS
            CASE_DOUBLE_INSTR:
#endif
#if JUMP_COMPRESS
            case EXEC_TRY: 
#endif
            case EXECUTE: case APPLY:
              show_wam_instr(all,P,A,H,wam);
            break;

          }
#endif
#if PROF>1
	if(DICE()) {prev_i=GETOP(P); prev_time=cputime();} else prev_i=0;
#endif
      break;

      case 2:
      { no sz=sizeof(term); double total_t=0.001;
        fprintf(STD_err,"\n%-24s \t %-10s \t %-7s \t %-7s\n\n",
        "WAM-INSTRUCTION","TIMES USED","PERCENT","REL-TIME");
	for(i=0; i<MAXOP; i++) total_t+=optime[i];
  for(i=0; i<MAXOP; i++)
    { double count=optable[i];
      if(count)
	  { double p=100.0*optable[i]/all;
	    fprintf(STD_err,"[%3ld] %-24s \t %9.0f \t %6.2f%c \t %6.2f%c\n",
	    i,instr_name(i),count,p,'%',(100.0*optime[i])/total_t,'%');
	  }
    }
  fprintf(STD_err,"\n%-24s \t %9ld\n","TOTAL WAM INSTRUCTIONS:",all);
  fprintf(STD_err,"\n%s\n\n\n",
"REL-TIME is statistically relevant only for a large number of instructions");
  fprintf(STD_err,
  "@=> instr_len:%lx lex:%lu code:%lu trail:%lu\nchoice:%lu heap:%lu end:%lu\n",
          (no)instr_len,
    (no)lextable,
    (no)cbase,
    (no)wam[TrailStk].base,
    (no)wam[ChoiceStk].base,
    (no)wam[HeapStk].base,
    (no)wam[HeapStk].margin
    );
  fprintf(STD_err,
  "USED: code:%lux%lu=%lu/%lu\natoms:%lux(%lu/%lu) chars:%lu/%lu\n",
    sizeof(cell),ctop-cbase,
    sizeof(cell)*(ctop-cbase),
    sizeof(cell)*g.shared[InstrStk].size,
    sizeof(string),newatom,MAXATOM,
    newlex-lextable,MAXLEX);
  fprintf(STD_err,
  "heap:%lu/%lu stack:%lu/%lu trail:%lu/%lu\nhtable:%lux(%lu/%lu) bytes\n",
    sz*(heapused-wam[HeapStk].base),sz*wam[HeapStk].size,
    sz*(stackused-wam[ChoiceStk].base),sz*wam[ChoiceStk].size,
    sz*(trailused-wam[TrailStk].base),sz*wam[TrailStk].size,
    sizeof(struct hentry),hcount+1,HMAX);
  fprintf(STD_err,"H=%lu A=%lu TR=%lu bytes\n\n",
    sz*(H-(term)wam[HeapStk].base),
    sz*(A-wam[ChoiceStk].base),sz*(TR_TOP-wam[TrailStk].base));
#if TRACE>2
    show_tags();
    hshow(BBOARDTIME,wam);
#endif

#if TRACE>1
    show_atoms();
#endif

      }
      break;
    }
}

#endif
#endif
