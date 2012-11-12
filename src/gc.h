extern struct specsyms g;
extern struct limit max;
extern string *atomtable;
extern term local_error(cell xval, string Msg, register stack wam);
extern term unify(register cell v1, register cell v2, register stack wam, register term *A);
extern string lextable,newlex;
extern no newatom;
extern no hcount;
extern no hget(register no pred, register no fun),hset(register no pred, register no fun, register no val),hdef(register no pred, register no fun, register no val, byte stamp);

#if TRACE > 0 || PROF
extern void *calloc_trace(no nb,no size),
            *malloc_trace(no nb,no size),
            free_trace(void *ptr);
#endif

#if GC>=2
#define PROFILE_GC
#if GC>=3
#define TRACE_GC_MEM
#if GC>=4
#define DEBUG_GC
#endif
#endif
#if GC>=5
#define MIMICK_LOOSE_SEGMENT_ORDER
#define FIND_UPPER_WITH_BITS
#endif
#endif

#ifdef TRACE_GC_MEM
bp_long size_old_heap ;
char *globalforwarded ;
#endif

#define pvtrailentry(ptr) ((cell)(*(ptr)) & TR_VTAG)

extern hentry htable ;

typedef struct cp_struct
{
  term start;
  bp_long arity;
  term H;
  term TR;
} cp;

#ifdef DEBUG_GC
static cp *cpoint;
static bp_long num_forwarded ;
static bp_long cpoints;
FILE *mark_info, *copy_info;
bp_long blocks_internal_referenced;
bp_long blocks_total;
term begin_new_heap, end_new_heap, begin_old_heap ;
char *marked ;
#endif

#define TR_GCTOP (term)(wam[TrailStk].top)
#define SET_TR_GCTOP(Val) wam[TrailStk].top=(term*)(Val)

#define TR_END (term)(wam[TrailStk].margin)

#define HP_BASE HEAPSTART()
#define CH_BASE (term)(wam[ChoiceStk].base)
#define TR_BASE (term)(wam[TrailStk].base)

#ifdef DEBUG_GC
#define DEBUG0(fileptr, format) fprintf(fileptr, format); \
fflush(fileptr)
#define DEBUG1(fileptr, format, arg1) fprintf(fileptr, format, arg1); \
fflush(fileptr)
#define DEBUG2(fileptr, format, arg1, arg2) fprintf(fileptr, format, arg1, arg2); \
fflush(fileptr)
#define DEBUG3(fileptr, format, arg1, arg2, arg3) fprintf(fileptr, format, arg1, arg2, arg3); \
fflush(fileptr)
#define DEBUG_CELL(fileptr, cell, wam) print_cell(fileptr, cell, wam); \
fflush(fileptr)
#define DEBUG_CLOSE(fileptr) fclose(fileptr)
#define line(f) \
  fprintf(f, "======================================\n")
#else
#define DEBUG0(fileptr, format) {}
#define DEBUG1(fileptr, format, arg1) {}
#define DEBUG2(fileptr, format, arg1, arg2)
#define DEBUG3(fileptr, format, arg1, arg2, arg3)
#define DEBUG_CELL(fileptr, cell,wam)
#define DEBUG_CLOSE(fileptr)
#endif

#define EXIT0(format) \
{ \
    fprintf(STD_err, format); \
    fflush(STD_err); \
    exit(-1); \
}

#define EXIT1(format, args1) \
{ \
    fprintf(STD_err, format, args1); \
    fflush(STD_err); \
    exit(-1); \
}

#define HP(ptr) (((term)(ptr))-begin_old_heap)
#define CH(ptr) (((term)(ptr))-CH_BASE)
#define TRGC(ptr) (((term)(ptr))-TR_BASE)
#define CD(ptr) (((term)(ptr))-((term)(g.shared[InstrStk].base)))
/*#define HT(ptr) (((term)(ptr))-((term)(&(htable[0].val))))*/
#define EX(ptr) (((term)(ptr))-(begin_new_heap))

#define UN_REACHABLE_TAG 0
#define REACHABLE_TAG 1
#define INTERNAL_TAG 2
#define REACHABLE_INTERNAL_TAG (REACHABLE_TAG|INTERNAL_TAG)

#ifndef TRACE_GC_MEM
#define MARK_REACHABLE(ptr,h,ma) ma[(term)(ptr)-h] = REACHABLE_TAG
#define UNMARK_REACHABLE(ptr,h,ma) ma[(term)(ptr)-h] = UN_REACHABLE_TAG
#define REACHABLE(ptr,h,ma) (ma[(term)(ptr)-h])

/*
char REACHABLE(term ptr, term h, char *ma)
{ bp_long i ; i = (term)(ptr)-h ;
	if ((i < 0) || (i >= 2400000)
     )
		fprintf(STD_err,"reachableoverflow\n") ;
	return(ma[i]) ;
}
*/

#define MARK_INTERNAL(ptr,h,m) m[(term)(ptr)-h] = REACHABLE_INTERNAL_TAG
#define INTERNAL(ptr,h,m) (m[(term)(ptr)-h] & INTERNAL_TAG)

#define MARK_FORWARDED(ptr,h)
#define FORWARDED(ptr,begin,end,h) \
	( VAR(*(ptr)) && (begin <= (term)(*ptr)) && ((term)(*ptr) < end))
/* end of the non-debug mode */
#endif


#ifdef TRACE_GC_MEM

MARK_REACHABLE(term ptr, term h, char *m)
{ bp_long i ; i = (term)(ptr)-h ; m[i]=REACHABLE_TAG ;
	if ((i < 0) || (i >= size_old_heap))
		fprintf(STD_err,"reachableoverflow MARK_REACHABLE %ld\n",(cell)ptr) ;
}

UNMARK_REACHABLE(term ptr, term h, char *m)
{ bp_long i ; i = (term)(ptr)-h ; m[i]=UN_REACHABLE_TAG ;
	if ((i < 0) || (i >= size_old_heap))
		fprintf(STD_err,"unreachableoverflow %ld\n",(cell)ptr) ;
}

REACHABLE(term ptr, term h, char *m)
{ bp_long i ; i = (term)(ptr)-h ;
	if ((i < 0) || (i >= size_old_heap))
		fprintf(STD_err,"reachableoverflow REACHABLE %ld\n",(cell)ptr) ;
	return(m[i]) ;
}

MARK_INTERNAL(term ptr, term h, char *m)
{ bp_long i ; i = (term)(ptr)-h ; m[i]=REACHABLE_INTERNAL_TAG ;
	if ((i < 0) || (i >= size_old_heap))
		fprintf(STD_err,"reachableoverflow MARK_INTERNAL %ld\n",(cell)ptr) ;
}

INTERNAL(term ptr, term h, char *m)
{ bp_long i ; i = (term)(ptr)-h ;
	if ((i < 0) || (i >= size_old_heap))
		fprintf(STD_err,"reachableoverflow INTERNAL %ld\n",(cell)ptr) ;
	return(m[i]& INTERNAL_TAG) ;
}

FORWARDED(term ptr, term begin, term end, term h)
{ bp_long i, b ;
  term p ;

	p = (term)(*ptr) ;
	i = (term)(ptr)-h ;
	if ((i < 0) || (i >= size_old_heap))
		fprintf(STD_err,"getforwardedoverflow\n") ;
	b = (globalforwarded[i]==REACHABLE_TAG) ;
	if (! VAR(p))
		{ if (b)
			fprintf(STD_err,"forwarded non var\n") ;
		}
	else	{ if ((begin <= p) && (p < end))
			{ if (! b)
				fprintf(STD_err,"pointer to new not forwarded\n") ;
			}
		  else if (b)
			fprintf(STD_err,"forwarded and not new pointer\n") ;
		}
	return(b) ;
}

MARK_FORWARDED(term ptr, term h)
{ bp_long i ;

	i = (term)(ptr)-h ;
	if (globalforwarded[i]==REACHABLE_TAG)
		fprintf(STD_err,"allready forwarded = %d\n",i) ;
	globalforwarded[i]=REACHABLE_TAG ;
	if ((i < 0) || (i >= size_old_heap))
		fprintf(STD_err,"forwardedoverflow\n") ;
}

#endif


#define TO_HEAP(ptr) ((begin_old_heap <=(term)ptr) \
		      && ((term)H>(term)ptr))
#define TO_EXTRA(ptr) ((begin_new_heap<=(term)ptr)  && (end_new_heap>(term)ptr))
#define SELFREF(ptr) (*(ptr)==(cell)(ptr))

#define FIND_LOWER_LIMIT \
lower=hpptr; \
while(INTERNAL(lower,begin_old_heap,marked)) lower--

#ifdef FIND_UPPER_WITH_BITS
#define FIND_UPPER_LIMIT \
upper=hpptr; \
while( INTERNAL(upper+1,begin_old_heap,marked) ) upper++
#else
#define FIND_UPPER_LIMIT \
upper=lower; \
while(COMPOUND(*upper)) upper+=GETARITY(*upper)
#endif

#ifdef DEBUG_GC
#define FIND_BLOCK \
     FIND_LOWER_LIMIT; \
     fprintf(copy_info,"\t\tlower--->Heap[%d]\n", HP(lower)); \
     FIND_UPPER_LIMIT; \
     fprintf(copy_info,"\t\tupper--->Heap[%d]\n", HP(upper)); \
     if(lower<hpptr) blocks_internal_referenced++; \
     blocks_total++
#else
#define FIND_BLOCK \
     FIND_LOWER_LIMIT; \
     FIND_UPPER_LIMIT;
#endif

#ifdef DEBUG_GC
#define FORWARD_BLOCK(beginptr, endptr, destptr) \
for(srcptr=beginptr; srcptr<=endptr; srcptr++) \
{ \
    DEBUG2(copy_info, "\t\tforwarding Heap[%d] to Extra[%d]\n", \
	  HP(srcptr), EX(destptr)); \
    if(!REACHABLE(srcptr,begin_old_heap,marked)) \
    { \
	fprintf(copy_info, "Error : want to forward but not marked !!!\n"); \
	fflush(copy_info); \
	EXIT0("Error in GC : look at bottom of info.copy !!!\n");  \
    } \
    if(FORWARDED(srcptr,begin_new_heap,end_new_heap,begin_old_heap)) \
    { \
	fprintf(copy_info, "Error : want to forward but allready !!!\n"); \
	fflush(copy_info); \
	EXIT0("Error in GC : look at bottom of info.copy !!!\n");  \
    } \
    UNMARK_REACHABLE(srcptr,begin_old_heap,marked); \
    MARK_FORWARDED(srcptr,begin_old_heap); \
    *(destptr) = *(srcptr); \
    *(srcptr) = (cell)(destptr); \
    (destptr)++; \
    num_forwarded++; \
}
#else
#define FORWARD_BLOCK(beginptr, endptr, destptr) \
for(srcptr=beginptr; srcptr<=endptr; srcptr++) \
{ \
    MARK_FORWARDED(srcptr,begin_old_heap); \
    UNMARK_REACHABLE(srcptr,begin_old_heap,marked); \
    *(destptr) = *(srcptr); \
    *(srcptr) = (cell)(destptr); \
    (destptr)++; \
}
#endif


#ifdef DEBUG_GC

static char empty[64];

static char *SAFE_NAME(cell C)
{ no i ;

	i = GETSYMNO(C) ;
	if (((bp_long)i < 0) || (i >= MAXATOM) || !atomtable[i]) {  
            sprintf(empty,"??%xu",(cell)C);
            return(empty) ;
        }
	return(atomtable[i]) ;

} /* SAFE_NAME */

static void print_cell(FILE *fileptr, cell C, stack wam)
{ term begin_old_heap = HP_BASE ;

  if(VAR(C)) {
    if(C>=(cell)wam[HeapStk].base && C<(cell)wam[HeapStk].end) 
      fprintf(fileptr, "VAR ->Heap[%d]\n", HP(C));
    else if(TO_CODE(C)) 
      fprintf(fileptr, "Code PTR ->%d\n", C);
    else {
      fprintf(fileptr, "BAD VAR:%ld at htable[%ld]\n",(bp_long)C,htable-(hentry)(C));
    }
  }
  else
    if(INTEGER(C))
      fprintf(fileptr, "INT %ld\n", C>>TAGBITS);
    else
      if(IDENTIFIER(C))
	{
	  if(GETARITY(C))
	    fprintf(fileptr, "FUNCTOR %s/%ld\n", SAFE_NAME(C), GETARITY(C));
	  else
	    fprintf(fileptr, "ATOM %s\n", SAFE_NAME(C) );
	}
      else
	fprintf(fileptr, "???? | TAG=%ld\n", GETTAG(C));
} /* print_cell */


static void display_trail_stack_structure(stack wam, FILE *fileptr)
{
  fprintf(fileptr, "trail-stack structure\n");
  line(fileptr);

  fprintf(fileptr, "size = %ld\n", wam[TrailStk].size);
  fprintf(fileptr, "over = %ld\n", wam[TrailStk].over);
  fprintf(fileptr, "top = %ld\n", (cell)TR_GCTOP);
  fprintf(fileptr, "oldtop = %ld\n", (cell)wam[TrailStk].oldtop);
  fprintf(fileptr, "base = %ld\n", (cell)TR_BASE);
  fprintf(fileptr, "margin = %ld\n", (cell)wam[TrailStk].margin);
  fprintf(fileptr, "end = %ld\n", (cell)TR_END);
  fprintf(fileptr, "maxused = %ld\n", (cell)wam[TrailStk].maxused);
  fprintf(fileptr, "name = %s\n", wam[TrailStk].name);

  fprintf(fileptr, "\nThe End\n");
} /* display_trail_stack_structure */

static void print_copy_cell(FILE *fileptr, cell C)
{
  if(VAR(C))
    fprintf(fileptr, "VAR ->Extra[%d]\n", EX(C));
  else
    if(INTEGER(C))
      fprintf(fileptr, "INT %ld\n", C>>TAGBITS);
    else
      if(IDENTIFIER(C))
	{
	  if(GETARITY(C))
	    fprintf(fileptr, "FUNCTOR %s/%ld\n", SAFE_NAME(C), GETARITY(C));
	  else
	    fprintf(fileptr, "ATOM %s\n", SAFE_NAME(C) );
	}
      else
	fprintf(fileptr, "???? | TAG=%ld\n", GETTAG(C));
} /* print_copy_cell */


static void print_choice(char *filename, term H, stack wam, term *A)
{
  FILE *info;
  term chptr;
  bp_long count=0;
  term begin_old_heap = HP_BASE ;
  
  if(!(info=fopen(filename, "w")))
    EXIT1("Cannot open file : %s\n", filename);

  
  line(info) ;
  for(count=0; count<=cpoints; count++)
    {
      for(chptr=cpoint[count].start;
	  chptr<cpoint[count].start+cpoint[count].arity; chptr++)
	{
	  fprintf(info, "Choice[%d] : %10ld : ", CH(chptr), (cell)(*chptr));
	  print_cell(info, (cell)(*chptr),wam);
	}
      fprintf(info, "Choice[%d] : %10ld : ", CH(chptr), (cell)(*chptr));
      fprintf(info, "H ----------------->Heap[%d]\n", HP(*chptr));
      chptr++;
      fprintf(info, "Choice[%d] : %10ld : ", CH(chptr), (cell)(*chptr));
      fprintf(info, "TR ---------------->Trail[%d]\n",
	      TRGC(*chptr));
      chptr++;
      fprintf(info, "Choice[%d] : %10ld : ", CH(chptr), (cell)(*chptr));
      fprintf(info, "P ----------------->Code[%d]\n", CD(*chptr));
      
      line(info) ;
    }
  fprintf(info, "\nThe End\n");
  fclose(info);
} /* print_choice */


static void print_heap(char *filename, term H, stack wam, term *A)
{
  FILE *info;
  term hpptr;
  bp_long count=0;
  term begin_old_heap = HP_BASE;
  
  if(!(info=fopen(filename, "w")))
    EXIT1("Cannot open file : %s\n", filename) ;

  fprintf(info, "Heap[0] at address %ld\n", (cell)begin_old_heap);
  fprintf(info, "Heap[%d] at address %ld\n\n", HP(H), (cell)H);

  
  line(info) ;
  for(hpptr=begin_old_heap; hpptr<H; hpptr++)
    {
      if(count<=cpoints)
	while((cell)hpptr==(cell)(*cpoint[count].H))
	  { line(info) ;
	    if(count<cpoints) count++;
	    else break;
	  }
      fprintf(info, "Heap[%d] : %12ld : ", HP(hpptr), (cell)(*hpptr));
      fprintf(info, " %s ", REACHABLE(hpptr,begin_old_heap,marked) ? "(r)" : "( )");
      fprintf(info, " %s ", INTERNAL(hpptr,begin_old_heap,marked) ? "(i)" : "( )");
      if(SELFREF(hpptr))
	fprintf(info, "SELFREF\n");
      else
	print_cell(info, (cell)(*hpptr),wam);
    }
  fprintf(info, "\nThe End\n");
  fclose(info);
} /* print_heap */



static void print_trail(char *filename, term H, stack wam, term *A, bp_long switched)
{
  FILE *info;
  term trptr, refptr;
  cell val;
  bp_long count=0;
  bp_long off1, off2 ;

  if (switched) off1 = 0 ; else off1 = 1 ;
  off2 = 1 - off1 ;

  if(!(info=fopen(filename, "w"))) EXIT1("Cannot open file : %s\n", filename);

  for(trptr=TR_BASE; trptr<TR_GCTOP; trptr++)
    {
      if(count<=cpoints)
	while((cell)trptr==(cell)(*cpoint[count].TR))
	  { line(info) ;
	    if(count<cpoints) count++;
	    else break;
	  }

	if(!(*trptr)) fprintf(info, "NULL\n");
	else
	if(pvtrailentry(trptr+off1)&&(trptr!=(term)(TR_GCTOP-1)))
		{ val = *(trptr+off2);
		  refptr = (term)(*(trptr+off1) - 1);

		  fprintf(info, "VTrail[%d] : %12ld : value = ",TRGC(trptr), *(trptr+off2));
		  print_cell(info, val,wam);
		  if(TO_HEAP(refptr))
			{ fprintf(info, "RTrail[%d] : %12ld : trails Heap[%d] : ",
				TRGC(trptr+1), (cell)(*(trptr+1)), HP(refptr));
			  print_cell(info, *refptr,wam);
			}
		  else
			{ fprintf(info, "RTrail[%d] : %12ld : trails ????[%ld] : ",
				TRGC(trptr+1), (cell)(*(trptr+1)), (cell)refptr);
			  print_cell(info, *refptr,wam);
			}
		  trptr++;
		}
	else
	if(TO_HEAP(*trptr))
	    { fprintf(info, "Trail[%d] : %12ld : trails Heap[%d] : ",
		      TRGC(trptr), (cell)(*trptr), HP(*trptr));
	      print_cell(info, **((term *)trptr),wam );
	    }
	else if(ON_HTABLE(*trptr))
	{ bp_long i ;
	  hentry v=(hentry)(*trptr);
	  i = v-htable ;
	 	if(v->val)
		{ fprintf(info, "Trail[%ld] : %12ld : trails Hash Table[%ld] points to Heap[%ld] : ",
		    TRGC(trptr), (cell)(*trptr), i,HP(v->val)) ;
        { term t=(term)(v->val);
		      print_cell(info, (cell)t, wam );
        }
		}
	  else
	    fprintf(info, "Trail[%d] : %12ld : trails ????[%ld]\n",
		    TRGC(trptr), (cell)(*trptr), (cell)(*trptr));
	}
    }
  fprintf(info, "\nThe End\n");
  fclose(info);
} /* print_trail */


static void print_regs(char *filename, term H, stack wam, term *A, term regs)
{
  bp_long i;
  FILE *info;
  
  if(!(info=fopen(filename, "w")))
      EXIT1( "Cannot open file : %s\n", filename);

  for(i=-10; i<=MAXREG; i++)
    {
      fprintf(info, "regs[%d] : ", i);
      print_cell(info, regs[i],wam);
    }
  fprintf(info, "\nThe End\n");
  fclose(info);
} /* print_regs */


static void print_extra(char *filename, term next, stack wam)
{
  FILE  *info;
  term extraptr;
  bp_long count=0;
  
  if(!(info=fopen(filename, "w")))
    EXIT0("Cannot open file : extra_heap\n");

  fprintf(info, "extra_heap begins at %ld\n\n", (cell)begin_new_heap);
  
  line(info) ;
  for(extraptr=begin_new_heap; extraptr<next; extraptr++)
    {
      if(count<=cpoints)
	while((cell)extraptr==(cell)(*cpoint[count].H))
	  {
	    
	    line(info) ;
	    if(count<cpoints)
	      count++;
	    else
	      break;
	  }
      fprintf(info, "Extra[%d] : %10ld : ", EX(extraptr),(cell)(*extraptr));
      if(SELFREF(extraptr))
	fprintf(info, "SELFREF\n");
      else
	if(TO_EXTRA(*extraptr))
	  fprintf(info, "VAR --> Extra[%d]\n", EX((term)*extraptr));
	else
	  print_cell(info, *extraptr,wam);
    }

  fclose(info);
} /* print_extra */


static void print_copy_extra(char *filename, term H, stack wam, term *A, bp_long offset)
{
  FILE *info;
  term hpptr;
  long count=0;
  
  if(!(info=fopen(filename, "w")))
    EXIT1("Cannot open file : %s\n", filename);

  fprintf(info, "Extra[0] at address %ld\n", (cell)begin_new_heap);
  fprintf(info, "Extra[%d] at address %ld\n\n", EX(end_new_heap),
	  (cell)end_new_heap);

  
  line(info) ;
  for(hpptr=begin_new_heap; hpptr<H; hpptr++)
    {
      if(count<=cpoints)
	while((cell)hpptr==(cell)(*cpoint[count].H))
	  {
	    
	    line(info) ;
	    if(count<cpoints)
	      count++;
	    else
	      break;
	  }
      fprintf(info, "Extra[%d] : %12ld : ", EX(hpptr), *hpptr);
      if(VAR(*hpptr))
	{
	  fprintf(info, "(already adjusted) ");
	  if((cell)hpptr==(cell)((term)(*hpptr)-offset))
	    fprintf(info, "SELFREF\n");
	  else
	    print_copy_cell(info, (cell)((term)(*hpptr)-offset));
	}
      else
	{
	  print_copy_cell(info, *hpptr);
	}
    }
  fprintf(info, "\nThe End\n");
  fclose(info);
} /* print_copy_extra */

#endif

