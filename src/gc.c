/* Originally developed by Gert Engels and Bart Demoen */
/* Modified and extended by Bart Demoen and Paul Tarau */
/* see trigger_gc - bottom of the file - only function exported tho engine.c */

#define FIRSTREG 1
#define ON_DCGS(PTR) (PTR>=DCGSTART() && PTR<DCGSTART()+MAXDCG)
#define ON_HTABLE(PTR) ((hentry)(PTR)>=htable && (hentry)(PTR)<htable+HMAX)
#define TO_CODE(PTR) (((term*)PTR)>=g.shared[InstrStk].base && ((term*)PTR)<g.shared[InstrStk].margin)
#define FIXGC 0

#define GC 1

/*
#define DEBUG_GC
#define PROFILE_GC
*/

#include "global.h"
extern void overflow_by(term *Top, bp_long LimitNo, stack wam, string culprit);

#if GC>0

#include <string.h>
#include "gc.h"

extern void warnmes(char *mes);
extern bp_long cputime (void);
extern no make_stack (stack s, bp_long n, bp_long check, 
       string name, term *area, no notzeroed);
extern void quietmes (string format, string mes);
extern void debugmes (string format, string mes);

static void gc_mes(bp_long tolerance) {  
  char lline[255];
  sprintf(lline, "%s, tolerance=%ld (bytes)\n %s\n",
    "*** Not enough memory recovered during GC",
    tolerance, 
    "*** Please start with more heap, using option -h"
  );
  fprintf(STD_err,"%s",lline);
  fflush(STD_err);
}

static term gc_check(term H, stack wam, bp_long gc_time)
{
  bp_long tolerance = wam[HeapStk].margin-wam[HeapStk].base;
       tolerance = tolerance >> 1; /* 1/4 crashes => 1/2 of existing space */

  g.gctime += gc_time;

  if(H+tolerance >= (term)wam[HeapStk].margin)
    {
      gc_mes(tolerance);
      return NULL;
    }
  return H;
}

static bp_long init_cps(term *A, stack wam, cp *cpoint)
{
  bp_long count=0, arity=0;
  term chptr=CH_BASE;

  while(chptr<=(term)A)
    {
      if(((term)(*(chptr+1))>=TR_BASE)
	 && ((term)(*(chptr+1))<=TR_GCTOP) && (VAR(((term)(*(chptr+1))))))
	{
	  cpoint[count].H = chptr++;
	  cpoint[count].TR = chptr++;
	  cpoint[count++].arity = arity;
	  arity=0;
	  cpoint[count].start = ++chptr;
	}
      else
	{
	  arity++;
	  chptr++;
	}
    }
  count = count-1;
  
  /* adjust for the first choice-point */
  cpoint[0].arity--;
  cpoint[0].start = CH_BASE+1;
  return count ;
} /* init_cps */

#ifdef MIMICK_LOOSE_SEGMENT_ORDER
static void setallHBtoH(H, A, wam)
term H ;
term *A ;
stack wam ;
{
  term chptr=CH_BASE;

  while(chptr<=(term)A)
    {
      if(((term)(*(chptr+1))>=TR_BASE)
	 && ((term)(*(chptr+1))<=TR_GCTOP) && (VAR(((term)(*(chptr+1))))))
	{
	  *chptr = (cell)H ;
	  chptr += 3 ;
	}
      else
	{
	  chptr++;
	}
    }
  
} /* setallHBtoH */
#endif

#ifdef DEBUG_GC
#define mark_cell(a,b,c) mark_cell1(a,b,c,wam,begin_old_heap,H)
static bp_long mark_cell1(tomark, heapstart, marked, wam,begin_old_heap,H)
term tomark, heapstart,begin_old_heap,H ;
char *marked ;
stack wam ;
#else
#define mark_cell(a,b,c) mark_cell1(a,b,c,begin_old_heap,H)
static bp_long mark_cell1(term tomark, term heapstart, char *marked,
term begin_old_heap,term H)
#endif
{ /* begin mark_cell1 */
  register bp_long arity;
  register cell star_tomark ;
  register bp_long m = 0 ;
  register char *pmark ;

label_last_call_opt1:

if(! TO_HEAP(((cell)tomark))) 
	{ DEBUG1(mark_info, "\tbut not to heap %d", 0); return(0) ; } 

  if(!REACHABLE(tomark,heapstart,marked))
    {
      pmark = marked + (tomark-heapstart) ;
      *pmark = REACHABLE_TAG ;

      MARK_REACHABLE(tomark,heapstart,marked);

label_last_call_opt2:
      DEBUG1(mark_info, "\tmarking Heap[%d] : ", HP(tomark));
      DEBUG_CELL(mark_info, *((term)(tomark)),wam);

      if(! TO_HEAP(((cell)tomark))) { 
        DEBUG1(mark_info, "\tbut not to heap %d", 0); return(m) ; } 
      m++;
      star_tomark = *tomark ;
      if( VAR(star_tomark)) {
	      if (TO_HEAP(star_tomark)) {
          if (tomark == (term)star_tomark) { return(m);}
	        tomark = (term)star_tomark ;
	        goto label_last_call_opt1 ;
        }
	        else return(m) ; /* i.e. treat as IDENTIFIER with arity == 0 */
      }
      if(IDENTIFIER(star_tomark)) {
	      if (! (arity = GETARITY(star_tomark))) { return(m) ;}
	      while(--arity) { 
          m += mark_cell(++tomark,heapstart,marked);
	        *(++pmark) = REACHABLE_INTERNAL_TAG ;
	          MARK_INTERNAL(tomark,heapstart,marked);
        }
        ++tomark; pmark++;
	  if (*pmark)

	  if (REACHABLE(tomark,heapstart,marked))

	  	{ MARK_INTERNAL(tomark,heapstart,marked);
	  	  *pmark = REACHABLE_INTERNAL_TAG ;
	  	  return(m) ;
	  	}
	  *pmark = REACHABLE_INTERNAL_TAG ;

	  MARK_INTERNAL(tomark,heapstart,marked) ;

	  goto label_last_call_opt2 ;
	}
    return(m) ;
    }
  else {
      DEBUG1(mark_info, "\tdone Heap[%d] : ", HP(tomark));
      DEBUG_CELL(mark_info, *((term)(tomark)),wam);
      return(m) ;
    }
    
} /* mark_cell */


static bp_long mark_and_early_reset(bp_long arity, term *A, stack wam, term regs, term H, 
                                 bp_long cpoints, cp *cpoint, term begin_old_heap, char *marked) {
  bp_long count, i, trail_cells_deleted=0, num_marked = 0;
#ifdef DEBUG_GC
  bp_long num_skipped=0 ;
#endif
  term trptr, free_trptr, chptr, refptr;
  term begintr, endtr;
  
  DEBUG0(mark_info, "MARK THE REACHABLE CELLS ...\n\n");
  DEBUG1(mark_info, "CALCULATED ARITY : %d\n\n", arity);
  DEBUG0(mark_info, "MARK FROM REGISTERS\n");
  
  /** should be 0? some data is in regs[0]? $$BUG??? */
  for(i=FIRSTREG; i<=arity; i++) {
    DEBUG1(mark_info, "   MARK FROM REGISTER %d\n", i);
    if(VAR(regs[i]))
      num_marked += mark_cell((term)(regs[i]),begin_old_heap,marked);
    else {
      DEBUG0(mark_info, "\tno variable : ");
      DEBUG_CELL(mark_info, regs[i],wam);
    }
  }
  
  DEBUG0(mark_info, "\nMARK FROM CHOICE-POINTS\n");
  
  /* first mark all lval entries */ 
  DEBUG0(mark_info, "   MARK lval entries first\n");
  begintr = TR_GCTOP-1 ;
  endtr = (term)(*cpoint[0].TR) ;
  /* the following should be made conditional: if there are any trailed hash entries ...*/
  for( trptr = begintr ; trptr >= endtr ; trptr-- ) { 
    if (pvtrailentry(trptr)) { trptr-- ; continue ; }
    if(TO_HEAP((*trptr)) || TO_CODE((*trptr))) continue ;
    /* else it should point to the hash table */
    /* test this and then start marking from *trptr as a normal root */
    { 
    hentry hptr =(hentry)(*trptr) ;
    cell val=(cell)hptr->val;
    if(ON_HTABLE(hptr) && val) {  
      DEBUG1(mark_info, "     mark from Hash Table[%d] : ", hptr-htable );
      DEBUG_CELL(mark_info, val, wam );
      if(VAR(val) && TO_HEAP(val)) {
		      num_marked +=
            mark_cell((term)val,begin_old_heap,marked);
      } 
    } /* end if: hash table entry */
    }
  }
  
#if FIXGC>0
  /* AG-DCG registers $$$ */
  for( trptr = begintr ; trptr >= endtr ; trptr-- ) { 
    if(!pvtrailentry(trptr)) continue;
    { term t = (term)(*trptr);
    if(ON_DCGS(t)) {
		    /* num_marked +=
        * mark_cell(t,begin_old_heap,marked);
        */
      fprintf(STD_err,"trail[%ld]->%ld\n",TRGC(trptr),t-(term)wam[HeapStk].base);
    }
    }
  }
#endif
  
  for(count=cpoints; count>=0; count--) {
    term current_H ;
    DEBUG1(mark_info, "   SCAN TRAIL SEGMENT YOUNGER THAN CHOICE-POINT %d\n",count);
    current_H = (term)(*cpoint[count].H) ;
    begintr = ((count==cpoints)?TR_GCTOP-1:(((term)(*cpoint[count+1].TR))-1)) ;
    endtr = (term)(*cpoint[count].TR) ;
    
    for ( trptr = begintr ; trptr >= endtr ; trptr-- ) {
      if (pvtrailentry(trptr) /*&& TO_HEAP(*trptr)*/) { /* begin value trail */
        cell temp, val;
        /* switch ref and value on trail */
        refptr = (term)(*trptr) ;
               
        *trptr = val = *(trptr-1);
        *(trptr-1) = (cell)refptr ;
        /* refptr = (term)((cell)refptr & ~ (cell)TR_VTAG);  $$$PT */
        refptr = (term)((cell)refptr-TR_VTAG);
#ifdef DEBUG_GC
        if(! TO_HEAP(refptr)) 
          DEBUG2(mark_info,"\tRTrail[%d] -> (?) ????[%ld]\n",TRGC(trptr), (cell)refptr);
#endif
        /* early reset possible ? */
        
        /* DLL BUG here - switching this off does not help !!! $$$ */
        
        if(!REACHABLE(refptr,begin_old_heap,marked)) { /* $$$ */
          DEBUG2(mark_info,"\tRTrail[%d] -> (?) ????[%ld] unreachable\n",TRGC(trptr), (cell)refptr);
          DEBUG0(mark_info, "\t\tearly reset of value trail\n");
		      *refptr = val;
          *trptr=(cell)NULL;
          trptr-- ;
          *trptr=(cell)NULL;
          trail_cells_deleted+=2;
        }
        else if (refptr >= current_H) /* check if can be tidied */ { 
          *trptr=(cell)NULL ;
          trptr-- ;
          *trptr=(cell)NULL;
          trail_cells_deleted +=2 ;
        } else { /* $$$ restore the state before value trailing */
          bp_long was_internal ;
          was_internal = INTERNAL(refptr,begin_old_heap,marked) ;
          DEBUG0(mark_info,"\t\trestoring state before value trailing\n");
          temp = *refptr;
          *refptr = val ;
          *trptr = temp;
          if(COMPOUND(val) || VAR((term)val)) { 
            DEBUG0(mark_info, "\t\trestored value is a functor or var\n");
            UNMARK_REACHABLE(refptr,begin_old_heap,marked);
            num_marked--;
            num_marked += mark_cell(refptr,begin_old_heap,marked);
          }
          if (was_internal) MARK_INTERNAL(refptr,begin_old_heap,marked);
          trptr--;
        }
      } /* end value trail entry */
      else if TO_HEAP(*trptr) {
        DEBUG3(mark_info, "\t Trail[%d] -> %s Heap[%d] : ",TRGC(trptr), 
          REACHABLE((term)*trptr,begin_old_heap,marked) ? "(r)" : "( )",HP(*trptr));
        DEBUG_CELL(mark_info, **(term *)trptr,wam);
        
        /* early reset possible ? */
        if(!REACHABLE((term)(*trptr),begin_old_heap,marked)) { 
          DEBUG0(mark_info, "\t\tearly reset ordinary trail\n");
          *(*(term *)trptr) = *trptr;
          *trptr=(cell)NULL;
          trail_cells_deleted++;
        }
        /* check if can be tidied */
        else if ((term)(*trptr) >= current_H) { 
          *trptr=(cell)NULL ;
          trail_cells_deleted++ ;
        }
      } /* end ordinary trail entry */
      else { 
        /* it should point to the hash table */
        /* it has been marked already */
        /* quietmes("hash entry in gc %s\n","found"); */
      }
    } /* end for */
    
    
    DEBUG1(mark_info, "   MARK FROM CHOICE-POINT %d\n", count);
    trptr = cpoint[count].start+cpoint[count].arity ;
    
    
    /* VCC GC BUG starts here */
    
    for(chptr=cpoint[count].start; chptr<trptr; chptr++) {
	     DEBUG1(mark_info, "     mark from Choice[%d] : ", CH(chptr) );
       DEBUG_CELL(mark_info, (cell)(*chptr), wam );
       if(VAR(*chptr))
	        num_marked += mark_cell((term)(*chptr),begin_old_heap,marked);
	   }
  } /* end of choice point "for" */
  
  DEBUG1(mark_info, "\nCOMPACT THE TRAIL (%d entries to delete)\n",trail_cells_deleted);  
  if(trail_cells_deleted>0) {
#ifdef DEBUG_GC
    print_trail("before.compacting.trail", H, wam, A,1);
#endif
    /* search for the first NULL entry (there is at least one) */
    trptr=TR_BASE;
    while(*trptr)
      trptr++;
    
    DEBUG1(mark_info, "\tThe first NULL entry on trail : Trail[%d]\n",TRGC(trptr));
    /* find the oldest younger choice-point (if there is one) */
    
    for(count=0; count<=cpoints; count++) 
      if((term)(*cpoint[count].TR)>trptr) break;
      
      if((count<cpoints)||((count==cpoints) && ((term)(*cpoint[count].TR)>trptr))) { 
        DEBUG1(mark_info,"\toldest younger choice-point : Choice[%d]\n",CH(cpoint[count].TR));
        free_trptr = trptr;
        trptr++;
        for(i=count; i<=cpoints; i++) { 
          while((term)(*cpoint[i].TR)>trptr) { 
            if(*trptr)
              *(free_trptr++) = *trptr;
            else { 
              DEBUG1(mark_info, "\tSkipping NULL entry on Trail[%d]\n",TRGC(trptr));
#ifdef DEBUG_GC
              num_skipped++;
#endif
            }
            trptr++;
          }
          *cpoint[i].TR=(cell)free_trptr;
        }
      } else { 
        DEBUG0(mark_info, "\tThere is no younger choice-point\n");
        free_trptr = trptr;
        trptr++;
      }
      while(trptr<TR_GCTOP) { 
        if(*trptr)
          *(free_trptr++) = *trptr;
        else { 
          DEBUG1(mark_info, "\tSkipping NULL entry on Trail[%d]\n",TRGC(trptr));
#ifdef DEBUG_GC
          num_skipped++;
#endif
        }
        trptr++;
      }
      SET_TR_GCTOP(free_trptr);
      DEBUG1(mark_info, "TRAIL COMPACTED : deleted %d NULL entries\n",num_skipped+1);
  }
  DEBUG1(mark_info, "\nMARK PHASE DONE !! (%d cells marked)\n\n", num_marked);
  DEBUG_CLOSE(mark_info);
  return(num_marked) ;
  
} /* mark_and_early_reset */

static term scan_forwarded(term scan, term next, bp_long offset, 
                           term H, stack wam, term segment_boundary, term begin_old_heap, 
                           term begin_new_heap, term end_new_heap, char *marked)
{
  term hpptr, lower, upper, srcptr;
  cell starscan ;
  
  DEBUG2(copy_info, "\tscan from Extra[%d] to Extra[%d]\n", EX(scan),EX(next-1));
  while(scan<next)
  {
    DEBUG1(copy_info, "\t\tscan -> Extra[%d] : ", EX(scan));
    starscan = *scan ;
    if(VAR(starscan))
    {
#if 1 /* AS NOT ALL VARS ARE ON THE HEAP!!! - Paul */
      if(TO_HEAP(starscan))
#endif
      {
        DEBUG_CELL(copy_info, starscan,wam);
        if((term)(starscan) < segment_boundary)
        {
          DEBUG0(copy_info, "\t\tintra- or backward-segment ");
          hpptr = (term)(starscan);
          if(!FORWARDED(hpptr,begin_new_heap,end_new_heap,begin_old_heap))
          {
            DEBUG0(copy_info, "and not forwarded yet\n");
            FIND_BLOCK;
            FORWARD_BLOCK(lower, upper, next);
          }
          else DEBUG0(copy_info, "but already forwarded\n");
          DEBUG2(copy_info, "\t\tredirect Extra[%d] to Extra[%d]\n",
            EX(scan), EX((**(term **)scan)));
          *scan = (cell)((*(term *)starscan)+offset);
        } /* an intra- or backward-segment heap reference */
        else
        {
          DEBUG0(copy_info,"\t\ta forward-segment heap reference : wait\n");
          DEBUG1(copy_info,"\t\theapcell -> Heap[%d] should be trailed !?!\n",
            HP((term)(*scan)));
        } /* a forward-segment heap reference */
      } /* *scan points in heap */
#if 1
      else
      {
        if(TO_EXTRA(*scan))
        { DEBUG1(copy_info, "VAR -> Extra[%d]\n", EX(*scan));
		      if((term)(*scan)==scan)
          { DEBUG0(copy_info, "\t\tit's an undef : adjust\n");
          *scan = (cell)((term)(*scan) + offset);
          }
          else DEBUG0(copy_info,"\t\tNOT an undef but points in extra ????\n");
        } /* *scan points in extra */
        else DEBUG0(copy_info, "does NOT point in heap or extra ????\n");
      }
#endif
    } /* *scan is a variable */
    else DEBUG0(copy_info, "not a variable\n");
    scan++;
  }
  return(next) ;
} /* scan_forwarded */




static term copy_to_extra_heap(bp_long arity, term H, stack wam, term regs, 
                               bp_long max_num_choice_points, term begin_new_heap, term end_new_heap, 
                               bp_long cpoints, cp *cpoint, term begin_old_heap, char *marked) {
  term chptr, hpptr, lower, upper, scan, next, trptr, *extra_segment_end;
  term begintr, endtr;
  bp_long count, offset, reg_i, a_value_trail_entry ;
  term srcptr, current_H;
  
#ifdef DEBUG_GC
  if(!(copy_info=fopen("info.copy", "w")))
    EXIT0("Cannot open file : info.copy\n");
  num_forwarded = blocks_internal_referenced = blocks_total = 0;
#endif
  
  if(!(extra_segment_end=TALLOC(max_num_choice_points )))
    EXIT0("Cannot allocate the extra_segment_end array\n");
  offset = begin_old_heap-begin_new_heap;
  
  scan = begin_new_heap ;
  
  DEBUG0(copy_info, "COPY TO EXTRA HEAP ...\n\n");
  next = begin_new_heap;  
  DEBUG0(copy_info, "DO CHOICE-POINTS (bottom-up)\n");
  for( count=0; count<=cpoints; count++) {
    current_H = (term)(*cpoint[count].H) ;
    DEBUG1(copy_info, "   DO CHOICE-POINT %d\n", count);
    endtr = cpoint[count].start+cpoint[count].arity ;
    for(chptr=cpoint[count].start; chptr<endtr; chptr++) {
      DEBUG1(copy_info, "\tconsidering Choice[%d] : ", CH(chptr));
      if(VAR(*chptr)&&TO_HEAP(*chptr)) {
        DEBUG1(copy_info, "root to Heap[%d] ", HP(*chptr));
        hpptr = (term)(*chptr);
        if(!FORWARDED(hpptr,begin_new_heap,end_new_heap,begin_old_heap)) {
          DEBUG0(copy_info, "and not forwarded yet\n");
          FIND_BLOCK;
          FORWARD_BLOCK(lower, upper, next);
        }
        else
          DEBUG0(copy_info, "but already forwarded\n");
        DEBUG2(copy_info, "\t\tredirect Choice[%d] to Extra[%d]\n",
          CH(chptr), EX(**(term *)chptr));
        *chptr = (cell)((term)(**(term *)chptr) + offset);
      }
      else {
        DEBUG0(copy_info, "no root-pointer : ");
        DEBUG_CELL(copy_info, *chptr, wam);
      }
    }
    
    { next = scan_forwarded(scan,next,offset,H,wam,current_H,
      begin_old_heap,begin_new_heap,end_new_heap,marked);
    scan = next ;
    }
    
    
    DEBUG1(copy_info, "   CLOSE BACKTRACK SEGMENT %d\n", count);
    extra_segment_end[count] = next;
    
    DEBUG1(copy_info,"SCAN TRAIL SEGMENT YOUNGER THAN CHOICE-POINT %d\n",count+1);
    begintr = (term)(*cpoint[count].TR) ;
    endtr = ((count==cpoints)?TR_GCTOP:(term)(*cpoint[count+1].TR)) ;
    current_H = ((count==cpoints)?H:(term)(*cpoint[count+1].H)) ;
    a_value_trail_entry = 0 ;
    for(trptr = begintr ; trptr < endtr ; trptr++) {	
      DEBUG1(copy_info, "\tconsider Trail[%d] : ", TRGC(trptr));
      if( pvtrailentry(trptr) ) { 
        a_value_trail_entry = TR_VTAG ;
        /* *trptr = ((*trptr) & ~ TR_VTAG) ; $$$PT */
        *trptr = ((*trptr) - TR_VTAG) ;
        DEBUG1(copy_info, "\tconsider Value Trail[%d] : ", TRGC(trptr));
#if FIXGC>0
        if(ON_DCGS((term)*trptr)) {
          fprintf(STD_err,"ON_DCGS copy_to_extra_segment=%ld\n",(bp_long)((term)(*trptr)-DCGSTART()));
        }
#endif
        goto treat_ordinary_trail_entry ;
      } /* trptr points to a value trail */
      else if(ON_HTABLE(*trptr)) { 
        /* it should point to the hash table */
        /* test this and then start copying from *trptr as a normal root */
       
        hentry phte = (hentry)(*trptr);
        if (phte->val) { 
          DEBUG1(copy_info, "     copy from Hash Table[%d] : ", phte-htable );
          DEBUG_CELL(copy_info, phte->val, wam );
          hpptr=(term)phte->val;
          if (! FORWARDED(hpptr,begin_new_heap,end_new_heap,begin_old_heap)) { 
            FIND_BLOCK;
            FORWARD_BLOCK(lower, upper, next);
          }
          phte->val = (cell)(  ((term)(*hpptr)) + offset) ;
        } /* pointer to trailed hash entry */
      }
      else {
        
treat_ordinary_trail_entry:
      
      if(FORWARDED((term)(*trptr),begin_new_heap,end_new_heap,begin_old_heap)) { 
        term newheappointer, oldheappointer ;
        DEBUG1(copy_info,"points to Heap[%d] : already forwarded\n",HP(*trptr));
        DEBUG2(copy_info,
          "\t\tredirect Trail[%d] to Extra[%d]\n",TRGC(trptr), EX(**(term *)trptr));
        oldheappointer = (term)(*trptr) ;
        *trptr = **(term *)trptr;
        newheappointer = (term)(*trptr) ;
        hpptr = (term)(* newheappointer) ;
        
        if(VAR(hpptr) && (!a_value_trail_entry)) {
#ifdef DEBUG_GC
          if(TO_EXTRA(hpptr)) { 
            DEBUG2(copy_info,"\t\t?done? Extra[%d] -> Extra[%d]\n",
              EX((term)(*trptr)),EX(hpptr-offset));
            /* this is an error ? */
          }
          else
#endif
          { if(hpptr >= oldheappointer) { 
            DEBUG2(copy_info, "\t\tExtra[%d] -> Heap[%d] ",
              EX((term)(*trptr)),HP((term)(**(term *)trptr)));
            if(FORWARDED(hpptr,begin_new_heap,end_new_heap,begin_old_heap)) {
              DEBUG0(copy_info, "but already forwarded\n");}
            else { 
              DEBUG0(copy_info, "and not forwarded yet\n");
              FIND_BLOCK;
              FORWARD_BLOCK(lower, upper, next);
            }
            DEBUG2(copy_info,"\t\tredir Extra[%d] to Extra[%d]\n",
              EX(*(term *)trptr),EX(***(term ***)trptr));
            *newheappointer = (cell)((term)(*hpptr)+offset);
          } /* that extra-cell points to heap */
          else DEBUG1(copy_info,"\t\tExtra[%d] points not to newer Heap segment\n",
            EX((term)(*trptr)));
          }
        } /* that extra-cell is a variable */
        else { DEBUG1(copy_info, "\t\tExtra[%d] is not a variable\n",EX((term)(*trptr))); }
        /* now adjust trail entry */
        *trptr = (cell)((term)(*trptr) + offset) ;
      } /* *trptr is forwarded */
      else { 
        DEBUG1(copy_info,"points to Heap[%d] : not forwarded yet ?\n",HP(*trptr));
        hpptr=(term)(*trptr) ;
        FIND_BLOCK;
        FORWARD_BLOCK(lower, upper, next);
        *trptr = (cell)(  ((term)(*hpptr)) + offset) ;
      }
      if (a_value_trail_entry){ 
        cell newval ;
        term poldval ;
        a_value_trail_entry = 0 ;
        next = scan_forwarded(scan,next,offset,H,wam,current_H,
          begin_old_heap,begin_new_heap,end_new_heap,marked) ;
        scan = next ;
        poldval = ((term)(*trptr)) - offset ;
        newval = *(trptr+1) ;
        
        /* switch back tagged pointer and value */
        *(trptr+1) = (*trptr) + TR_VTAG ;
        *trptr = *poldval ;
        
        /* now the newval is treated as a register */
        /* and in the end, it is copied to the place where oldval was */
        
        DEBUG0(copy_info, "\tconsider new value\n");
        if(VAR(newval)) { 
          if(TO_HEAP(newval)) { 
            DEBUG1(copy_info, "points to Heap[%d] : ", HP(newval));
            hpptr=(term)newval;
            if(!FORWARDED(hpptr,begin_new_heap,end_new_heap,begin_old_heap)) { 
              DEBUG0(copy_info, "not forwarded yet\n");
              FIND_BLOCK;
              FORWARD_BLOCK(lower, upper, next);
            }
            else 
              DEBUG0(copy_info, "already forwarded\n");
            DEBUG1(copy_info, "\t\tredirect new value to Extra[%d]\n", EX(*(term)newval));
            newval = (cell)((term)(*(term)newval));
          } /* newval points to heap */
          else 
            DEBUG0(copy_info, "a variable that doesn't point to heap ??\n");
          newval = (cell)((term)newval+offset);
        } /* newval is a variable */
        else 
          DEBUG0(copy_info, "is not a variable\n");
        
        *poldval = newval ;
        trptr++ ; /* skip an extra trail entry */
      }
      } /* trptr points to ordinary trail entry */
      } /* stop scan next trail segment */
    } /* stop do choice-points */
    
    DEBUG0(copy_info, "DO THE REGISTERS\n");
    for(reg_i=FIRSTREG; reg_i<=arity; reg_i++) {
      DEBUG1(copy_info, "\tconsider regs[%d] : ", reg_i);
      if(VAR(regs[reg_i]))
      {
        if(TO_HEAP(regs[reg_i]))
        {
          DEBUG1(copy_info, "points to Heap[%d] : ", HP(regs[reg_i]));
          hpptr=(term)regs[reg_i];
          if(!FORWARDED(hpptr,begin_new_heap,end_new_heap,begin_old_heap))
          {
            DEBUG0(copy_info, "not forwarded yet\n");
            FIND_BLOCK;
            FORWARD_BLOCK(lower, upper, next);
          }
          else
            DEBUG0(copy_info, "already forwarded\n");
          DEBUG2(copy_info, "\t\tredirect regs[%d] to Extra[%d]\n", reg_i,
            EX(*(term)regs[reg_i]));
          regs[reg_i] = (cell)((term)(*(term)regs[reg_i]));
        } /* regs[i] points to heap */
        else
          DEBUG0(copy_info, "a variable that doesn't point to heap ??\n");
        regs[reg_i] = (cell)((term)regs[reg_i]+offset);
      } /* regs[i] is a variable */
      else
        DEBUG0(copy_info, "is not a variable\n");
    } /* stop do registers */
    
    next = scan_forwarded(scan,next,offset,H,wam,H,
                begin_old_heap,begin_new_heap,end_new_heap,marked) ;
    scan = next ;
    
    DEBUG0(copy_info, "\nADJUST THE H FIELD IN THE CHOICE-POINTS\n");
    for(count=0; count<=cpoints; count++)    {
      DEBUG2(copy_info, "\tredirect Choice[%d] to Extra[%d]\n",
        CH(cpoint[count].H), EX(extra_segment_end[count]));
      *(term *)cpoint[count].H = extra_segment_end[count] + offset ;
    }
    
    DEBUG1(copy_info, "\nCOPY PHASE DONE (%d cells forwarded)\n\n",num_forwarded);
    DEBUG1(copy_info, "#blocks forwarded : %d\n", blocks_total);
    DEBUG1(copy_info, "#blocks first referenced via internal cel : %d\n",
      blocks_internal_referenced);
    
#ifdef DEBUG_GC
    { bp_long c = 0 ;
    for(hpptr=begin_old_heap; hpptr<H; hpptr++)
      if(REACHABLE(hpptr,begin_old_heap,marked)) { 
        c++ ;
        fprintf(copy_info, 
          "\n\n Error : Heap[%d] reachable but not forwarded !!!\n",
          HP(hpptr));
        fflush(copy_info);
        fprintf(STD_err, "Error in GC : look at bottom of info.copy !!!\n");
        fflush(STD_err);
      }
      if (c) exit(-1);
    }
#endif
    
    DEBUG_CLOSE(copy_info);
    XFREE(extra_segment_end);
    
    /* adjust the real H pointer */
    if (end_new_heap != next)
      fprintf(STD_err,"different number of forwarded than space allocated\n") ;
    return next;
} /* copy_to_extra_heap */


static term gc_test0(term H, term regs, term *A, instr P, stack wam, bp_long arity)
{
  bp_long max_num_choice_points, offset, size_new_heap,gc_time;
#ifdef PROFILE_GC
  bp_long mark_time, copy_time, move_time ;
#endif
  bp_long num_marked;
#ifndef DEBUG_GC
  cp *cpoint;
  bp_long cpoints;
  term begin_new_heap, end_new_heap, begin_old_heap ;
  char *marked ;
#endif
#ifndef TRACE_GC_MEM
  bp_long size_old_heap ;
#endif
#ifdef PROFILE_GC
  static bp_long total_gc_time = 0 ;
#endif
#if defined DEBUG_GC || defined PROFILE_GC
  static bp_long nr_gcs = 0 ;
  nr_gcs++ ;
#endif

  gc_time = cputime();
  begin_old_heap = HP_BASE ;

  /* initialize some structures */
  max_num_choice_points = 3 + ((term)A-CH_BASE)/3;
  if(!(cpoint=XALLOC(max_num_choice_points,cp )))
    EXIT0("Cannot allocate the cpoint array of cp's\n");
  cpoints = init_cps(A, wam,cpoint);

  /* allocate the marked and internal bits */
  size_old_heap = H - begin_old_heap ;
  marked = (char *)(wam[MarkStk].base) ;
#ifdef DEBUG_GC
  print_choice("before.gc.choice", H, wam, A);
  print_heap("before.gc.heap", H, wam, A);
  print_trail("before.gc.trail", H, wam, A,0);
  print_regs("before.gc.regs", H, wam, A, regs);
  if(!(mark_info=fopen("info.mark", "w")))
    EXIT0("Cannot open file : info.mark\n");
#endif

  /* marking and early reset */
#ifdef PROFILE_GC
  mark_time = cputime();
#endif

  /* VCC GC BUG happens here */
  num_marked = mark_and_early_reset(arity, A, wam, regs, H,
					cpoints,cpoint,begin_old_heap,marked);
  				
  /* update the structure that describes the choice_points */
  cpoints = init_cps(A, wam,cpoint); /* actually only TR is reset here */
  
#ifdef PROFILE_GC
  mark_time = cputime() - mark_time ;
#endif
#ifdef DEBUG_GC
  print_choice("after.marking.choice", H, wam, A);
  print_heap("after.marking.heap", H, wam, A);
  print_trail("after.marking.trail", H, wam, A,1);
  print_regs("after.marking.regs", H, wam, A, regs);
#endif
  
#ifdef TRACE_GC_MEM
  /* allocate the forwarded bits and the extra heap */
  if(!(globalforwarded=ZALLOC(size_old_heap, char)))
    EXIT0("Cannot allocate the forwarded bits\n");
#endif
  size_new_heap = num_marked;
#ifdef DEBUG_GC
  if(!(begin_new_heap=ZALLOC(size_new_heap,cell)))
#else
  if(!(begin_new_heap=XALLOC(size_new_heap,cell )))
#endif
    EXIT0("Cannot allocate the begin_new_heap\n");
  end_new_heap = begin_new_heap + size_new_heap ;
  offset = begin_old_heap-begin_new_heap;

  /* copy phase */
#ifdef PROFILE_GC
  copy_time = cputime();
#endif
  H=copy_to_extra_heap(arity, H, wam, regs,max_num_choice_points,
		begin_new_heap,end_new_heap,
		cpoints,cpoint,begin_old_heap,marked);
#ifdef PROFILE_GC
  copy_time = cputime() - copy_time;
#endif

#ifdef DEBUG_GC
  print_copy_extra("after.copying.extra_heap", H, wam, A, offset);
#endif
  
  /* move back phase */
#ifdef PROFILE_GC
  move_time = cputime();
#endif
  memcpy((void *)begin_old_heap, (void *)begin_new_heap, 
				((term)H-begin_new_heap)*sizeof(cell));

  /* adjust the real H */
  H = H+offset;
#ifdef PROFILE_GC
  move_time = cputime() - move_time ;
#endif

#ifdef DEBUG_GC
  print_choice("after.gc.choice", H, wam, A);
  print_heap("after.gc.heap", H, wam, A);
  print_trail("after.gc.trail", H, wam, A,0);
  print_regs("after.gc.regs", H, wam, A, regs);
#endif

  XFREE(cpoint);
  XFREE(begin_new_heap);
#ifdef TRACE_GC_MEM
  XFREE(globalforwarded);
#endif

  gc_time = cputime() - gc_time;

  if (H != (begin_old_heap + size_new_heap))
  	{ 
  	  bp_long i = H - (begin_old_heap + size_new_heap);
  	  if (i>0)
             fprintf(STD_err,"more forwarded then marked=%ld\n",i) ;
  	  else 
             fprintf(STD_err,"more marked then forwarded=%ld\n",i) ;
  	}

#ifdef PROFILE_GC
  total_gc_time += gc_time ;
  fprintf(STD_err,"bin(before = %d, after = %d, total = %d, increment = %d, mark = %d, copy = %d, back = %d, rest = %d, gcnr = %d).\n",
  size_old_heap*sizeof(cell),
  size_new_heap*sizeof(cell),total_gc_time,gc_time,mark_time,
  copy_time,move_time,gc_time-mark_time-copy_time-move_time,nr_gcs) ;
  fflush(STD_err);
#endif


#ifdef MIMICK_LOOSE_SEGMENT_ORDER
  setallHBtoH(H,A,wam) ;
#endif

  return gc_check(H,wam,gc_time);
} /* 
 */

#define GC_SHOW_BEFORE(Top,Margin) \
  { char sbuf[255];\
    sprintf(sbuf,"before GC at: %s/%ld (left %ld bytes)\n", \
    NAME(xval),ires-1,((bp_long)(Margin)-(bp_long)(Top))); \
    debugmes("%s\n",sbuf); \
  }
#define GC_SHOW_AFTER(OldH,NewH) \
  { char sbuf[255];\
    sprintf(sbuf,"after GC at: %s/%ld (recovered %ld bytes)\n", \
    NAME(xval),ires-1,((bp_long)(OldH)-(bp_long)(NewH))); \
    debugmes("%s\n",sbuf); \
  }

static term gc_test1(register term H, register term regs, register term *A, register instr P, register stack wam, register bp_long ires)
{
  if(ZERO==wam[MarkStk].size)
  {  bp_long heap_size=wam[HeapStk].size;
     bp_long check=wam[HeapStk].over;
     
     /*
     bp_long heap_size=wam[HeapStk].margin-wam[HeapStk].base;
     bp_long check=wam[HeapStk].end-wam[HeapStk].margin;
     */

     if(!make_stack(&wam[MarkStk],
        heap_size>>TAGBITS,
        check>> TAGBITS,
        "mark",NULL,0))
           OVER("GC-error: no memory for mark array",
                           (term *)H,HeapStk,NO());
  }

   /* ires contains arity - save regs[arity+1]..MAXDCG,MAXREGS */
   /* $$$ seems to be missed by GC PT */
   return gc_test0(H, regs, A, P, wam, ires); /*+MAXDCG);*/
}

#endif

static term gc_test(register term H, register term regs, register term *A,
             register instr P, register stack wam)
{ term xref;
  cell xval=ADDR2FUN(P);
  register bp_long ires=GETARITY(xval);
  GC_SHOW_BEFORE(H,(term)wam[HeapStk].margin);
  xref=gc_test1(H, regs, A, P, wam, ires);
  GC_SHOW_AFTER(H,xref);
  return xref;
}

/* only function CALLED from engine.c */

term trigger_gc(register term H, register term regs, register term *A, 
                register instr P, register stack wam) { 
  if(!OUTPUT_INT(g.gc)) 
    OVER("Please enlarge heap with option -h",(term *)H,HeapStk,NO());
  BUG("gc_called");
  X(1)=INPUT_INT(0);
#if 0
  X(2)=new_func("shunt",0);
  X(3)=new_func("no_wait",0);
#else
  X(2)=X(3)=X(1);
#endif
  return gc_test(H, regs, A, P, wam);
}

