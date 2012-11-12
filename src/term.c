#include "global.h"

#ifdef TSTORE
#include "term.h"
#include "termStore.h"

typedef cell half;
extern struct specsyms g;
extern double ints_to_double(register half i1, register half i2, register half i3);
extern term local_error(cell xval, string Msg, register stack wam);
extern bp_long deep_hash(register cell xval,bp_long max,bp_long mod);
extern cell input_fun (string name, no arity);
extern void trim_float(string buf);
extern term make_float(term H, double f);
extern void warnmes (string mes);

/* TERM STORE IMPLEMENTATION */

/* TESTS */

#if 0
int main(int argc,char *args) {termTester(); return 0;}
#endif

static char *sNIL="[]";

Term NIL() {
  Term T=newTerm(0);
  setArg(0,T,STRING_TYPE,S2O(sNIL));
  return T;
}

BYTE isNIL(Term T) {
  return (BYTE)(T->size==1 && STRING_TYPE==T->types[0] && 0==strcmp(sNIL,O2S(T->args[0])));
}

void termTester() {
  printf("\n***ENTERING termTester\n");
  {
  Term F=newTerm(2);
  setArg(0,F,STRING_TYPE,S2O("f"));
  setArg(1,F,INT_TYPE,I2O(99));
  setArg(2,F,FLOAT_TYPE,F2O(3.14));
  {
  Term G=newTerm(2);
  setArg(0,G,STRING_TYPE,S2O("g"));
  setArg(1,G,TERM_TYPE,T2O(F));
  setArg(2,G,STRING_TYPE,S2O("c"));
  { Term F1=O2T(getArg(1,G));
    showTerm(F1);printf("\n");
  }
  { char *s=O2S(getArg(2,G));
    printf("arg 2 of G is a string=>%s\n",s);
  }
  showTerm(G);printf("\n");
  freeTerm(G); /* this also frees F that's part of G */
  }
  { Term Tail=newStringList("b",NIL());
    Term L=newStringList("a",Tail);
    showTerm(L);
    freeTerm(L);
  }
  }
  printf("\n***EXITING termTester\n");
}


/* PROLOG TERM REPRESENTATION */

Term newTerm(bp_long arity) {
  Term T=(Term)malloc(sizeof(struct Term));
  /* range=0..arity, 0=functor, 1..arity=args */
  T->size=arity+1;
  T->types=(BYTE*)malloc(T->size*sizeof(BYTE));
  T->args=(OBJECT*)malloc(T->size*sizeof(OBJECT));
  {bp_long i;
   for(i=0;i<T->size;i++) {
     T->types[i]=(BYTE)BAD_TYPE;
     T->args[i]=NULL;
   }
  }
  return T;
}

OBJECT S2O(char *s) {
  return (OBJECT)s;
}

OBJECT I2O(bp_long l) {
  return (OBJECT)l;
}

OBJECT V2O(ulong v) {
  return (OBJECT)v;
}

OBJECT F2O(double x) {
  ulong l; float *f=(float*)&l; *f=(float)x;
  return (OBJECT)l;
}

OBJECT T2O(Term T) {
  return (OBJECT)T;
}

char *O2S(OBJECT O) {
  return (char *)O;
}

bp_long O2I(OBJECT O) {
  return (bp_long)O;
}

ulong O2V(OBJECT O) {
  return (ulong)O;
}

double O2F(OBJECT O) {
  ulong l=(ulong)O; 
  float *f; f=(float*)&l;
  return (double)*f;
}

Term O2T(OBJECT O) {
  return (Term)O;
}

Term newStringList(char *head, Term tail) {
  Term T=newTerm(2);
  /* Prolog compatible list functor */
  setArg(0,T,STRING_TYPE,S2O("."));
  /* head of a list - assumed a String */
  setArg(1,T,STRING_TYPE,S2O(head));
  /* tail of the list - should be another String list or NIL() */
  setArg(2,T,TERM_TYPE,T2O(tail));
  /* return the new List as a Term */
  return T;
}

void freeTerm(Term T) {
  bp_long i;
   for(i=0;i<T->size;i++) {
     BYTE type=T->types[i];
     if(TERM_TYPE==type) {
       Term X=O2T(T->args[i]);
       freeTerm(X);
     }
     else if(STRING_TYPE==type) {
       char *s=O2S(T->args[i]);
       freeTermString(s);
     }
   }
   free(T->types);
   free(T->args);
   free(T);
}

bp_long getArity(Term T) {
  return T->size-1;
}

BYTE setArg(bp_long i,Term T,BYTE termType,OBJECT arg) {
  if(i<0 || i>=T->size || termType>LAST_TYPE || termType==BAD_TYPE) return FALSE;
  
  T->types[i]=termType;
  if(STRING_TYPE==termType) arg=S2O(newTermString(O2S(arg)));
  T->args[i]=arg;
  return TRUE;
}

BYTE getType(bp_long i,Term T) {
  if(i<0 || i>=T->size) return BAD_TYPE;
  return T->types[i];
}

OBJECT getArg(bp_long i,Term T) {
  if(i<0 || i>=T->size) return NULL;
  return T->args[i];
}

static bp_long compareObjects(OBJECT O1,BYTE type1, OBJECT O2,BYTE type2) {
  bp_long ccode=(bp_long)type1-(bp_long)type2;
  if(0!=ccode) return ccode;
  switch(type1) {
    case STRING_TYPE: {
      char *s1=O2S(O1);
      char *s2=O2S(O2);
      ccode=strcmp(s1,s2);
      /*fprintf(STD_err,"compareObjects: %d %d strings=> %s %s => %d\n",O1,O2,s1,s2,ccode);*/
    } break;
    case INT_TYPE: {
      bp_long i1=O2I(O1);
      bp_long i2=O2I(O2);
      ccode=i1-i2;
      /*fprintf(STD_err,"compareObjects: %d %d ints=> %d %d => %d\n",O1,O2,i1,i2,ccode);*/
    } break;
    case FLOAT_TYPE: {
      double df1=O2F(O1);
      double df2=O2F(O2);
      ccode=(bp_long)(df1-df2);
    } break;
    case VAR_TYPE: {
      return 0;
    } break;
    case TERM_TYPE: {
      Term T1=O2T(O1);
      bp_long a1=getArity(T1);
      Term T2=O2T(O2);
      bp_long a2=getArity(T2);
      bp_long i;
      /*fprintf(STD_err,"compareObjects: %d %d terms => a1=%d a2=%d\n",O1,O2,a1,a2);*/
      if(a1!=a2) ccode=a1-a2;
      else {
        for(i=0;i<=a1;i++) {
          bp_long c;
          BYTE xtype1=getType(i,T1);
          OBJECT X1=getArg(i,T1);    
          BYTE xtype2=getType(i,T2);
          OBJECT X2=getArg(i,T2);    
          c=compareObjects(X1,xtype1,X2,xtype2);
          if(0==c) continue;
          ccode=c;
          break;
        }
      }
    } break;
    default: 
      /*fprintf(STD_err,"compareObjects: %d %d bad data => %d %d\n",O1,O2,type1,type2);*/
      warnmes("bad data in compare");     
      return -2;
  }
  return ccode;
}

/*
  -1: T1<T2
  0: T1 equals T2
  1: T1>T2
  -2: error
*/
bp_long compareTerms(Term T1,Term T2) {
  bp_long ccode;
  if(NULL==T1 || NULL==T2)  {
     warnmes("NULL data in compareTerms");
     return -2;
  }
  if(T1==T2) return 0;
  ccode=compareObjects(T2O(T1),TERM_TYPE,T2O(T2),TERM_TYPE);
  if(ccode>0) ccode=1;
  else if(ccode<0) ccode=-1;
  return ccode;
}

static bp_long hashCodeOf(OBJECT O,BYTE type,bp_long arity,bp_long maxdepth) {
  bp_long hcode=0;
  if(--maxdepth<0) {
    warnmes("maxdepth exceded in hashCodeOf");
    return -4;
  }
  switch(type) {
    case STRING_TYPE: {
      char *s=O2S(O);
      STRING_HASH(s,hcode,arity);
    } break;
    case INT_TYPE: {
      hcode=O2I(O);
    } break;
    case FLOAT_TYPE: {
      double df=O2F(O);
      hcode=(bp_long)df;
    } break;
    case VAR_TYPE: {
      return -1;
    } break;
    case TERM_TYPE: {
      Term T=O2T(O);
      bp_long arity=getArity(T);
      bp_long i;
      for(i=0;i<=arity;i++) {
        bp_long h;
        BYTE xtype=getType(i,T);
        OBJECT X=getArg(i,T);    
        h=hashCodeOf(X,xtype,arity,maxdepth);
        if(h<0) return h;
        else {
          hcode=(hcode<<5)+h;
        }
      }
    } break;
    default: 
      warnmes("bad data in hashCodeOf");
      return -2;
  }
  if(hcode<0) hcode=-hcode;
  return hcode;
}

/*
   all hashCodes are >=0
   negative codes indicate error conditions
*/ 
bp_long hashCode(Term T) {
  bp_long maxdepth=1<<16;
  if(NULL==T) {
    warnmes("NULL data in hashCode");
    return -3;
  }
  return hashCodeOf(T2O(T),TERM_TYPE,getArity(T),maxdepth);
}


static char *appendTo(char *buf,BYTE type,OBJECT O) {
  char dbuf[256];
  size_t l=0;
  switch(type) {
    case STRING_TYPE: {
      char *s=(char *)O;
      l=strlen(s);
      sprintf(buf,"%s",s);
    }
    break;
    case INT_TYPE: {
      sprintf(dbuf,"%ld",O2I(O));
      l=strlen(dbuf);
      sprintf(buf,"%s",dbuf);
    }
    break;
    case VAR_TYPE: {
      sprintf(dbuf,"_%ld",O2V(O));
      l=strlen(dbuf);
      sprintf(buf,"%s",dbuf);
    }
    break;
    case FLOAT_TYPE: {
      double df=O2F(O);
      sprintf(dbuf,"%#.4lf",df);
      trim_float(dbuf);
      l=strlen(dbuf);
      sprintf(buf,"%s",dbuf);
    }          
    break;
    default:
      sprintf(dbuf,"<?%ld?>",(bp_long)O);
      l=strlen(dbuf);
      sprintf(buf,"%s",dbuf);
  }
  return buf+l;
}

static char *appTo(char *buf,char *s) {
  bp_long l=strlen(s);
  sprintf(buf,"%s",s);
  return buf+l;
}

static char *toString0(Term T,char *buffer) {
  if(NULL==T) buffer=appTo(buffer,"(null)");
  { bp_long i;
    bp_long l=T->size;
    for(i=0;i<l;i++) {
      OBJECT X=T->args[i];
      BYTE type=T->types[i];
     
      if(i==1 && l>1) buffer=appTo(buffer,"(");
      if(i>1) buffer=appTo(buffer,",");  

      if(NULL==X && BAD_TYPE==type) buffer=appTo(buffer,"(null)");  
      else if(TERM_TYPE==type && NULL!=X) {
        buffer=toString0(O2T(X),buffer);
      }
      else {
        buffer=appendTo(buffer,T->types[i],X);
      }
    }
    if(l>1) buffer=appTo(buffer,")");
  }
  return buffer;
}

void toString(Term T,char *buffer) {
   buffer[0]='\0';
   if(NULL==T || NULL==toString0(T,buffer)) {
     sprintf(buffer,"(null)");
   }
}

char* toNewString(Term T) {
  char *s;
  toString(T,g.sbuf);
  s=strdup(g.sbuf);
  g.sbuf[0]='\0';
  return s; /* to be freed! */
}

bp_long stringLengthOf(Term T) {
  bp_long l;
  toString(T,g.sbuf);
  l=strlen(g.sbuf);
  g.sbuf[0]='\0';
  return l;
}

void showTerm(Term T) {
  char sbuf[1<<18];
  toString(T,sbuf);
  printf("%s",sbuf);
  sbuf[0]='\0';
}


/* GETTING / RETURNING A TERM FROM/TO PROLOG */

#define BP_op OUTPUT_INT(X(1))
#define BP_input X(2)
#define BP_data X(3)
#define BP_result X(4)

#define BP_is_integer(Cell) INTEGER(Cell)
#define BP_is_var(Cell) VAR(Cell)
#define BP_is_atom(Cell) SYMCONST(Cell)
#define BP_is_nonvar(Cell) NONVAR(Cell)
#define BP_is_compound(Cell) COMPOUND(Cell)
#define BP_is_float(Cell) BP_FLOAT(Cell)
#define BP_is_number(Cell) NUMERIC(Cell)
#define BP_is_list(Cell) IS_LIST(Cell)

#define BP_string(Name) input_fun((Name),0)
#define BP_functor(Name,Arity) input_fun((Name),Arity)
#define BP_integer(Int) INPUT_INT((Int))
#define BP_funptr(Ptr) PTR2INT((Ptr))
#define BP_make_float(Var,FVal) {(Var)=(cell)H; H=make_float(H,(FVal));}

#define BP_nil g.NIL

#define BP_put_new_var(Var) (Var)=(cell)(SETREF(H,H),H++)
#define BP_put_old_var(Var) {SETREF(H,Var);H++;}

#define BP_begin_put_list(Result) (Result)=(cell)H
#define BP_put_list(Element) PUSH_LIST((cell)(Element))
#define BP_end_put_list() PUSH_NIL()

#define BP_put_arg(Element) {SETREF(H,(Element)); H++;}
#define BP_get_integer(From,IVar) IVar=OUTPUT_INT(From)
#define BP_get_string(From,SVar) SVar=NAME(From)

#define BP_check_call() \
if(!BP_is_integer(X(1))) \
    return LOCAL_ERR(X(1), \
      "op-code of C-function must be an integer");\
BP_result=BP_input;

static OBJECT externalize_object(term t,BYTE *type) {
  OBJECT result=NULL;
  *type=BAD_TYPE;

  if(BP_is_integer(t)) {
    bp_long i;
    BP_get_integer(t,i);
    *type=INT_TYPE;
    result=I2O(i);
  }
  else if(BP_is_atom(t)) {
    char *s;
    BP_get_string(t,s);
    *type=STRING_TYPE;
   result=S2O(s);
  }
  else if(BP_is_var(t)) {
     cell val=GETREF(t);
     if(BP_is_float(val)) {
       double df=ints_to_double((half)(t[1]),(half)(t[2]),(half)(t[3]));
       *type=FLOAT_TYPE;
       result=F2O(df);  
     }
     else if(BP_is_var(val)) {
       *type=VAR_TYPE;
       result=V2O(val);
     }
     else if(BP_is_compound(val)) { /* recursive case */
       bp_long i;
       bp_long l=GETARITY(val);
       Term T=newTerm(l);
       *type=TERM_TYPE;
       setArg(0,T,STRING_TYPE,S2O(NAME(val)));
       for(i=1;i<=l;i++) {
         OBJECT X; BYTE xtype=BAD_TYPE;
         cell xval;term xref;
         FDEREF(t+i);
         if(!BP_is_compound(xval)) xref=C2T(xval);
         X=externalize_object(xref,&xtype);
         if(NULL==X && xtype==BAD_TYPE) {T=NULL; *type=BAD_TYPE; break;}
         setArg(i,T,xtype,X);
       }
       result=T;
     }
     else {
       *type=BAD_TYPE;
       result=NULL;
     }   
  }
  else {
    *type=BAD_TYPE;
    result=NULL;
  }
  return result;
}

/*
  TODO: variables are not made canonical
  they are passed as their original addresses
*/
Term externalize(term t) {
  BYTE type=BAD_TYPE;
  Term T=NULL;
  OBJECT X=externalize_object(t,&type);
  if(NULL==X && type==BAD_TYPE) T=NULL;
  else if(TERM_TYPE==type && X!=NULL) T=O2T(X);
  else {
    T=newTerm(0);
    setArg(0,T,type,X);
  }
  return T;
}

static term internalize_object(OBJECT O,BYTE type,register term H,term result) {
  *result=BP_nil;
 
  if(NULL==H) return NULL;
  switch(type) {
    case STRING_TYPE: {
      *result=BP_string(O2S(O));
    } break;
    case INT_TYPE: {
      *result=BP_integer(O2I(O));
    } break;
    case FLOAT_TYPE: {
      double df=O2F(O);
      BP_make_float(*result,df);
    } break;
    case VAR_TYPE: {
      /* TODO */
      BP_put_new_var(*result);
    } break;
    case TERM_TYPE: {
      Term T=O2T(O);
      bp_long i;
      bp_long l=T->size;
      cell *args=malloc(l*sizeof(cell));
  
      for(i=0;i<l;i++) {
        BYTE xtype=getType(i,T);
        OBJECT X=getArg(i,T);
     
        H=internalize_object(X,xtype,H,args+i);
        if(NULL==H) return NULL;
      }     
      args[0]=PUTARITY(args[0],getArity(T));
      *result=T2C(H);
      for(i=0;i<l;i++) {
        BP_put_arg(args[i]);
      }
      free(args);
    
    } break;
    default: 
      /*fprintf(STD_err,"BAD_OBJECT=>object=%ld type=%ld H=%ld\n",O,type,H);*/
      warnmes("bad data in internalize_object");
      return NULL;
  }
  return H;
}

/*
  TODO: currently each variable is seen as new
  use a hashtable and connect each to their first
  occurence
*/
term internalize(Term T,register term H,term result) {
  if(NULL==T) return NULL;
  return internalize_object(T2O(T),TERM_TYPE,H,result);
}

#define TSTEST_K_V 0
#define PUSH_TERM_K_V 1
#define PUT_TERM_K_V 2
#define NEW_ITERATOR_K 3
#define CLOSE_ITERATOR_I 4
#define HAS_TERMS_K 5
#define GET_NEXT_TERM_I 6
#define REMOVE_CURRENT_TERM_I 7
#define UPDATE_CURRENT_TERM_I 8
#define DELETE_ALL_TERMS_K 9
#define COUNT_TERMS_K 10
#define NEW_TERM_K 11
#define INSTANCE_OF_I 12
#define FREE_TERM_I 13
#define NEW_KEY_ITERATOR 14
#define PROCESS_TERM_F_X 15

term term_store_op(register term H, register term regs,register stack wam)
{
  BP_check_call();
  BP_result=BP_string("yes");

  switch(BP_op) {
    case TSTEST_K_V: {
      Term K; Term V;    
      K=externalize(C2T(BP_input));
      showTerm(K);printf("<=K:%ld\n",(long)K);
      printf("K:hashCode=%ld\n",hashCode(K));

      V=externalize(C2T(BP_data));  
      showTerm(V);printf("<=V\n");
      printf("V:hashCode=%ld\n",hashCode(V));
      
      { bp_long l; char *buf; char *s=toNewString(V); 
        printf("V:toNewString=%s\n",s);
        free(s);
        l=stringLengthOf(V);
        printf("V:stringLengthOf=%ld\n",l);
        buf=(char *)malloc(l+1);
        toString(V,buf);
        printf("V:to safely sized buffer=%s\n",buf);
        free(buf);
      };

      printf("compareTerms(K,V)=%ld\n",compareTerms(K,V));
      { cell result=ZERO;
        Term KV=newTerm(2);
        setArg(0,KV,STRING_TYPE,S2O("=>"));

        setArg(1,KV,TERM_TYPE,T2O(K));
        setArg(2,KV,TERM_TYPE,T2O(V));

        H=internalize(KV,H,&result);
        BP_result=result;
      }
    }

    case PUSH_TERM_K_V: {
      Term K=externalize(C2T(BP_input));
      Term V=externalize(C2T(BP_data));
      pushTerm(K,V);
    }
    break;
    case PUT_TERM_K_V: /*termTester()*/;
      { Term K; Term V;     
        K=externalize(C2T(BP_input));
        V=externalize(C2T(BP_data));  
        putTerm(K,V);
      }
      break;
    case NEW_ITERATOR_K: {
        Term K=externalize(C2T(BP_input));
        ulong i=newIterator(K);
        BP_result=BP_integer(i);
      }
      break;
    case CLOSE_ITERATOR_I:{
        ulong i;
        BP_get_integer(BP_input,i); 
        closeIterator(i);
      }
      break;
    case HAS_TERMS_K: {
        Term K=externalize(C2T(BP_input));
        BP_result=BP_integer(hasTerms(K));
      }
      break;
    case GET_NEXT_TERM_I:  {
         bp_long i; Term V;cell result=ZERO;
         BP_get_integer(BP_input,i);
         V=getNextTerm(i);
         H=internalize(V,H,&result);  
         BP_result=result;
      }
      break;
    case REMOVE_CURRENT_TERM_I:  {
         ulong i;
         BP_get_integer(BP_input,i);
         removeCurrentTerm(i);
      }
      break;
    case UPDATE_CURRENT_TERM_I:  {
         ulong i; Term V;
         BP_get_integer(BP_input,i);
         V=externalize(C2T(BP_data));
         updateCurrentTerm(i,V);
      }
    break;
    case DELETE_ALL_TERMS_K: {
      Term K=externalize(C2T(BP_input));
      deleteAllTerms(K);
    }
    break;
    case COUNT_TERMS_K: {
      Term K=externalize(C2T(BP_input));
      BP_result=BP_integer(countTerms(K));
    }
    break;

    case NEW_TERM_K: {
      Term K=externalize(C2T(BP_input));
      BP_result=BP_integer((cell)K);
    }
    break;

    case INSTANCE_OF_I: {
      cell handle;ulong result=ZERO;
      BP_get_integer(BP_input,handle);
      { Term K=(Term)handle;
        H=internalize(K,H,&result); 
      }
      BP_result=result;
    }
    break;

    case FREE_TERM_I: {
      cell handle;
      BP_get_integer(BP_input,handle);
      { Term K=(Term)handle;
        freeTerm(K);
      }
    }
    break;

    case NEW_KEY_ITERATOR: {
        bp_long i=newKeyIterator();
        BP_result=BP_integer(i);
    }
    break;

    case PROCESS_TERM_F_X: {
      bp_long f; Term X,Y; ulong result=ZERO;
      BP_get_integer(BP_input,f);
      X=externalize(C2T(BP_data));
      Y=processTerm(f,X);
      H=internalize(Y,H,&result);
      if(NULL!=Y) freeTerm(Y);
      BP_result=result;
    }
    break;
    default: 
      return LOCAL_ERR(BP_op,"bad op-code in term_store_op");
  }

  return H;
}
#endif
