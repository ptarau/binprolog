#include "global.h"

#if VCCDLL
/*
#define DLL_EXPORT(Type) __declspec(dllexport) Type __stdcall
*/
#define DLL_EXPORT(Type) __declspec(dllexport) Type __cdecl
#else
#define DLL_EXPORT(Type) Type
#endif

extern void bp_exit_mes(string mes, bp_long i);
extern void *init_bp0(bp_long argc, char **argv,FILE* bp_stdin,FILE* bp_stdout);
extern char *run_bp0(register stack wam,char *query,bp_long *retcode);

DLL_EXPORT(void*) init_bp(bp_long argc, char **argv, FILE* bp_stdin, FILE* bp_stdout) {
  
   char *default_argv[]={"bp.dll",NULL};
   char **bp_argv;
   if(NULL==bp_stdin) bp_stdin=stdin;
   if(NULL==bp_stdout) bp_stdout=stdout;
   if(NULL==argv) {
     bp_argv=default_argv; 
     argc=1;
   }
   else 
     bp_argv=argv;
   
   return init_bp0(argc, bp_argv, bp_stdin, bp_stdout);
}

DLL_EXPORT(char*) run_bp(void *wam, char *query) {
   bp_long retcode=0;
   return run_bp0((stack)wam,query,&retcode); /* ignores retcode */
}

/* inialisation work in c.c, useful if BinProlog is embedded \
   in a C application
*/

extern int init_c(void);

DLL_EXPORT(int) bp_main(int argc, char **argv)
{ int ok; bp_long retcode=0; int initcode=0;
  void *wam=init_bp(argc,argv,stdin,stdout);
  ok=(!!wam);
  initcode=init_c();
  printf("Started Prolog Runtime System %d.\n",initcode);
  ok=ok && (initcode>0); /* inialise C-code for the host in c.c */

  if(ok) {
#if 0
    /* not used, but possible: see similar code in dir BP_DLL */

    /* query/answer then stop */
    char *query;
    char *answer;
    
    query="^(X,(for(X,1,5),>(X,2)))";
    answer=run_bp(wam,query);
    if(NULL==answer) answer="no";
    printf("query=>%s\nanswer=>%s\n",query,answer);

    query="*(I,for(I,1,10))";
    answer=run_bp(wam,query);
    if(NULL==answer) answer="no";
    printf("query=>%s\nanswer=>%s\n",query,answer);

    query="*(:(2,X),member(X,[a,b,c,d]))";
    answer=run_bp(wam,query);
    if(NULL==answer) answer="no";
    printf("query=>%s\nanswer=>%s\n",query,answer);

    ok=1;
#else
    run_bp0(wam,NULL,&retcode); /* plain toplevel: keeps retcode info */
#endif
  }
  else 
    retcode=!ok;
    bp_exit_mes("halted, code",retcode);
    return retcode;
}
