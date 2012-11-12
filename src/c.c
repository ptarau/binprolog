/************MANDATORY CODE: DO NOT DELETE ******************************/

#ifdef VCCDLL
#define DllImport __declspec(dllimport)
DllImport int bp_main(int argc, char **argv);
#else
extern int bp_main(int argc, char **argv);

/* MAIN FILE: change this if you want to call BinProlog as a DLL */
/* or some other form of dynamically linked library              */

#ifndef NOMAIN
int main(int argc, char **argv) 
{
  return bp_main(argc,argv);
}
#endif
#endif

/* code BinProlog will call before after its own initalization process */

int init_c() {
  return 1;
}

/* YOU SHOULD PROVIDE HERE BOTH init() and main() */

/**************************END OF MANDATORY CODE*************************/


/* USER CODE FOR BinProlog's C-iterface */

/****************************************************************
          STUB in CASE THE C_INTERFACE IS NOT USED

(Real) files using new C-interface (c.pl c.h c.c have been moved to 
directory ../c_inter
*****************************************************************/

#include "global.h"

extern struct specsyms g;

string c_interface="";

static string c_errmess="c.c (STUB ONLY) : undefined behaviour\n";

term if0(register term regs, register term H, register instr P, register term *A, register stack wam)
{
  fprintf(STD_err,"%s",c_errmess);
  return NULL;
}

term new_builtin(register term H, register term regs, register term *A, register instr P, register stack wam)
{
  fprintf(STD_err,"%s",c_errmess);
  return NULL;
}
