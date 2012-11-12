#include "global.h"
#include "c_defs.h"
#include "binpro.h"

struct bp_instr user_bp[] = {
{220,0,1,"prolog:v1393c0"},
{221,3,0,"_"},
{17,0,1,"true"},
{0,0,0,"c"},
{255,0,0,"user_bp"}};

bp_long user_bp_size=sizeof(user_bp);

#define c_threshold_min 5
#define c_threshold_max 500

