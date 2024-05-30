#include <stdio.h>
#include <stdbool.h>

#include "modcomp_sim_types.h"

void disp_pc(FILE* io_unit, unsigned __int16 loc_pc) {
	fprintf(io_unit, " Program counter 0x%04x\n", loc_pc );

}