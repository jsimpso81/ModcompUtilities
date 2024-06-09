#include "simj_base.h"

#include <stdio.h>
#include <stdbool.h>

void disp_pc(FILE* io_unit, SIMJ_U16 loc_pc) {
	fprintf(io_unit, " Program counter 0x%04x\n", loc_pc );

}