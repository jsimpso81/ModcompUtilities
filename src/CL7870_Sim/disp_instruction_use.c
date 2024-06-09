#include "simj_base.h"

#include <stdio.h>


void disp_instruction_use(FILE* io_unit) {

	SIMJ_U16 j = 0;
	char opcode[100];

	for (j = 0; j < 256; j++) {
	
		util_get_opcode_disp(j<<8, opcode, 100);
		fprintf(io_unit, " %d %s %d\n", j, opcode, cpu_inst_used[j]);
	}
}