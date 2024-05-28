#include <stdio.h>

#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"


void disp_instruction_use() {

	unsigned __int16 j = 0;
	char opcode[100];

	for (j = 0; j < 256; j++) {
	
		util_get_opcode_disp(j<<8, opcode, 100);
		printf(" %d %s %d\n", j, opcode, cpu_inst_used[j]);
	}
}