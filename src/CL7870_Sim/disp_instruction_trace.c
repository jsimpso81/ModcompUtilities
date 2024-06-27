#include "simj_base.h"

#include "cpu_macro_register_access.h"
#include "cpu_instruction_memory_macros.h"

#include <stdio.h>

#include <stdbool.h>
#include <string.h>


void disp_instruction_trace(FILE* io_unit) {

	int j = 0;

	SIMJ_U16 trace_index = 0;
	SIMJ_U32 trace_stack[1024] = { 0 };
	SIMJ_U16 addr = 0;
	SIMJ_U16 instruction = 0;
	SIMJ_U8  junkxx[200] = { 0 };

	cpu_get_instruction_trace(&trace_index, &trace_stack[0]);

	fprintf(io_unit, "\nInstruction Trace\n");
	fprintf(io_unit, " Virt Addr   Opcode\n");

	// --------loop over stack;
	for (j = 0; j < 1024; j++) {

		addr = trace_stack[(trace_index + j) & 0x03ff];
		instruction = GET_MEMORY_VALUE_ABS(addr);

		util_get_opcode_disp(instruction, &junkxx[0], sizeof(junkxx));

		fprintf(io_unit, "   0x%04x:  0x%04x    %s \n",
			addr, instruction, junkxx);

	}
}
