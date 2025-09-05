// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			XXXX.c
//
//	Description:	Routines to xxxxxxx.
//
//	Externally accessible routines:
//					XXXXXXX
// 
// Internal only routines:
//					XXXXXXX
//
// Notes:
//		XXXXXX
// 
// ================================================================================================
//	Revision history:
//		6/28/2024	JAS		Added new header
// ================================================================================================

#include "simj_base.h"

#include "cpu_macro_register_access.h"
#include "cpu_instruction_memory_macros.h"

#include <stdio.h>

#include <stdbool.h>
#include <string.h>


void disp_instruction_trace(FILE* io_unit) {

	int j = 0;

	SIMJ_U16 loc_trace_index = 0;
	SIMJ_U16 loc_trace_virt_pc[1024] = { 0 };
	PSW loc_trace_psw[1024] = { 0 };
	SIMJ_U16 loc_trace_actint[1024] = { 0 };
	SIMJ_U32 loc_trace_stack[1024] = { 0 };
	SIMJ_U32 loc_trace_stack_w1[1024] = { 0 };
	SIMJ_U32 loc_trace_stack_w2[1024] = { 0 };
	SIMJ_U32 loc_trace_stack_w3[1024] = { 0 };
	SIMJ_U32 loc_trace_stack_w4[1024] = { 0 };

	SIMJ_U16 abs_addr = 0;
	SIMJ_U16 virt_addr = 0;
	SIMJ_U16 instruction = 0;
	SIMJ_U8  junkxx[200] = { 0 };

	cpu_get_instruction_trace(&loc_trace_index, 
			&loc_trace_virt_pc[0], &loc_trace_psw[0], &loc_trace_actint[0],
					&loc_trace_stack[0],	&loc_trace_stack_w1[0], &loc_trace_stack_w2[0],
					&loc_trace_stack_w3[0], &loc_trace_stack_w4[0]);

	fprintf(io_unit, "\nInstruction Trace\n");
	fprintf(io_unit, " PSW                                                      Act Int | Virt Addr | Abs Addr   Opcode            ActInt      PSW\n");

	int index = 0;
	PSW loc_psw = { 0 };
	SIMJ_U16 loc_actint = 0;

	// --------loop over stack;
	for (j = 0; j < 1024; j++) {

		index = (loc_trace_index + j) & 0x03ff;
		virt_addr = loc_trace_virt_pc[index];
		loc_psw = loc_trace_psw[index];
		loc_actint = loc_trace_actint[index];

		abs_addr = loc_trace_stack[index];
		instruction = GET_MEMORY_VALUE_ABS(abs_addr);

		util_get_opcode_disp(instruction, &junkxx[0], sizeof(junkxx));

		fprintf(io_unit, " PSW 0x%04x, GRP: %2d, IM: %d, OM: %d, PRV: %d, OH: %d, N: %d, Z: %d, O: %d, C: %d ",
			loc_psw.all, loc_psw.sep.grb, loc_psw.sep.im, loc_psw.sep.om, loc_psw.sep.prv,
			loc_psw.sep.oh, loc_psw.sep.cc_n, loc_psw.sep.cc_z, loc_psw.sep.cc_o, loc_psw.sep.cc_c);
		fprintf(io_unit, "  0x%04x  |  0x%04x:  |  0x%08x:  0x%04x  %s \n",
			loc_actint, virt_addr, abs_addr, instruction, junkxx);

	}
}
