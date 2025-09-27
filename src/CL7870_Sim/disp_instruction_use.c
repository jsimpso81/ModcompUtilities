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

#include <stdio.h>


void disp_instruction_use(FILE* io_unit) {

	SIMJ_U16 j = 0;
	SIMJ_U16 j1 = 0;
	char opcode[100];
	SIMJ_U16 instruct = 0;


	for (j = 0; j < NUMB_OPSI; j++) {
		instruct = (j << 8);		// this is fake... it misses the augments..
		// util_get_opcode_disp(instruct, opcode, 100);
		strcpy_s(opcode, 100, &(opcode_si_string[j][0]));
		fprintf(io_unit, " 0x%04x %s %d\n", instruct, opcode, cpu_inst_used[j]);
	}
}