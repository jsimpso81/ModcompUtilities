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
#include <stdbool.h>
#include <string.h>

#include "modcomp_opcodes.h"


void util_get_opcode_disp(SIMJ_U16 instruction, char* op_buffer, size_t buf_size) {

	SIMJ_U16 opcode_index = OPSI_NOINST;

	opcode_index = opcode_get_opcode_index(instruction);
	strcpy_s(op_buffer, buf_size, &(opcode_si_string[opcode_index][0]));
	return;
}