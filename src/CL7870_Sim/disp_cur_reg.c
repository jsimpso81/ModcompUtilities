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

void disp_cur_reg(FILE* io_unit) {

	fprintf(io_unit, " Current register block\n");
	fprintf(io_unit, "       0  |  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n",
		cpu_get_register_value(0),
		cpu_get_register_value(1),
		cpu_get_register_value(2),
		cpu_get_register_value(3),
		cpu_get_register_value(4),
		cpu_get_register_value(5),
		cpu_get_register_value(6),
		cpu_get_register_value(7)
	);
	fprintf(io_unit, "       8  |  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n",
		cpu_get_register_value(8),
		cpu_get_register_value(9),
		cpu_get_register_value(10),
		cpu_get_register_value(11),
		cpu_get_register_value(12),
		cpu_get_register_value(13),
		cpu_get_register_value(14),
		cpu_get_register_value(15)
	);

}
