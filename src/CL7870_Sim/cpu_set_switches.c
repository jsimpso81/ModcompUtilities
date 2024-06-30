// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			cpu_set_switches.c
//
//	Description:	Sets CPU front panel (Register 0) switches.
//
//	Externally accessible routines:
//					void cpu_set_switches(SIMJ_U16 switch_value)
// 
//	Internal only routines:
// 
// Notes:			
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================

#include "simj_base.h"


void cpu_set_switches(SIMJ_U16 switch_value) {

	gbl_fp_switches = switch_value;
	cpu_set_register_value(0, switch_value);

}