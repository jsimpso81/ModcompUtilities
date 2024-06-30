// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			cpu_do_run.c
//
//	Description:	Performs CPU run.
//
//	Externally accessible routines:
//					void cpu_do_run()
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


void cpu_do_run() {


	// -------- ensure we can do a run
	if (!cpu_get_power_on()) {
		printf(" *** ERROR ***  Cant perform RUN.  CPU is not powered on.\n");
	}
	else if (gbl_fp_single_step) {
		printf(" *** ERROR ***  Cant perform RUN.  CPU is being single stepped.\n");
	}
	else {

		// --------set run flag
		gbl_fp_runlight = true;

	}

}