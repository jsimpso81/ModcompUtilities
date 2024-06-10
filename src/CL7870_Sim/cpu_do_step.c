#include "simj_base.h"


void cpu_do_step( SIMJ_U16 step_count ) {

	SIMJ_U16 j;

	// -------- ensure we can do a run
	if (!cpu_get_power_on()) {
		printf(" *** ERROR ***  Cant perform STEP.  CPU is not powered on.\n");
	}
	else if (gbl_fp_runlight) {
		printf(" *** ERROR ***  Cant perform STEP.  CPU is not HALTed.\n");
	}
	else {

		// --------Do single stepping.
		bool diffval = true;
		for (j = 0; j < step_count; j++) {
			gbl_fp_single_step = true;
			WakeByAddressSingle((LPVOID)&gbl_fp_single_step);
			WaitOnAddress(&gbl_fp_single_step, &diffval, sizeof(gbl_fp_single_step), INFINITE);
			disp_pc(stdout, cpu_get_program_counter());
			disp_psw(stdout, cpu_get_current_PSW());
			disp_cur_reg(stdout);
		}

	}

}