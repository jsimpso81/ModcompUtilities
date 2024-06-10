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