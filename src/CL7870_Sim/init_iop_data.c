#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "modcomp_sim_external_globals.h"
#include "modcomp_opcodes.h"
#include "modcomp_sim_procedures.h"

void init_iop_data() {

	int j;

	for (j = 0; j < 64; j++) {

		iop_last_dev_status[j] = 0;

		iop_output_data_proc[j] =  NULL ;
		iop_output_cmd_proc[j] =  NULL ;
		iop_input_data_proc[j] =  NULL ;
		iop_input_status_proc[j] =  NULL ;
	}

	iop_output_data_proc[0x0a] = device_null_output_data;
	iop_output_cmd_proc[0x0a] = device_null_output_cmd;
	iop_input_data_proc[0x0a] = device_null_input_data;
	iop_input_status_proc[0x0a] = device_null_input_status;


}