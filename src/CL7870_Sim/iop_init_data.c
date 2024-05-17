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

		iop_device_buffer[j] = NULL;
		iop_thread_stop_request[j] = 0;
		iop_device_thread_handle[j] = 0;
		iop_device_thread_id[j] = 0;

	}

	device_null_init(0x0a);

}