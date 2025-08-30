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
#include <stdlib.h>
#include <stdbool.h>

#include "modcomp_opcodes.h"

void iop_init_data() {

	int j;

	// --------indexed by device address..
	for (j = 0; j < 64; j++) {

		// iop_last_dev_status[j] = 0;

		iop_output_data_proc[j] =  NULL ;
		iop_output_cmd_proc[j] =  NULL ;
		iop_input_data_proc[j] =  NULL ;
		iop_input_status_proc[j] =  NULL ;
		iop_mount_unit_proc[j] = NULL;
		iop_dismount_unit_proc[j] = NULL;

		iop_device_buffer[j] = NULL;
		iop_thread_stop_request[j] = 0;
		iop_device_thread_handle[j] = 0;
		iop_device_thread_id[j] = 0;

	}


	// -------- indexed by DMP number.
	for (j = 0; j < 64; j++) {
		iop_vdmp_miap_page[j] = 0;
		iop_vdmp_miap_length[j] = 0;
	}
}