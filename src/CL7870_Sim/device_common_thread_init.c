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



void device_common_thread_init(LPVOID data_buffer,
	DEVICE_WORKER_THREAD worker_proc,
	DEVICE_OUTPUT_DATA output_data_proc,
	DEVICE_OUTPUT_CMD output_cmd_proc,
	DEVICE_INPUT_DATA input_data_proc,
	DEVICE_INPUT_STATUS input_status_proc,
	DEVICE_MOUNT_UNIT mount_unit_proc,
	DEVICE_DISMOUNT_UNIT dismount_unit_proc) {

	uintptr_t  device_thread;
	DWORD   device_thread_id = 0;

	DEVICE_GENERIC_DATA* generic_data_ptr;

	generic_data_ptr = (DEVICE_GENERIC_DATA*)data_buffer;

	SIMJ_U16 device_address = generic_data_ptr->device_address;

	if (device_address <= 0 || device_address > 63) {
		printf("\n *** ERROR *** Trouble creating device.  Device address %02x is invalid.  Device not created.\n", device_address);

	}

	// -------- device address is good, create thread.
	else {
		// --------assign IO callbacks.
		iop_output_data_proc[device_address] = output_data_proc;
		iop_output_cmd_proc[device_address] = output_cmd_proc;
		iop_input_data_proc[device_address] = input_data_proc;
		iop_input_status_proc[device_address] = input_status_proc;
		iop_mount_unit_proc[device_address] = mount_unit_proc;
		iop_dismount_unit_proc[device_address] = dismount_unit_proc;

		// --------start thread
		iop_thread_stop_request[device_address] = 0;
		iop_thread_stop_request2[device_address] = 0;

		device_thread = device_common_start_thread(data_buffer, worker_proc, &device_thread_id);

		// Check the return value for success.
		// If CreateThread fails, terminate execution. 
		// This will automatically clean up threads and memory. 

		if (device_thread == 0) {
			printf("\n *** ERROR *** Trouble creating worker thread for device %02x.  Device not created.\n", device_address);
			// --------back everything out!
			device_common_remove(device_address);
		}
		else {
			printf(" Device at device address  %02x created.\n", device_address);
			iop_device_thread_handle[device_address] = device_thread;
			iop_device_thread_id[device_address] = device_thread_id;
		}
	}
}
