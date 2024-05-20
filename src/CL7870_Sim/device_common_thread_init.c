#include <windows.h>
#include <stdio.h>

#include "modcomp_sim_types.h"
#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"



void device_common_thread_init(unsigned __int16 device_address, 
			DEVICE_WORKER_THREAD worker_proc, 
			DEVICE_OUTPUT_DATA output_data_proc, 
			DEVICE_OUTPUT_CMD output_cmd_proc, 
			DEVICE_INPUT_DATA input_data_proc, 
			DEVICE_INPUT_STATUS input_status_proc) {

	unsigned __int16 loc_dev_addr;
	unsigned __int16* thread_dev_ptr;
	HANDLE  device_thread;
	DWORD   device_thread_id;

	loc_dev_addr = device_address;
	thread_dev_ptr = &loc_dev_addr;


	// --------assign IO callbacks.
	iop_output_data_proc[device_address] = output_data_proc;
	iop_output_cmd_proc[device_address] = output_cmd_proc;
	iop_input_data_proc[device_address] = input_data_proc;
	iop_input_status_proc[device_address] = input_status_proc;

	// --------start thread
	iop_thread_stop_request[device_address] = 0;

	device_thread = CreateThread(
			NULL,					// default security attributes
			0,							// use default stack size  
			worker_proc,				// thread function name
			thread_dev_ptr,				// argument to thread function 
			0,							// use default creation flags 
			&device_thread_id);			// returns the thread identifier 


	// Check the return value for success.
	// If CreateThread fails, terminate execution. 
	// This will automatically clean up threads and memory. 

	if (device_thread == NULL) {
		printf("\n *** ERROR *** Trouble creating worker thread for device %02x.  Device not created.\n", device_address);
		// --------back everything out!
		device_common_remove(device_address);
	}
	else {
		printf("\n Device at device address  %02x created.\n", device_address);
		iop_device_thread_handle[device_address] = device_thread;
		iop_device_thread_id[device_address] = device_thread_id;
	}

}
