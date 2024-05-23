#include <windows.h>
#include <stdio.h>

#include "modcomp_sim_types.h"
#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"



void device_common_thread_init(LPVOID data_buffer,
	DEVICE_WORKER_THREAD worker_proc,
	DEVICE_OUTPUT_DATA output_data_proc,
	DEVICE_OUTPUT_CMD output_cmd_proc,
	DEVICE_INPUT_DATA input_data_proc,
	DEVICE_INPUT_STATUS input_status_proc) {


	HANDLE  device_thread;
	DWORD   device_thread_id;

	DEVICE_GENERIC_DATA* generic_data_ptr;

	generic_data_ptr = (DEVICE_GENERIC_DATA*)data_buffer;

	unsigned __int16 device_address = generic_data_ptr->device_address;

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

		// --------start thread
		iop_thread_stop_request[device_address] = 0;

		device_thread = CreateThread(
			NULL,					// default security attributes
			0,							// use default stack size  
			worker_proc,				// thread function name
			(LPVOID)data_buffer,	// argument to thread function 
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
}
