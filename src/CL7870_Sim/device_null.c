#include <windows.h>
#include <stdio.h>

#include "modcomp_sim_types.h"
#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"

// -------- DEVICE NULL

void  device_null_output_data(unsigned __int16 device_address, unsigned __int16 data_value) {
	//printf("\n device_null output data -- called - %04x\n", data_value);
}

void  device_null_output_cmd(unsigned __int16 device_address, unsigned __int16 cmd_value) {
	//printf("\n device_null output cmd -- called - %04x\n", cmd_value);
}

unsigned __int16  device_null_input_data(unsigned __int16 device_address) {
	//printf("\n device_null input data -- called - 0x0000\n");
	return 0;
}

unsigned __int16  device_null_input_status(unsigned __int16 device_address) {
	//printf("\n device_null input status -- called - 0x0180\n");
	return 0x0180;

}


DWORD WINAPI device_null_worker_thread(LPVOID lpParam) {

	unsigned __int16 loc_device_addr;
	DEVICE_NULL_DATA* device_data = 0;

	loc_device_addr = *(unsigned __int16*)lpParam;
	device_data = (DEVICE_NULL_DATA*)iop_device_buffer[loc_device_addr];


	//console_status |= 0x8000; //--------indicate we are on.

	while (iop_thread_stop_request[loc_device_addr] == 0) {

		// --------if a new command process it
		//if (console_command != 0) {

		//	cmd_type = (console_command >> 14) & 0x0003;

		//	switch (cmd_type) {

		//		// --------command
		//	case 1:

		//		// --------terminate
		//		if ((console_command & 0x0400) != 0) {
		//			dev_reading = false;
		//			dev_writing = false;
		//			console_input_buffer_count = 0;
		//			console_input_buffer_index = 0;
		//			console_status |= 0x0080;
		//		}
		//		break;

		//		// --------transfer initiate
		//	case 2:
		//		if (console_command & 0x0800) {
		//			dev_reading = true;
		//			dev_writing = false;	// not full duplex?? so can't do both.
		//		}
		//		else {
		//			dev_reading = false;
		//			dev_writing = true;	// not full duplex?? so can't do both.
		//		}
		//		break;

		//	}
		//	console_command = 0;
		//}

		//--------if not connected, get connected.
		//if (!dev_connected) {
		//	// --------set status

		//	//---------do a listen -- 2 second timeout
		//}
	}

	// --------unset global values and deallocate memory
	device_common_remove(loc_device_addr);
	iop_thread_stop_request[loc_device_addr] = 0;

	ExitThread(0);

	return 0;

}

// TODO: this routine could be made generic
void device_null_init(unsigned __int16 device_address) {

	DEVICE_NULL_DATA* device_data = 0;
	unsigned __int16 loc_dev_addr;
	unsigned __int16* thread_dev_ptr;
	HANDLE  device_thread;
	DWORD   device_thread_id;

	loc_dev_addr = device_address;
	thread_dev_ptr = &loc_dev_addr;

	// --------make certain another device has not been initialized here
	if (iop_device_buffer[device_address] != NULL) {
		printf("\n *** ERROR ***  Device at device address %02x already allocated\n", device_address);
	}
	else {

		// --------create data structure
		iop_device_buffer[device_address] = (void*)HeapAlloc(GetProcessHeap(),
			HEAP_ZERO_MEMORY,
			sizeof(DEVICE_NULL_DATA));

		if (iop_device_buffer[device_address] == NULL) {
			printf("\n *** ERROR ***  Could not allocate memory for device address %02x.  Device not created.\n", device_address);
		}
		else {

			// --------fill in global data
			device_data = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];

			device_data->ctrl_status = 0x8080;
			device_data->ctrl_command = 0x0000;

			// --------assign IO callbacks.
			iop_output_data_proc[device_address] = device_null_output_data;
			iop_output_cmd_proc[device_address] = device_null_output_cmd;
			iop_input_data_proc[device_address] = device_null_input_data;
			iop_input_status_proc[device_address] = device_null_input_status;

			// --------start thread
			iop_thread_stop_request[device_address] = 0;

			device_thread = CreateThread(
				NULL,					// default security attributes
				0,							// use default stack size  
				device_null_worker_thread,	// thread function name
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
	}
}
