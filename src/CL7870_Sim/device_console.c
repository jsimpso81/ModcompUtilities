// -------- DEVICE CONSOLE
#include <windows.h>
#include <stdio.h>
#include <stdbool.h>

#include "modcomp_sim_types.h"
#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"

//  only one instance of this is allowed, at device address 0x0A.  Therefore the data here is global.

#define status_exists			0x8000
#define status_busy				0x0100
#define status_data_not_ready	0x0080
#define status_input_ready      0x0040
#define status_break			0x0020
#define status_full_duplex		0x0010

#define cmd_mask				0xc000
#define cmd_control				0x4000
#define cmd_transfer_initiate	0x8000
#define cmd_noop				0x4000
//#define cmd_other

#define ctrl_terminate			0x0400
#define ctrl_abort				0x0100
#define ctrl_icb				0x0040
#define ctrl_break_det			0x0020
#define ctrl_si_rel				0x0140


void  device_console_output_data(unsigned __int16 device_address, unsigned __int16 data_value) {
	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];
	//printf("\n device_console output data -- called - %04x\n", data_value);
	if (databuffer->ctrl_output_buffer_index >= DEVICE_CONSOLE_MAX_BUFFER) {
		printf("\n Console device output buffer overflow.\n");
	}
	else {
		databuffer->ctrl_output_buffer[databuffer->ctrl_output_buffer_index++] = (0x00ff & data_value);
		databuffer->ctrl_wake++;
		WakeByAddressSingle(&(databuffer->ctrl_wake));
	}
}

void  device_console_output_cmd(unsigned __int16 device_address, unsigned __int16 cmd_value) {
	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];
	//printf("\n device_console output cmd -- called - %04x\n", cmd_value);
	que_uword_send(&(databuffer->ctrl_command_que), cmd_value);
	databuffer->ctrl_wake++;
	WakeByAddressSingle(&(databuffer->ctrl_wake));
}

unsigned __int16  device_console_input_data(unsigned __int16 device_address ) {
	unsigned __int16 ourvalue = 0;
	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];
	if (databuffer->ctrl_input_buffer_count > databuffer->ctrl_input_buffer_index) {
		ourvalue = databuffer->ctrl_input_buffer[databuffer->ctrl_input_buffer_index++];
		databuffer->ctrl_status |= (status_busy | status_data_not_ready);
	}
	printf("\n device_null input data -- called - 0x%04x\n", ourvalue);
	return ourvalue;
}

unsigned __int16  device_console_input_status(unsigned __int16 device_address ) {
	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];
	printf("\n device_console input status -- called - 0x%04x\n", databuffer->ctrl_status);
	return databuffer->ctrl_status;
}

DWORD WINAPI device_console_worker_thread(LPVOID lpParam) {

	unsigned __int16 loc_device_addr;
	DEVICE_CONSOLE_DATA* device_data = 0;
	bool dev_connected = false;
	bool dev_reading = false;
	bool dev_writing = false;
	unsigned __int16 cmd_type = 0;
	unsigned __int16 loc_cmd = 0;

	// --------get info for this device.
	loc_device_addr = *(unsigned __int16*)lpParam;
	device_data = (DEVICE_CONSOLE_DATA*)iop_device_buffer[loc_device_addr];


	//console_status |= 0x8000; //--------indicate we are on.

	while (iop_thread_stop_request[loc_device_addr] == 0) {

		// --------process commands
		while (que_uword_recv(&(device_data->ctrl_command_que), &loc_cmd) ) {

			cmd_type = (loc_cmd >> 14) & 0x0003;

			switch (cmd_type) {

				// --------command
			case 1:

				// --------terminate
				if ((loc_cmd & 0x0400) != 0) {
					dev_reading = false;
					dev_writing = false;
					device_data->ctrl_input_buffer_count = 0;
					device_data->ctrl_input_buffer_index = 0;
					device_data->ctrl_status |= 0x0080;
				}
				break;

				// --------transfer initiate
			case 2:
				if (loc_cmd & 0x0800) {
					dev_reading = true;
					dev_writing = false;	// not full duplex?? so can't do both.
				}
				else {
					dev_reading = false;
					dev_writing = true;	// not full duplex?? so can't do both.
				}
				break;
			}
			
		}

		//--------if not connected, get connected.
		if (!dev_connected) {
			// --------set status

			//---------do a listen -- 2 second timeout
		}
	}


	// --------unset global values and deallocate memory
	device_common_remove(loc_device_addr);
	iop_thread_stop_request[loc_device_addr] = 0;

	ExitThread(0);

	return 0;

}


// TODO: this routine could be made generic
void device_console_init(unsigned __int16 device_address) {

	DEVICE_CONSOLE_DATA* device_data = 0;
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
			sizeof(DEVICE_CONSOLE_DATA));

		if (iop_device_buffer[device_address] == NULL) {
			printf("\n *** ERROR ***  Could not allocate memory for device address %02x.  Device not created.\n", device_address);
		}
		else {

			// --------fill in global data
			device_data = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];

			device_data->ctrl_status = 0x8080;
			que_uword_init(&(device_data->ctrl_command_que));
			device_data->ctrl_wake = 0;
			device_data->ctrl_input_buffer_count = 0;
			device_data->ctrl_input_buffer_index = 0;
			device_data->ctrl_output_buffer_count = 0;
			device_data->ctrl_output_buffer_index = 0;

			// --------assign IO callbacks.
			iop_output_data_proc[device_address] = device_console_output_data;
			iop_output_cmd_proc[device_address] = device_console_output_cmd;
			iop_input_data_proc[device_address] = device_console_input_data;
			iop_input_status_proc[device_address] = device_console_input_status;

			// --------start thread
			iop_thread_stop_request[device_address] = 0;

			device_thread = CreateThread(
				NULL,					// default security attributes
				0,							// use default stack size  
				device_console_worker_thread,	// thread function name
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
