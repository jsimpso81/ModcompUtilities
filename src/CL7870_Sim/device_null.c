#include <windows.h>
#include <stdio.h>

#include "modcomp_sim_types.h"
#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"


// -------- DEVICE NULL

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



// ============================================================================================================================
void  device_null_output_data(unsigned __int16 device_address, unsigned __int16 data_value) {
	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];
	//printf("\n device_null output data -- called - %04x\n", data_value);
	if (databuffer->ctrl_output_buffer_index >= DEVICE_NULL_MAX_BUFFER) {
		printf("\n Null device output buffer overflow.\n");
	} 
	else {
		databuffer->ctrl_output_buffer[databuffer->ctrl_output_buffer_index++] = (0x00ff & data_value);
		databuffer->ctrl_wake++;
		WakeByAddressSingle(&(databuffer->ctrl_wake));
	}

}

// ============================================================================================================================
void  device_null_output_cmd(unsigned __int16 device_address, unsigned __int16 cmd_value) {
	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];
	//printf("\n device_null output cmd -- called - %04x\n", cmd_value);
	que_uword_send(&(databuffer->ctrl_command_que), cmd_value);
	databuffer->ctrl_wake++;
	WakeByAddressSingle(&(databuffer->ctrl_wake));
}

// ============================================================================================================================
unsigned __int16  device_null_input_data(unsigned __int16 device_address) {
	unsigned __int16 ourvalue = 0;
	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];
	if (databuffer->ctrl_input_buffer_count > databuffer->ctrl_input_buffer_index) {
		ourvalue = databuffer->ctrl_input_buffer[databuffer->ctrl_input_buffer_index++];
		databuffer->ctrl_status |= (status_busy | status_data_not_ready );
	}
	printf("\n device_null input data -- called - 0x%04x\n",ourvalue);
	return ourvalue;
}

// ============================================================================================================================
unsigned __int16  device_null_input_status(unsigned __int16 device_address) {
	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];
	printf("\n device_null input status -- called - 0x%04x\n", databuffer->ctrl_status);
	return databuffer->ctrl_status;
}


// ============================================================================================================================
DWORD WINAPI device_null_worker_thread(LPVOID lpParam) {

	unsigned __int16 loc_device_addr;
	DEVICE_NULL_DATA* device_data = 0;

	// --------data to send to device when requested
	unsigned __int8 read_data[] = { 0x3F, 0x00, 0x00,																// 02
				0xED, 0x40, 0xFF, 0xD2, 0x78, 0x42, 0x00, 0x0F, 0x67, 0x3D, 0x70, 0x2E, 0x00, 0x00, 0x71, 0x4E,		// 18
		//	C---- - (0010)
				0x00, 0x18, 0xE5, 0x20, 0x00, 0x2E, 0xE5, 0x50, 0x00, 0x2F, 0x6D, 0x35, 0x78, 0x33, 0x00, 0x00,		// 34
				0x61, 0x2F, 0xF8, 0x32, 0x70, 0x5F, 0x00, 0x18, 0x7D, 0x33, 0x00, 0x17, 0xE5, 0x60, 0x00, 0x01,		// 50
		//	C---- - (0020)
				0x62, 0x64, 0xED, 0x40, 0x44, 0x00, 0xE6, 0x60, 0x00, 0x25, 0x00, 0x00, 0xFF, 0x02, 0xF7, 0x01,		// 66
				0xF7, 0x01, 0xF7, 0x01, 0x00, 0x00, 0x00, 0x00, 0xF7, 0x01, 0xF7, 0x01, 0x01, 0x80, 0xFF, 0x80,		// 82

		//	C---- - (0100)  // 83
				0xA4, 0x40, 0x00, 0x01, 0xE6, 0x40, 0x01, 0x7A, 0xE6, 0x40, 0x01, 0x71, 0xE6, 0x70, 0x01, 0x7C,
				0x62, 0x44, 0xE6, 0x40, 0x01, 0x78, 0xE6, 0x40, 0x01, 0x75, 0xED, 0x60, 0x01, 0x80, 0x6C, 0xFF,
		//	C---- - (0110)
				0xFE, 0xF6, 0x70, 0x6F, 0x01, 0x10, 0x63, 0x44, 0x62, 0x4F, 0xE6, 0x40, 0x01, 0x17, 0x00, 0x00,
				0x62, 0x4F, 0x63, 0x42, 0xFE, 0x4F, 0xED, 0x20, 0x0F, 0xE7, 0xED, 0x30, 0xF4, 0x19, 0x68, 0x33,
		//	C---- - (0120)
				0xED, 0x60, 0xFE, 0xDE, 0xE7, 0x70, 0x01, 0x71, 0xE7, 0xA0, 0x01, 0x62, 0x70, 0x6F, 0x01, 0x24,
				0xE7, 0x70, 0x01, 0x6E, 0x6C, 0xFF, 0x77, 0x8D, 0x01, 0x37, 0xE5, 0x80, 0x01, 0x43, 0x6D, 0x58,
		//	C---- - (0130)
				0x68, 0x53, 0x60, 0x5D, 0x76, 0x50, 0x01, 0x36, 0x6F, 0x83, 0x61, 0x8D, 0x6F, 0x88, 0xE7, 0x70,
				0x01, 0x71, 0x09, 0x94, 0xE7, 0xA0, 0x01, 0x62, 0x70, 0x8F, 0x01, 0x39, 0x76, 0xFF, 0x01, 0x48,
		//	C---- - (0140)
				0x65, 0xFF, 0x6B, 0x94, 0xEE, 0x90, 0x00, 0x00, 0x6D, 0x89, 0x61, 0x8D, 0x7F, 0x88, 0x01, 0x39,
				0xE7, 0x70, 0x01, 0x6E, 0x65, 0x4D, 0x78, 0x43, 0x01, 0x2D, 0xE7, 0x70, 0x01, 0x71, 0xE7, 0xA0,
		//	C---- - (0150)
				0x01, 0x62, 0x7D, 0x33, 0x01, 0x4F, 0xE7, 0x70, 0x01, 0x6E, 0xED, 0x30, 0xF4, 0x19, 0x6C, 0x44,
				0x61, 0x2F, 0xF8, 0x42, 0x70, 0x3F, 0x01, 0x58, 0x7D, 0x44, 0x01, 0x5F, 0xFF, 0x02, 0x00, 0x00,
		//	C---- - (0160)
				0xE7, 0x70, 0x01, 0x71, 0xE7, 0x70, 0x01, 0x7A, 0x76, 0x45, 0x01, 0x60, 0x76, 0x48, 0x01, 0x62,
				0xE7, 0x70, 0x01, 0x7C, 0x0C, 0x44, 0xAF, 0x42, 0x60, 0x3F, 0xFF, 0x0A, 0xED, 0x40, 0x44, 0x00,
		//	C---- - (0170)
				0xF7, 0x08, 0x00, 0x00, 0x76, 0x47, 0x01, 0x71, 0x65, 0x41, 0x00, 0x00, 0xED, 0x40, 0x00, 0x00,
				0x00, 0x00, 0xFF, 0x07, 0x00, 0x00, 0xFF, 0x07, 0x00, 0x00, 0xFF, 0x07, 0x00, 0x00, 0x00, 0x00,

				0x00 };  // jas added one last null for 

	int next_char_inx_to_read = 0;
	int last_char_inx_in_string = 338;
	bool dev_reading = false;
	bool dev_writing = false;


	unsigned __int16 last_wake = 0;
	unsigned __int16 cmd_type;
	unsigned __int16 our_status = 0;
	unsigned __int16 orig_status = 0;
	unsigned __int16 loc_cmd = 0;

	int j;
	int maxj;
	union {
		unsigned __int8 byte[2];
		signed __int16 word;
	} chksum = { .word = 0 };
	union {
		unsigned __int8 byte[2];
		signed __int16 word;
	} tmp16 = { .word = 0 };

	// -------- get local device address from calling parameter to this routine 
	loc_device_addr = *(unsigned __int16*)lpParam;

	// -------- get pointer to device data.
	device_data = (DEVICE_NULL_DATA*)iop_device_buffer[loc_device_addr];

	// -------- st initial data
	device_data->ctrl_status = status_exists | status_busy | status_data_not_ready;


	// --------add specific bytes for different I/O cards.  For now use 4809... (these differences may be ignorned
	// --------anyway by the console controller.
	//	FIL100(B3LDR) = Z'8000'
	//	FIL100(B4LDR) = Z'8080'
	//	FIL100(SMBTI) = Z'8023'

	chksum.word = 0;
	// --------calculate word checksum of 100 boot block.
	for (j = 83; j < (83 + 256); j += 2) {
		tmp16.byte[0] = read_data[j+1];
		tmp16.byte[1] = read_data[j];
		chksum.word += tmp16.word;
	}
	chksum.word *= -1;
	read_data[83 + 256 - 4] = chksum.byte[1];
	read_data[83 + 256 - 3] = chksum.byte[0];


	// --------copy local data buffer to global buffer.
	maxj = sizeof(read_data);
	for (j = 0; j < maxj; j++) {
		device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = read_data[j];
	}

	// -------- do forever, until requested to stop and exit.
	while (iop_thread_stop_request[loc_device_addr] == 0) {

		last_wake = device_data->ctrl_wake;

		// --------process commands
		while ( que_uword_recv(&(device_data->ctrl_command_que), &loc_cmd) ) {

			last_wake = device_data->ctrl_wake;

			// --------get internal status to work on...
			orig_status = (our_status = device_data->ctrl_status);

			cmd_type = loc_cmd & cmd_mask;

			switch (cmd_type) {

				// --------command
				case cmd_control:

					// --------terminate
					if ((loc_cmd & ctrl_terminate) != 0) {
						printf("\n Device null - terminate requested.\n");
						dev_reading = false;
						dev_writing = false;
		//				console_input_buffer_count = 0;
		//				console_input_buffer_index = 0;
						our_status |= ( status_data_not_ready );
					}
					break;

				// --------transfer initiate
				case cmd_transfer_initiate:

					if (loc_cmd & 0x0800 ) {
						printf("\n Device null - transfer initiate - read requested.\n");
						dev_reading = true;
						dev_writing = false;	// not full duplex?? so can't do both.
						our_status |= status_data_not_ready;
					}
					else {
						printf("\n Device null - transfer initiate - write requested.\n");
						dev_reading = false;
						dev_writing = true;	// not full duplex?? so can't do both.
					}
					break;
			
			}
			// -------- if reading and data available, update status.
			if (dev_reading) {
				if (device_data->ctrl_input_buffer_count > device_data->ctrl_input_buffer_index) {
					our_status &= (~(status_busy | status_data_not_ready));
				}
			}

			// --------if writing and buffer space available, update status.


			// --------update device status
			device_data->ctrl_status = our_status;
			// if (our_status != orig_status)
			//	printf("\n Device null status updated 0x%04x\n", our_status);

		}

		// -------- if reading and data available, update status.
		if (dev_reading) {
			if (device_data->ctrl_input_buffer_count > device_data->ctrl_input_buffer_index) {
				our_status &= (~( status_busy | status_data_not_ready));
			}
		}

		// --------if writing and buffer space available, update status.


		// --------update device status
		device_data->ctrl_status = our_status;
		// if ( our_status != orig_status )
		//	printf("\n Device null status updated 0x%04x\n", our_status);

		// --------wait for timeout or a new request.
		WaitOnAddress(&(device_data->ctrl_wake), &last_wake, sizeof(last_wake), (DWORD)50);
	}

	// --------unset global values and deallocate memory
	device_common_remove(loc_device_addr);
	iop_thread_stop_request[loc_device_addr] = 0;

	ExitThread(0);

	return 0;

}

// ============================================================================================================================
// --------initialize the device.  calls common routines.  Only custom thing is to initialize the 
// --------data buffer after it is created.
void device_null_init(unsigned __int16 device_address) {

	DEVICE_NULL_DATA* device_data = 0;
	unsigned __int16 loc_dev_addr;
	unsigned __int16* thread_dev_ptr;

	loc_dev_addr = device_address;
	thread_dev_ptr = &loc_dev_addr;

	// --------make certain we aren't double allocating a device and allocate buffer memory for device.
	device_data = device_common_buffer_allocate(device_address, sizeof( DEVICE_NULL_DATA) );

	if ( device_data != NULL ) {

		// -------- fill in global buffer.
		device_data->ctrl_status = 0x8080;
		que_uword_init(&(device_data->ctrl_command_que));
		device_data->ctrl_wake = 0;
		device_data->ctrl_input_buffer_count = 0;
		device_data->ctrl_input_buffer_index = 0;
		device_data->ctrl_output_buffer_count = 0;
		device_data->ctrl_output_buffer_index = 0;

		// --------initialize worker thread.
		device_common_thread_init( device_address, device_null_worker_thread, device_null_output_data, device_null_output_cmd, device_null_input_data, device_null_input_status);

	}
}
