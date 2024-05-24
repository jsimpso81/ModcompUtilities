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

#define ctrl_di_enable			0x2000
#define ctrl_si_enable			0x1000
#define ctrl_terminate			0x0400
#define ctrl_abort				0x0100
#define ctrl_break_select		0x0100
#define ctrl_icb				0x0040
#define ctrl_enable_brk_det		0x0020
#define ctrl_si_rel				0x0004

#define transinit_write			0x0800



// ============================================================================================================================
void  device_null_output_data(unsigned __int16 device_address, unsigned __int16 data_value) {
	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];
	char junk = data_value & 0x00ff;
	//printf("\n\n device null output data -- 0x%02hx\n\n", data_value);
	putchar(junk);
	//if (databuffer->ctrl_output_buffer_index >= DEVICE_NULL_MAX_BUFFER) {
	//	printf("\n Null device output buffer overflow.\n");
	//} 
	//else {
	//	databuffer->ctrl_output_buffer[databuffer->ctrl_output_buffer_index++] = (0x00ff & data_value);
	//	databuffer->ctrl_wake++;
	//	WakeByAddressSingle(&(databuffer->ctrl_wake));
	//}

}

// ============================================================================================================================
void  device_null_output_cmd(unsigned __int16 device_address, unsigned __int16 cmd_value) {
	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];
	//printf("\n device_null output cmd -- called - %04x\n", cmd_value);
	databuffer->ctrl_status |= (status_busy);
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
	}
	if (databuffer->ctrl_input_buffer_count <= databuffer->ctrl_input_buffer_index) {
		databuffer->ctrl_status |= (status_data_not_ready);
	}

	printf("\n device_null input data -- called - 0x%04x, index %d \n",ourvalue, (databuffer->ctrl_input_buffer_index-1) );
	return ourvalue;
}

// ============================================================================================================================
unsigned __int16  device_null_input_status(unsigned __int16 device_address) {
	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];
	unsigned __int16 loc_status;
	loc_status = databuffer->ctrl_status;
	// printf("\n device_null input status -- called - 0x%04x\n", loc_status);
	return loc_status;
}


// ============================================================================================================================
DWORD WINAPI device_null_worker_thread(LPVOID lpParam) {

	unsigned __int16 loc_device_addr = 0;
	DEVICE_NULL_DATA* device_data = 0;

#include "device_null_initial_boot_block.h"
#include "device_null_salprep.h"
#include "device_null_sal.h"

	int next_char_inx_to_read = 0;
	int last_char_inx_in_string = 338;
	bool dev_reading = false;
	bool dev_writing = false;
	bool si_enabled = false;
	bool di_enabled = false;

	unsigned __int16 last_wake = 0;
	unsigned __int16 cmd_type;
	unsigned __int16 loc_status = 0;
	unsigned __int16 orig_status = 0;
	unsigned __int16 loc_cmd = 0;

	int j;
	int maxj;

	union {
		unsigned __int8 byte[2];
		signed __int16 word;
		unsigned __int16 uword;
	} chksum = { .word = 0 };

	union {
		unsigned __int8 byte[2];
		signed __int16 word;
		unsigned __int16 uword;
	} chksum2 = { .word = 0 };


	union {
		unsigned __int8 byte[2];
		signed __int16 word;
		unsigned __int16 uword;
	} chksum_neg = { .word = 0 };


	union {
		unsigned __int8 byte[2];
		signed __int16 word;
		unsigned __int16 uword;
	} tmp16 = { .word = 0 };


	// -------- get local device address from calling parameter to this routine 
	loc_device_addr = *(unsigned __int16*)lpParam;
	printf("\n Starting device null at device address %d\n", loc_device_addr);
	if (loc_device_addr == 0) {
		printf("\n thread didn't get device address \n");
	}

	// -------- get pointer to device data.
	device_data = (DEVICE_NULL_DATA*)(iop_device_buffer[loc_device_addr]);

	// --------disable si and di
	si_enabled = false;
	di_enabled = false;

	// -------- set initial data
	loc_status = (status_exists | status_data_not_ready);
	device_data->ctrl_status = loc_status;


	// --------add specific bytes for different I/O cards.  For now use 4809... (these differences may be ignorned
	// --------anyway by the console controller.
	//	FIL100(B3LDR) = Z'8000'
	//	FIL100(B4LDR) = Z'8080'
	//	FIL100(SMBTI) = Z'8023'

	// --------this is to calculate a running checksum that is sent after all of SAL is sent.
	chksum.word = 0;

	// --------calculate word checksum of 100 boot block.
	maxj = sizeof(boot100_data);
	for (j = 0; j < maxj; j += 2) {
		tmp16.byte[1] = boot100_data[j];	
		tmp16.byte[0] = boot100_data[j+1];
		chksum.uword += tmp16.uword;
	}
	chksum_neg.word = -1 * chksum.word;	// so it adds up to zero.
	printf("\n 100 block checksum = 0x%04hx\n", chksum.uword);

	boot100_data[maxj - 4] = chksum_neg.byte[1];
	boot100_data[maxj - 3] = chksum_neg.byte[0];


	// --------copy initial boot block to device input data global.
	maxj = sizeof(initial_boot_block);
	for (j = 0; j < maxj; j++) {
		device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = initial_boot_block[j];
	}
	printf("\n initial boot block - bytes %d, buffer end %d\n", maxj, device_data->ctrl_input_buffer_count-1);
	
	// --------copy 100 boot block start and size to device input data global.
	maxj = sizeof(boot100_loc_size);
	for (j = 0; j < maxj; j++) {
		device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = boot100_loc_size[j];
	}
	printf("\n 100 boot block offset / size  - bytes %d, buffer end %d\n", maxj, device_data->ctrl_input_buffer_count - 1);


	// --------copy 100 boot block to device input data global.
	maxj = sizeof(boot100_data);
	for (j = 0; j < maxj; j++) {
		device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = boot100_data[j];
	}
	printf("\n 100 boot block   - bytes %d, buffer end %d\n", maxj, device_data->ctrl_input_buffer_count - 1);
	
	// --------copy salprep (400) boot block to device input data global.
	maxj = sizeof(salprep_data);
	for (j = 0; j < maxj; j++) {
		device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = salprep_data[j];
	}
	printf("\n salprep - bytes %d, buffer end %d\n", maxj, device_data->ctrl_input_buffer_count - 1);

	// --------calculate word checksum of 400 boot block.
	chksum2.uword = 0;
	maxj = sizeof(salprep_data);
	for (j = 0; j < maxj; j += 2) {
		tmp16.byte[1] = salprep_data[j];
		tmp16.byte[0] = salprep_data[j + 1];
		chksum2.uword += tmp16.uword;
	}

	chksum.uword = chksum2.uword;

	chksum2.word *= -1;
	printf("\n 400 block (salprep) checksum = 0x%04hx\n", chksum2.uword);

	device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = chksum2.byte[1];
	device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = chksum2.byte[0];
	// device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = 0;
	//device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = 0;



	/*
	// --------copy sal  to device input data global.
	maxj = sizeof(sal_data);
	for (j = 0; j < maxj; j++) {
		device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = sal_data[j];
	}
	printf("\n sal  - bytes %d, buffer end %d\n", maxj, device_data->ctrl_input_buffer_count - 1);
	*/

	// --------calculate word checksum of 400 boot block.
	chksum2.uword = 0;
	maxj = sizeof(sal_data);
	for (j = 0; j < maxj; j += 2) {
		tmp16.byte[1] = sal_data[j];
		tmp16.byte[0] = sal_data[j + 1];
		chksum2.uword += tmp16.uword;
	}

	chksum.uword += chksum2.uword;
	chksum.word *= -1;

	chksum2.word *= -1;
	printf("\n sal checksum = 0x%04hx\n", chksum2.uword);

	printf("\n over all checksum = 0x%04hx\n", chksum.uword);

	/*
	// --------copy running 400 boot and sal checksum to input buffer.
	device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = chksum.byte[1];
	device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = chksum.byte[0];
	device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = 0;
	device_data->ctrl_input_buffer[device_data->ctrl_input_buffer_count++] = 0;

	*/
	printf("\n Null device -- input buffer data filled size = %d\n", device_data->ctrl_input_buffer_count);

	// -------------------------------------------------------------------------------------------
	// -------- do forever, until requested to stop and exit.
	while (iop_thread_stop_request[loc_device_addr] == 0) {

		last_wake = device_data->ctrl_wake;

		// --------get internal status to work on...
		orig_status = (loc_status = device_data->ctrl_status);

		// --------process commands
		while ( que_uword_recv(&(device_data->ctrl_command_que), &loc_cmd) ) {

			last_wake = device_data->ctrl_wake;

			// --------get internal status to work on...
			orig_status = (loc_status = device_data->ctrl_status);

			cmd_type = loc_cmd & cmd_mask;

			switch (cmd_type) {

				// --------command
				case cmd_control:

					// --------terminate
					// -- The Terminate command causes an immediate halt of the transfer during an I/O Data
					// -- transfer operation.  Completes any other required actions associated with the transfer, 
					// -- goes Not Busy, and generates a Service Interrupt(SI) if enabled.
					// -- While the not busy, issuance of a Terminate(bit 5 set) will cause the generation of 
					// -- an SI, if enabled.  The CTC responds identically to a normal Terminate or an
					// -- abort Terminate with bit 7 set.
					if ((loc_cmd & ctrl_terminate) != 0) {
						if ((loc_cmd & ctrl_icb) != 0) {
							// printf("\n Device null - terminate w/ICB requested.\n");
							dev_reading = false;
							dev_writing = false;
							//				console_input_buffer_count = 0;
							//				console_input_buffer_index = 0;
							loc_status |= (status_data_not_ready);
							loc_status &= (~status_busy);
							si_enabled = false;
							di_enabled = false;
							device_data->ctrl_input_buffer_count = 0;
							device_data->ctrl_input_buffer_index = 0;
							device_data->ctrl_output_buffer_count = 0;
							device_data->ctrl_output_buffer_index = 0;
						}
						else {
							// printf("\n Device null - terminate requested.\n");
							dev_reading = false;
							dev_writing = false;
							//				console_input_buffer_count = 0;
							//				console_input_buffer_index = 0;
							loc_status |= (status_data_not_ready);
							loc_status &= (~status_busy);
							if (si_enabled) {
								// TODO: Cause SI
							}
						}
					}



					// -------- NOOP Command
					// --  The No Op command is a control command that causes
					// -- an enable or disable of Data or SI levels as specified by
					// -- 	bits 2 and 3. When the CTC is "not busy" No Op resets all
					// -- 	controller status indicators.
					// -- 	The NO OP command also enables or disables the
					// -- 	Break Detection logic in the controller.
					// -- 	When bit 7 is set and 13 reset, the command will cause
					// -- 	the enabling or disabling of Break Detection as specified
					// -- 	by bit 10. Enabling of Break Detection will cause the CTC
					// -- 	to set Break Detect(status bit 10) and if connected,
					// -- 		generate an SI when connected.Refer to table 3 - 1 for use
					// -- 		of bits 7, 10 and 13.
					// --
					// --  brk_sel & !brk_det & !si_rel = disable brk det
					// --  brk_sel & brk_det & !si_reg = enable brk de
					// --  brk_sel & !brk_det & si_rel = si release
					// --  Break detection is independent of the currently
					// --  	programmed transfer direction, if the Controller is busy.
					// --  	The CTC is enabled for break detection and a break is
					// --  	detected at the same time that a "normal" SI occurs.If the
					// --  	order of the two interrupts is indeterminate, the program
					// --  	must test the CTC's status to determine the cause of the
					// --  	SI.
					// --  	NOTE: With Break Detection disabled, there is no
					// --  	question as to the cause of an SI.Therefore an
					// --  	SI Release command is not needed under these
					// --  	conditions.
					// --  	When Break Detection is enabled with SI disabled the
					// --  	CTC will set Break status upon Break Detection but will
					// --  	not require an SI Release command.

					else {
						si_enabled = (loc_cmd & ctrl_si_enable) ? true : false;
						di_enabled = (loc_cmd & ctrl_di_enable) ? true : false;
						if (loc_status & ( ~status_busy )) {
							// TODO: reset status bits
						}
						if (loc_cmd & ctrl_break_select) {
							switch (loc_cmd & (ctrl_enable_brk_det | ctrl_si_rel)) {
							case 0:
								break;
							case ctrl_enable_brk_det:
								break;
							case ctrl_si_rel:
								break;
							default:
								printf("\n Device null - invalid noop break select command 0x%04x \n", loc_cmd);
							}
						}

					}
					break;

				// --------transfer initiate
				case cmd_transfer_initiate:

					if ( loc_cmd & transinit_write ) {
						// printf("\n Device null - transfer initiate - write requested.\n");
						dev_reading = false;
						dev_writing = true;	// not full duplex?? so can't do both.
					}
					else {
						// printf("\n Device null - transfer initiate - read requested.\n");
						dev_reading = true;
						dev_writing = false;	// not full duplex?? so can't do both.
						loc_status |= status_data_not_ready;
					}
					break;
			
			}
			// -------- if reading and data available, update status.
			if (dev_reading) {
				if (device_data->ctrl_input_buffer_count > device_data->ctrl_input_buffer_index) {
					loc_status &= (~(status_busy | status_data_not_ready));
				}
				else {
					loc_status |= status_data_not_ready;
				}
			}

			// --------if writing and buffer space available, update status.
			if (dev_writing) {
				// if (device_data->ctrl_output_buffer_count > device_data->ctrl_output_buffer_index) {
					loc_status &= (~(status_busy | status_data_not_ready));
				// }
			}


			// --------update device status
			device_data->ctrl_status = loc_status;
			// if (loc_status != orig_status)
			//	printf("\n Device null status updated 0x%04x\n", our_status);

		}

		// --------all commands processed, indicate not busy.
		loc_status &= (~status_busy);

		// -------- if reading and data available, update status.
		if (dev_reading) {
			if (device_data->ctrl_input_buffer_count > device_data->ctrl_input_buffer_index) {
				loc_status &= (~( status_busy | status_data_not_ready));
			}
			else {
				loc_status |= status_data_not_ready;
			}
		}

		// --------if writing and buffer space available, update status.
		if (dev_writing) {
			// if (device_data->ctrl_output_buffer_count > device_data->ctrl_output_buffer_index) {
				loc_status &= (~(status_busy | status_data_not_ready));
			// }
		}
		// --------update device status
		device_data->ctrl_status = loc_status;
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

	loc_dev_addr = device_address;

	// --------make certain we aren't double allocating a device and allocate buffer memory for device.
	device_data = device_common_buffer_allocate(device_address, sizeof( DEVICE_NULL_DATA) );

	if ( device_data != NULL ) {

		// -------- fill in global buffer.
		device_data->device_address = loc_dev_addr;
		device_data->ctrl_status = 0x8080;
		que_uword_init(&(device_data->ctrl_command_que));
		device_data->ctrl_wake = 0;
		device_data->ctrl_input_buffer_count = 0;
		device_data->ctrl_input_buffer_index = 0;
		device_data->ctrl_output_buffer_count = 0;
		device_data->ctrl_output_buffer_index = 0;

		// --------initialize worker thread.
		device_common_thread_init( (LPVOID)device_data, 
								device_null_worker_thread, 
								device_null_output_data, 
								device_null_output_cmd, 
								device_null_input_data, 
								device_null_input_status);

	}
}
