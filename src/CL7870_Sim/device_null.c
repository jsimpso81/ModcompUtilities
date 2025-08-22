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

#include <process.h>
#include <stdio.h>


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

void device_null_load_sal(DEVICE_NULL_DATA* device_data);
void device_null_load_sal_new(DEVICE_NULL_DATA * device_data);


// ============================================================================================================================
void  device_null_output_data(SIMJ_U16 device_address, SIMJ_U16 data_value) {

	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];

	char junk = data_value & 0x00ff;

	//printf("\n\n device null output data -- 0x%02hx\n\n", data_value);

	// --------for now just write character to stdout, no buffering.
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
void  device_null_output_cmd(SIMJ_U16 device_address, SIMJ_U16 cmd_value) {

	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];

	//printf("\n device_null output cmd -- called - %04x\n", cmd_value);

	// --------this is a command - set the controller busy.
	databuffer->ctrl_status |= (status_busy);

	// --------add the command to the work queue.
	que_uword_send(&(databuffer->ctrl_command_que), cmd_value);
	
	// ---------wake the thread to process this command.
	databuffer->ctrl_wake++;
	WakeByAddressSingle((LPVOID) & (databuffer->ctrl_wake));
}

// ============================================================================================================================
SIMJ_U16  device_null_input_data(SIMJ_U16 device_address) {

	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];
	SIMJ_U16 ourvalue = 0;
	SIMJ_U8 ourbyte = 0;

	device_common_buffer_get(&databuffer->in_buff, &ourbyte);

	ourvalue = ourbyte;

	// --------If buffer is empty,, set data_not_ready flag in status word.
	if (device_common_buffer_isempty(&databuffer->in_buff) ) {
		databuffer->ctrl_status |= (status_data_not_ready);
	}

	fprintf(stderr, " device_null input data -- called - 0x%04x, index %d \n",ourvalue, databuffer->in_buff.last_byte_read_index );

	return ourvalue;
}

// ============================================================================================================================
SIMJ_U16  device_null_input_status(SIMJ_U16 device_address) {

	DEVICE_NULL_DATA* databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];

	SIMJ_U16 loc_status;

	// --------get current control status and return to user.
	loc_status = databuffer->ctrl_status;

	// printf("\n device_null input status -- called - 0x%04x\n", loc_status);
	return loc_status;
}


// ============================================================================================================================
DWORD WINAPI device_null_worker_thread(LPVOID lpParam) {

	SIMJ_U16 loc_device_addr = 0;
	DEVICE_NULL_DATA* device_data = 0;


	bool dev_reading = false;
	bool dev_writing = false;
	bool si_enabled = false;
	bool di_enabled = false;

	SIMJ_U16 last_wake = 0;
	SIMJ_U16 cmd_type;
	SIMJ_U16 loc_status = 0;
	SIMJ_U16 orig_status = 0;
	SIMJ_U16 loc_cmd = 0;


	// -------- get local device address from calling parameter to this routine 
	loc_device_addr = *(SIMJ_U16*)lpParam;
	printf(" Starting device null at device address %d\n", loc_device_addr);
	if (loc_device_addr == 0) {
		printf("\n *** ERROR *** Thread didn't get device address \n");
	}

	// -------- get pointer to device data.
	device_data = (DEVICE_NULL_DATA*)(iop_device_buffer[loc_device_addr]);

	// --------disable si and di
	si_enabled = false;
	di_enabled = false;

	// -------- set initial data
	loc_status = (status_exists | status_data_not_ready);
	device_data->ctrl_status = loc_status;


	// device_null_load_sal( device_data );
	device_null_load_sal_new( device_data );


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
							fprintf(stderr, " Device null - terminate w/ICB requested: 0x%04x.\n", loc_cmd );
							dev_reading = false;
							dev_writing = false;
							//				console_input_buffer_count = 0;
							//				console_input_buffer_index = 0;
							loc_status |= (status_data_not_ready);
							loc_status &= (~status_busy);
							si_enabled = false;
							di_enabled = false;
							// device_data->ctrl_input_buffer_count = 0;
							// device_data->ctrl_input_buffer_index = 0;
							// device_data->ctrl_output_buffer_count = 0;
							// device_data->ctrl_output_buffer_index = 0;
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
								fprintf(stderr, " Device null - invalid noop break select command 0x%04x \n", loc_cmd);
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
			// if (dev_reading) {
				if ( !device_common_buffer_isempty( &device_data->in_buff) ) {
					loc_status &= (~(status_busy | status_data_not_ready));
				}
				else {
					loc_status |= status_data_not_ready;
				}
			//}

			// --------if writing and buffer space available, update status.
			// if (dev_writing) {
				// if (device_data->ctrl_output_buffer_count > device_data->ctrl_output_buffer_index) {
				//	loc_status &= (~(status_busy | status_data_not_ready));
				// }
			//}


			// --------update device status
			device_data->ctrl_status = loc_status;
			// if (loc_status != orig_status)
			//	printf("\n Device null status updated 0x%04x\n", our_status);

		}

		// --------all commands processed, indicate not busy.
		loc_status &= (~status_busy);

		// -------- if reading and data available, update status.
		//if (dev_reading) {
			if (!device_common_buffer_isempty(&device_data->in_buff)) {
				loc_status &= (~( status_busy | status_data_not_ready));
			}
			else {
				loc_status |= status_data_not_ready;
			}
		//}

		// --------if writing and buffer space available, update status.
		//if (dev_writing) {
			// if (device_data->ctrl_output_buffer_count > device_data->ctrl_output_buffer_index) {
			//	loc_status &= (~(status_busy | status_data_not_ready));
			// }
		//}
		
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

	printf(" Device worker thread exit. Device address %d\n", loc_device_addr);

	// --  ExitThread(0);
	_endthreadex(0);

	return 0;

}

// ============================================================================================================================
// --------initialize the device.  calls common routines.  Only custom thing is to initialize the 
// --------data buffer after it is created.
void device_null_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp) {

	DEVICE_NULL_DATA* device_data = 0;
	SIMJ_U16 loc_dev_addr;

	loc_dev_addr = device_address;

	// --------make certain we aren't double allocating a device and allocate buffer memory for device.
	device_data = device_common_device_buffer_allocate(device_address, sizeof( DEVICE_NULL_DATA) );

	if ( device_data != NULL ) {

		// -------- fill in global buffer.
		// -------- standard information for ALL controllers
		device_data->device_address = loc_dev_addr;
		device_data->ctrl_status = 0x8080;
		que_uword_init(&(device_data->ctrl_command_que));
		device_data->ctrl_wake = 0;
		device_data->bus = bus;
		device_data->pri = prio;
		device_data->dmp = dmp;
		strcpy_s(device_data->info, 40, "Null (console)");

		// --------data specific to this device.
		strcpy_s(device_data->filename, 255, "");
		device_data->ipport = 0;
		device_common_buffer_init(&device_data->in_buff);
		device_common_buffer_init(&device_data->out_buff);

		// --------initialize worker thread.
		device_common_thread_init( (LPVOID)device_data, 
								device_null_worker_thread, 
								device_null_output_data, 
								device_null_output_cmd, 
								device_null_input_data, 
								device_null_input_status);

	}
}

// ================================================================================================
void device_null_load_sal( DEVICE_NULL_DATA* device_data ) {
	// ========================================== Buffer for booting =============================================================================
	// --------add specific bytes for different I/O cards.  For now use 4809... (these differences may be ignorned
	// --------anyway by the console controller.
	//	FIL100(B3LDR) = Z'8000'
	//	FIL100(B4LDR) = Z'8080'
	//	FIL100(SMBTI) = Z'8023'

#include "device_null_initial_boot_block.h"
#include "device_null_salprep.h"
#include "device_null_sal.h"

	int j;
	int maxj;

	union {
		SIMJ_U8 byte[2];
		SIMJ_S16 word;
		SIMJ_U16 uword;
	} chksum = { .word = 0 };

	union {
		SIMJ_U8 byte[2];
		SIMJ_S16 word;
		SIMJ_U16 uword;
	} chksum2 = { .word = 0 };

	union {
		SIMJ_U8 byte[2];
		SIMJ_S16 word;
		SIMJ_U16 uword;
	} chksum3 = { .word = 0 };

	union {
		SIMJ_U8 byte[2];
		SIMJ_S16 word;
		SIMJ_U16 uword;
	} chksum_neg = { .word = 0 };


	union {
		SIMJ_U8 byte[2];
		SIMJ_S16 word;
		SIMJ_U16 uword;
	} tmp16 = { .word = 0 };



	// --------calculate word checksum of 100 boot block.
	chksum.word = 0;
	maxj = sizeof(boot100_data);
	for (j = 0; j < maxj; j += 2) {
		tmp16.byte[1] = boot100_data[j];
		tmp16.byte[0] = boot100_data[j + 1];
		chksum.uword += tmp16.uword;
	}
	chksum_neg.word = -1 * chksum.word;	// so it adds up to zero.
	fprintf(stderr, " 100 block checksum = 0x%04hx\n", chksum.uword);

	// --------store in buffer.
	// boot100_data[maxj - 4] = chksum_neg.byte[1];
	// boot100_data[maxj - 3] = chksum_neg.byte[0];

	// --------copy initial boot block to device input data global.
	maxj = sizeof(initial_boot_block);
	for (j = 0; j < maxj; j++) {
		device_common_buffer_put(&device_data->in_buff, initial_boot_block[j]);
	}
	fprintf(stderr, " initial boot block - bytes %d, buffer end %d\n", maxj, device_data->in_buff.last_byte_writen_index);

	// --------copy 100 boot block start and size to device input data global.
	maxj = sizeof(boot100_loc_size);
	for (j = 0; j < maxj; j++) {
		device_common_buffer_put(&device_data->in_buff, boot100_loc_size[j]);
	}
	fprintf(stderr, " 100 boot block offset / size  - bytes %d, buffer end %d\n", maxj, device_data->in_buff.last_byte_writen_index);

	// --------copy 100 boot block to device input data global.
	maxj = sizeof(boot100_data);
	for (j = 0; j < maxj; j++) {
		device_common_buffer_put(&device_data->in_buff, boot100_data[j]);
	}
	fprintf(stderr, " 100 boot block   - bytes %d, buffer end %d\n", maxj, device_data->in_buff.last_byte_writen_index);

	// boot100_data[maxj - 4] = chksum_neg.byte[1];
	// boot100_data[maxj - 3] = chksum_neg.byte[0];
	fprintf(stderr, " 100 block checksum = 0x%04hx\n", chksum_neg.word);

	device_common_buffer_put(&device_data->in_buff, chksum_neg.byte[1]);
	device_common_buffer_put(&device_data->in_buff, chksum_neg.byte[0]);

	// -------- add zero to end....
	device_common_buffer_put(&device_data->in_buff, 0);
	device_common_buffer_put(&device_data->in_buff, 0);

	// --------copy salprep (400) boot block end and length to device input data global.
	// maxj = sizeof(salprep_loc_size);
	// for (j = 0; j < maxj; j++) {
	// 	device_common_buffer_put(&device_data->in_buff, salprep_loc_size[j]);
	// }
	// fprintf(stderr, " salprep - loc /size bytes %d, buffer end %d\n", maxj, device_data->in_buff.last_byte_writen_index);

	// --------copy salprep (400) boot block to device input data global.
	maxj = sizeof(salprep_data);
	for (j = 0; j < maxj; j++) {
		device_common_buffer_put(&device_data->in_buff, salprep_data[j]);
	}
	fprintf(stderr, " salprep - bytes %d, buffer end %d\n", maxj, device_data->in_buff.last_byte_writen_index);

	// --------calculate word checksum of 400 boot block.
	chksum2.uword = 0;
	maxj = sizeof(salprep_data);
	for (j = 0; j < maxj; j += 2) {
		tmp16.byte[1] = salprep_data[j];
		tmp16.byte[0] = salprep_data[j + 1];
		chksum2.uword += tmp16.uword;
	}

	chksum.word = chksum2.word * -1;

	fprintf(stderr, " 400 block (salprep) checksum = 0x%04hx\n", chksum.uword);

	//  device_common_buffer_put(&device_data->in_buff, chksum2.byte[1] );
	//  device_common_buffer_put(&device_data->in_buff, chksum2.byte[0] );

	// -------- add zero to end....
	//  device_common_buffer_put(&device_data->in_buff,0 );
	//  device_common_buffer_put(&device_data->in_buff,0 );

	// --------copy sal  to device input data global.
	maxj = sizeof(sal_data);
	for (j = 0; j < maxj; j++) {
		device_common_buffer_put(&device_data->in_buff, sal_data[j]);
	}
	fprintf(stderr, " sal  - bytes %d, buffer end %d\n", maxj, device_data->in_buff.last_byte_writen_index);

	// --------calculate word checksum of sal block.
	// -------- this is a running checksum so dont start at zero.
	chksum3.uword = 0;
	maxj = sizeof(sal_data);
	for (j = 0; j < maxj; j += 2) {
		tmp16.byte[1] = sal_data[j];
		tmp16.byte[0] = sal_data[j + 1];
		chksum3.uword += tmp16.uword;
	}

	chksum.uword = chksum3.word * -1;

	fprintf(stderr, " sal checksum = 0x%04hx\n", chksum.uword);

	chksum.uword = chksum2.uword + chksum3.uword;
	chksum.word *= -1;
	fprintf(stderr, " 400 boot (salprep) and sal overall checksum = 0x%04hx\n", chksum.uword);

	// --------copy running 400 (salprep) boot and sal checksum to input buffer.
	device_common_buffer_put(&device_data->in_buff, chksum.byte[1]);
	device_common_buffer_put(&device_data->in_buff, chksum.byte[0]);
	device_common_buffer_put(&device_data->in_buff, 0);
	device_common_buffer_put(&device_data->in_buff, 0);


	fprintf(stderr, " Null device -- input buffer data filled size = %d\n", device_data->in_buff.last_byte_writen_index);

}


// ================================================================================================
void device_null_load_sal_new(DEVICE_NULL_DATA* device_data) {

#include "device_null_sal_new.h"

	int maxj;
	int j;

	// --------copy sal  to device input data global.
	maxj = sizeof(sal_data);
	for (j = 0; j < maxj; j++) {
		device_common_buffer_put(&device_data->in_buff, sal_data[j]);
	}
	fprintf(stderr, " salnew  - bytes %d, buffer end %d\n", maxj, device_data->in_buff.last_byte_writen_index);

}