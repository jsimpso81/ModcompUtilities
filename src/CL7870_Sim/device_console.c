// -------- CONSOLE DEVICE -- VIA COMM PORT

#include <windows.h>
#include <stdio.h>
#include <stdbool.h>

#include "modcomp_sim_types.h"
#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"


// -------- DEVICE CONSOLE

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
void  device_console_output_data(unsigned __int16 device_address, unsigned __int16 data_value) {

	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];

	unsigned __int8 junk = data_value & 0x00ff;

	device_common_buffer_put(&databuffer->out_buff, junk);
	if (device_common_buffer_isfull(&databuffer->out_buff)) {
		databuffer->ctrl_status |= status_data_not_ready;
	}

	//printf("\n\n device console output data -- 0x%02hx\n\n", data_value);

	//	databuffer->ctrl_wake++;
	//	WakeByAddressSingle(&(databuffer->ctrl_wake));

}

// ============================================================================================================================
void  device_console_output_cmd(unsigned __int16 device_address, unsigned __int16 cmd_value) {

	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];

	//printf("\n device_console output cmd -- called - %04x\n", cmd_value);

	// --------this is a command - set the controller busy.
	databuffer->ctrl_status |= (status_busy);

	// --------add the command to the work queue.
	que_uword_send(&(databuffer->ctrl_command_que), cmd_value);

	// ---------wake the thread to process this command.
	databuffer->ctrl_wake++;
	WakeByAddressSingle((PVOID) & (databuffer->ctrl_wake));
}

// ============================================================================================================================
unsigned __int16  device_console_input_data(unsigned __int16 device_address) {

	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];
	unsigned __int16 ourvalue = 0;
	unsigned __int8 ourbyte = 0;

	device_common_buffer_get(&databuffer->in_buff, &ourbyte);

	ourvalue = ourbyte;

	// --------If buffer is empty,, set data_not_ready flag in status word.
	if (device_common_buffer_isempty(&databuffer->in_buff)) {
		databuffer->ctrl_status |= (status_data_not_ready);
	}

	printf("\n device_null input data -- called - 0x%04x, index %d \n", ourvalue, databuffer->in_buff.last_byte_read_index);

	return ourvalue;
}

// ============================================================================================================================
unsigned __int16  device_console_input_status(unsigned __int16 device_address) {

	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];

	unsigned __int16 loc_status;

	// --------get current control status and return to user.
	loc_status = databuffer->ctrl_status;

	// printf("\n device_console input status -- called - 0x%04x\n", loc_status);
	return loc_status;
}


// ============================================================================================================================
DWORD WINAPI device_console_comm_worker_thread(LPVOID lpParam) {

	unsigned __int16 loc_device_addr = 0;
	DEVICE_CONSOLE_DATA* device_data = 0;
	unsigned __int16 last_wake = 0;
	unsigned int com_state = 0;
	HANDLE loc_comm_handle = NULL;
	unsigned __int8 loc_read_data[2000] = { 0 };
	DWORD desired_read_bytes = 0;
	DWORD actual_read_bytes = 0;
	BOOL read_status = false;
	unsigned __int8 loc_write_data[2000] = { 0 };
	DWORD bytes_to_write = 0;
	DWORD bytes_written = 0;
	BOOL write_status = false;

	// -------- get local device address from calling parameter to this routine 
	loc_device_addr = *(unsigned __int16*)lpParam;
	printf("\n Starting device console communications thread at device address %d\n", loc_device_addr);
	if (loc_device_addr == 0) {
		printf("\n thread didn't get device address \n");
	}

	// -------- get pointer to device data.
	device_data = (DEVICE_CONSOLE_DATA*)(iop_device_buffer[loc_device_addr]);

	// -------- get local comm handle
	loc_comm_handle = device_data->com_handle;

	int loc_stop_request = 0;

	// TODO: Serial comm -- THis is way to crude with too many data copies potentially high latency.  Fix.

	// -------------------------------------------------------------------------------------------
	// -------- do forever, until requested to stop and exit.
	while ((iop_thread_stop_request[loc_device_addr] == 0) && (loc_stop_request == 0)) {

		last_wake = device_data->ctrl_wake;


		switch (com_state) {

			// --------init -- do flush, start read.
			case 0:

				// --------set next normal state
				com_state = 1;
				break;

			// -------- start a read.
			case 1:

				// --------do a read.
				desired_read_bytes = 1;
				actual_read_bytes = 0;
				read_status = ReadFile(loc_comm_handle, &loc_read_data, 
									desired_read_bytes, &actual_read_bytes,NULL );
				if (actual_read_bytes > 0) {
					printf(" Console bytes read %d.  Device Addr %d\n", actual_read_bytes, loc_device_addr);
					device_common_buffer_put(&device_data->in_buff, loc_read_data[0]);
				}

				// --------if a write is needed, do the write.
				bytes_to_write = 0;
				// -------- for now limit bytes written to 500
				// TODO: Console calculate correct write timeout for different baud rates.
				while  (!device_common_buffer_isempty(&device_data->out_buff) && bytes_to_write < 500 ) {
					if (device_common_buffer_get(&device_data->out_buff, &loc_write_data[bytes_to_write]) ) {
						bytes_to_write++;
					}
				}
				if (bytes_to_write > 0) {
					write_status = WriteFile( loc_comm_handle, &loc_write_data, bytes_to_write, 
								&bytes_written, NULL );
					printf(" Console bytes write requested %d, written %d.  Device Addr %d\n", bytes_to_write, bytes_written, loc_device_addr);
				}

				break;

			case 99:
				loc_stop_request = 1;
				break;
		}

		// --------wait for timeout or a new request.
		// WaitOnAddress(&(device_data->ctrl_wake), &last_wake, sizeof(last_wake), (DWORD)50);
	}

	// --------unset global values and deallocate memory
	iop_thread_stop_request2[loc_device_addr] = 0;

	printf(" Device comm thread exit. Device address %d\n", loc_device_addr);

	ExitThread(0);

	return 0;

}



// ============================================================================================================================
DWORD WINAPI device_console_worker_thread(LPVOID lpParam) {

	unsigned __int16 loc_device_addr = 0;
	DEVICE_CONSOLE_DATA* device_data = 0;

	bool dev_reading = false;
	bool dev_writing = false;
	bool si_enabled = false;
	bool di_enabled = false;

	unsigned __int16 last_wake = 0;
	unsigned __int16 cmd_type;
	unsigned __int16 loc_status = 0;
	unsigned __int16 orig_status = 0;
	unsigned __int16 loc_cmd = 0;


	DWORD last_error = 0;
	int open_status = 1;
	int set_param_status = 1;

	DWORD comm_thread_id = 0;
	HANDLE comm_thread_handle = NULL;

	HANDLE loc_com_device = NULL;



	// -------- get local device address from calling parameter to this routine 
	loc_device_addr = *(unsigned __int16*)lpParam;
	printf("\n Starting device console at device address %d\n", loc_device_addr);
	if (loc_device_addr == 0) {
		printf(" *** ERROR *** Thread didn't get device address \n");
	}

	// -------- get pointer to device data.
	device_data = (DEVICE_CONSOLE_DATA*)(iop_device_buffer[loc_device_addr]);

	// --------disable si and di
	si_enabled = false;
	di_enabled = false;

	// -------- set initial data
	loc_status = (status_exists | status_data_not_ready);
	device_data->ctrl_status = loc_status;

	// --------Open com port
	open_status = device_common_serial_open("COM1", &loc_com_device, &last_error);

	// -------- if com port was opened..
	if (open_status == 0) {

		// --------set global com handle for other thread.
		device_data->com_handle = loc_com_device;

		// -------- set com port parameters
		set_param_status = device_common_serial_set_params(loc_com_device, &last_error);

		if (set_param_status != 0) {
			printf(" *** ERROR ***  Trouble setting com port parameters.\n");
		}

		// -------- Configure read and write operations to time out after 100 ms.
		COMMTIMEOUTS timeouts = { 0 };
		timeouts.ReadIntervalTimeout = 0;
		timeouts.ReadTotalTimeoutConstant = 20;	// ms
		timeouts.ReadTotalTimeoutMultiplier = 0;
		timeouts.WriteTotalTimeoutConstant = 1500;	// ms
		timeouts.WriteTotalTimeoutMultiplier = 0;

		BOOL success = SetCommTimeouts(loc_com_device, &timeouts);
		if (!success) {
			printf(" *** ERROR ***  Failed to set serial timeouts\n");
			// CloseHandle(port);
			// return INVALID_HANDLE_VALUE;
		}

		// Flush away any bytes previously read or written.
		success = FlushFileBuffers(loc_com_device);
		if (!success) {
			printf(" *** ERROR ***  Failed to flush serial port\n");
			//CloseHandle(port);
			//return INVALID_HANDLE_VALUE;
		}


		// --------initialize comm worker thread.
		comm_thread_handle = device_common_start_thread((LPVOID)device_data,
			device_console_comm_worker_thread,
			&comm_thread_id);

		// -------- comm thread created, fill in information.
		if (comm_thread_handle != NULL) {
			printf("\n Device at device address  %02x communications thread created.\n", loc_device_addr);
			iop_device_thread_handle2[loc_device_addr] = comm_thread_handle;
			iop_device_thread_id2[loc_device_addr] = comm_thread_id;
		}
		// --------trouble creating comm worker thread.
		else {
			printf("\n *** ERROR *** Trouble creating communications worker thread for device %02x.  Device not created.\n", loc_device_addr);

			// -------- ask worker thread to stop
			iop_thread_stop_request[loc_device_addr] = 1;

		}
	}
	// -------- error opening com port, stop thread...
	else {
		printf(" *** ERROR ***  Trouble opening com port.  Device being terminated.\n");
		iop_thread_stop_request[loc_device_addr] = 1;
	}

	// -------------------------------------------------------------------------------------------
	// -------- do forever, until requested to stop and exit.
	while (iop_thread_stop_request[loc_device_addr] == 0) {

		last_wake = device_data->ctrl_wake;

		// --------get internal status to work on...
		orig_status = (loc_status = device_data->ctrl_status);

		// --------process commands
		while (que_uword_recv(&(device_data->ctrl_command_que), &loc_cmd)) {

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
						printf(" Device console - terminate w/ICB requested. Dev Addr %d\n",loc_device_addr);
						dev_reading = false;
						dev_writing = false;
						loc_status |= (status_data_not_ready);		// set no data ready
						loc_status &= (~status_busy);				// clear busy
						si_enabled = false;
						di_enabled = false;
						device_common_buffer_set_empty(&device_data->in_buff);
						device_common_buffer_set_empty(&device_data->out_buff);
					}
					else {
						printf(" Device console - terminate requested. Dev Addr %d\n", loc_device_addr);
						dev_reading = false;
						dev_writing = false;
						loc_status |= (status_data_not_ready);
						loc_status &= (~status_busy);
						device_common_buffer_set_empty(&device_data->in_buff);
						// device_common_buffer_set_empty(&device_data->out_buff);
						// TODO: Console investigate clearing output buffer on terminate 
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
					printf(" Device console - NOOP Command. Dev Addr %d\n", loc_device_addr);
					si_enabled = (loc_cmd & ctrl_si_enable) ? true : false;
					di_enabled = (loc_cmd & ctrl_di_enable) ? true : false;
					if (loc_status & (~status_busy)) {
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
							printf(" Device console - invalid noop break select command 0x%04x \n", loc_cmd);
						}
					}

				}
				break;

				// --------transfer initiate
			case cmd_transfer_initiate:

				// --------for some reason the console is backwards !
				// TODO: Investigate console transfer initiate.
				if ( !( loc_cmd & transinit_write ) ) {
					printf(" Device console - transfer initiate - write requested.  Dev addr: %d\n",loc_device_addr);
					dev_reading = false;
					dev_writing = true;	
					loc_status &= ~status_data_not_ready;
				}
				else {
					printf(" Device console - transfer initiate - read requested.  Dev addr: %d\n", loc_device_addr);
					dev_reading = true;
					dev_writing = false;	
					loc_status |= status_data_not_ready;
				}
				break;

			}
			// -------- if reading and data available, update status.
			if (dev_reading) {
				if (!device_common_buffer_isempty(&device_data->in_buff)) {
					loc_status &= ~status_data_not_ready;
				}
				else {
					loc_status |= status_data_not_ready;
				}
			}

			// --------if writing and buffer space available, update status.
			if (dev_writing) {
				if (device_common_buffer_isfull(&device_data->out_buff)) {
					loc_status |= status_data_not_ready;
				}
				else {
					loc_status &= ~status_data_not_ready;
				}
			}


			// --------update device status
			device_data->ctrl_status = loc_status;
			// if (loc_status != orig_status)
			//	printf("\n Device console status updated 0x%04x\n", our_status);

		}

		// --------all commands processed, indicate not busy.
		loc_status &= (~status_busy);

		// -------- if reading and data available, update status.
		if (dev_reading) {
			if (!device_common_buffer_isempty(&device_data->in_buff)) {
				loc_status &= ~status_data_not_ready;
			}
			else {
				loc_status |= status_data_not_ready;
			}
		}

		// --------if writing and buffer space available, update status.
		if (dev_writing) {
			if (device_common_buffer_isfull(&device_data->out_buff)) {
				loc_status |= status_data_not_ready;
			}
			else {
				loc_status &= ~status_data_not_ready;
			}
		}

		// --------update device status
		device_data->ctrl_status = loc_status;
		// if ( our_status != orig_status )
		//	printf("\n Device console status updated 0x%04x\n", our_status);

		// --------wait for timeout or a new request.
		WaitOnAddress(&(device_data->ctrl_wake), &last_wake, sizeof(last_wake), (DWORD)50);
	}

	// --------try to close com port
	if ( loc_com_device != NULL )
		device_common_serial_close(loc_com_device, &last_error);

	// --------unset global values and deallocate memory
	device_common_remove(loc_device_addr);
	iop_thread_stop_request[loc_device_addr] = 0;

	printf(" Device worker thread exit. Device address %d\n", loc_device_addr);

	ExitThread(0);

	return 0;

}




// ============================================================================================================================
// --------initialize the device.  calls common routines.  Only custom thing is to initialize the 
// --------data buffer after it is created.
void device_console_init(unsigned __int16 device_address, unsigned __int16 bus, unsigned __int16 prio, unsigned __int16 dmp) {

	DEVICE_CONSOLE_DATA* device_data = 0;
	unsigned __int16 loc_dev_addr;
	loc_dev_addr = device_address;

	// --------make certain we aren't double allocating a device and allocate buffer memory for device.
	device_data = device_common_device_buffer_allocate(device_address, sizeof(DEVICE_CONSOLE_DATA));

	if (device_data != NULL) {

		// -------- fill in global buffer.
		// -------- standard information for ALL controllers
		device_data->device_address = loc_dev_addr;
		device_data->ctrl_status = 0x8080;
		que_uword_init(&(device_data->ctrl_command_que));
		device_data->ctrl_wake = 0;
		device_data->bus = bus;
		device_data->pri = prio;
		device_data->dmp = dmp;
		strcpy_s(device_data->info, 40, "Console");

		// --------data specific to this device.
		device_data->com_handle = NULL;
		device_common_buffer_init(&device_data->in_buff);
		device_common_buffer_init(&device_data->out_buff);

		// --------initialize main worker thread.
		device_common_thread_init((LPVOID)device_data,
			device_console_worker_thread,
			device_console_output_data,
			device_console_output_cmd,
			device_console_input_data,
			device_console_input_status);
	}
}
