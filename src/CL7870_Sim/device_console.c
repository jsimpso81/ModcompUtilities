g// -------- CONSOLE DEVICE -- VIA COMM PORT

#include "simj_base.h"

#include <process.h>
#include <stdio.h>
#include <stdbool.h>


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


void device_console_process_command(SIMJ_U16 loc_cmd, DEVICE_CONSOLE_DATA* device_data);


// ============================================================================================================================
void  device_console_output_data(SIMJ_U16 device_address, SIMJ_U16 data_value) {

	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];

	SIMJ_U8 junk = data_value & 0x00ff;
	// -------- just some diagnostics.
	// SIMJ_U8 junk2 = (SIMJ_U8)((data_value >> 8) & 0x00ff);
	// if (junk2 != 0) {
	// 	fprintf(stderr, " Console output data, high byte not zero: 0x%04x\n", data_value);
	// }

	device_common_buffer_put(&databuffer->out_buff, junk);

	// --------if writing and buffer isn't empty
	if (databuffer->write_in_progress && !device_common_buffer_isempty(&databuffer->out_buff)) {

		// -------- Request ownership of the resource.
		TAKE_RESOURCE( databuffer->ResourceStatusUpdate );
		// -------set data not ready.
		databuffer->ctrl_status |= status_data_not_ready;
		// -------- Release ownership of the resource.
		GIVE_RESOURCE( databuffer->ResourceStatusUpdate );
	}

	// --------wake up comm thread.
	databuffer->ctrl_wake++;
	WakeByAddressSingle((PVOID) & (databuffer->ctrl_wake));

	// --------allow other threads to run
	// SwitchToThread();

}

// ============================================================================================================================
void  device_console_output_cmd(SIMJ_U16 device_address, SIMJ_U16 cmd_value) {

	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];

	//printf("\n device_console output cmd -- called - %04x\n", cmd_value);

	// --------process the command
	device_console_process_command(cmd_value, databuffer);

	// --------allow other threads to run  -- NOT SURE IF HELPFUL
	SwitchToThread();

}

// ============================================================================================================================
SIMJ_U16  device_console_input_data(SIMJ_U16 device_address) {

	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];
	SIMJ_U16 ourvalue = 0;
	SIMJ_U8 ourbyte = 0;
	bool new_data = false;

	// --------if there is a byte, get it.
	new_data = device_common_buffer_get(&databuffer->in_buff, &ourbyte);

	ourvalue = ourbyte;

	// --------If buffer is empty,, set data_not_ready flag in status word.
	if (databuffer->read_in_progress && device_common_buffer_isempty(&databuffer->in_buff)) {
		// -------- Request ownership of the resource.
		TAKE_RESOURCE( databuffer->ResourceStatusUpdate );
		databuffer->ctrl_status |= status_data_not_ready;
		// -------- Release ownership of the resource.
		GIVE_RESOURCE( databuffer->ResourceStatusUpdate );
	}

	// fprintf(stderr," device_console input data -- called - 0x%04x, index %d, new: %s \n", ourvalue, databuffer->in_buff.last_byte_read_index, (new_data ? "New  " : "Empty"));

	// --------wake up comm thread.
	databuffer->ctrl_wake++;
	WakeByAddressSingle( (PVOID) & (databuffer->ctrl_wake));

	return ourvalue;
}

// ============================================================================================================================
SIMJ_U16  device_console_input_status(SIMJ_U16 device_address) {

	DEVICE_CONSOLE_DATA* databuffer = (DEVICE_CONSOLE_DATA*)iop_device_buffer[device_address];

	SIMJ_U16 loc_status;
	SIMJ_U16 loc_status1;


	// --------allow other threads to run
	// SwitchToThread();

	// -------- Request ownership of the resource.
	TAKE_RESOURCE( databuffer->ResourceStatusUpdate );

	// --------get current control status and return to user.
	loc_status = databuffer->ctrl_status;

	loc_status1 = loc_status;

	// -------- if reading and data available, update status.
	if (databuffer->read_in_progress) {
		if (device_common_buffer_isempty(&databuffer->in_buff)) {
			loc_status |= status_data_not_ready; // no data to read.
		}
		else {
			loc_status &= (~status_data_not_ready); // something to read.
		}
	}

	// --------if writing and buffer space available, update status.
	if (databuffer->write_in_progress) {
		if (!device_common_buffer_isempty(&databuffer->out_buff)) {
			loc_status |= status_data_not_ready; // cant write
		}
		else {
			loc_status &= (~status_data_not_ready); // can write
		}
	}

	if (loc_status1 != loc_status) {
		databuffer->ctrl_status = loc_status;
	}
	// -------- Release ownership of the resource.
	GIVE_RESOURCE( databuffer->ResourceStatusUpdate );

	// printf("\n device_console input status -- called - 0x%04x\n", loc_status);

	return loc_status;
}


// ============================================================================================================================
DWORD WINAPI device_console_comm_worker_thread(LPVOID lpParam) {

	SIMJ_U16 loc_device_addr = 0;
	DEVICE_CONSOLE_DATA* device_data = 0;
	SIMJ_U16 last_wake = 0;
	unsigned int com_state = 0;
	HANDLE loc_comm_handle = NULL;
	SIMJ_U8 loc_read_data[2000] = { 0 };
	DWORD desired_read_bytes = 0;
	DWORD actual_read_bytes = 0;
	BOOL read_status = false;
	SIMJ_U8 loc_write_data[2000] = { 0 };
	DWORD bytes_to_write = 0;
	DWORD bytes_written = 0;
	BOOL write_status = false;
	DWORD j = 0;
	BOOL status;



	// -------- get local device address from calling parameter to this routine 
	loc_device_addr = *(SIMJ_U16*)lpParam;


	printf(" Starting device console communications thread at device address %d\n", loc_device_addr);
	if (loc_device_addr == 0) {
		printf(" Console device thread didn't get device address \n");
	}

	// -------- get pointer to device data.
	device_data = (DEVICE_CONSOLE_DATA*)(iop_device_buffer[loc_device_addr]);

	// -------- raise the priority of this thread.
	status = SetThreadPriority( (HANDLE)iop_device_thread_handle2[loc_device_addr], THREAD_PRIORITY_ABOVE_NORMAL);
	if (!status) {
		printf(" *** ERROR *** Failed to raise priority of console communications thread.  Status 0x%08x\n", GetLastError());
	}

	// -------- get local comm handle
	loc_comm_handle = device_data->com_handle;

	int loc_stop_request = 0;

	// TODO: Serial comm -- THis is way to crude with too many data copies potentially high latency.  Fix.

	// -------------------------------------------------------------------------------------------
	// -------- do forever, until requested to stop and exit.
	while ((iop_thread_stop_request2[loc_device_addr] == 0) && (loc_stop_request == 0)) {

		last_wake = device_data->ctrl_wake;

		switch (com_state) {

			// --------init -- do flush, start read.
			case 0:

				// --------set next normal state
				com_state = 1;
				break;

			// -------- DO IO.
			case 1:

				// --------if write in progress, do a write
				// --------for now do any outstanding writes regardless of IO in progress status.  
				// --------cpu may have turned around IO operation before buffer is cleared...
				//if (device_data->write_in_progress) {

				// --------if a write is needed, do the write.
				bytes_to_write = 0;

				// -------- for now limit bytes written to 500
				// TODO: Console calculate correct write timeout for different baud rates.
				while (!device_common_buffer_isempty(&device_data->out_buff) && bytes_to_write < 500) {
					if (device_common_buffer_get(&device_data->out_buff, &loc_write_data[bytes_to_write])) {
						// --------DEBUG
						fprintf(stderr,"%c", loc_write_data[bytes_to_write]);
						// --------END DEBUG
						bytes_to_write++;
					}
				}

				if (bytes_to_write > 0) {
					write_status = WriteFile(loc_comm_handle, &loc_write_data, bytes_to_write,
						&bytes_written, NULL);
					// fprintf(stderr, " Console bytes write requested %d, written %d.  Device Addr %d\n", bytes_to_write, bytes_written, loc_device_addr);

					// -------- Request ownership of the resource.
					TAKE_RESOURCE( device_data->ResourceStatusUpdate );

					// --------signal buffer not full -- ready for more.
					if (device_data->write_in_progress) {
						device_data->ctrl_status &= (~status_data_not_ready);
					}
					// -------- Release ownership of the resource.
					GIVE_RESOURCE( device_data->ResourceStatusUpdate );

					// --------initiate DI to get more
					if (device_data->write_in_progress && device_data->DI_enabled) {
						cpu_request_DI(device_data->bus, device_data->pri, device_data->device_address);
					}
				}

				// -------- Request ownership of the resource.
				TAKE_RESOURCE( device_data->ResourceStatusUpdate );

				// --------signal buffer not full -- ready for more. -- JUST IN CASE.
				if (device_data->write_in_progress && device_common_buffer_isempty(&device_data->out_buff)) {
					device_data->ctrl_status &= (~status_data_not_ready);
				}
				// -------- Release ownership of the resource.
				GIVE_RESOURCE( device_data->ResourceStatusUpdate );

				//}

				// --------if read in progress, do a read.  --- only do a read if buffer is empty.  This way interrupts can keep up!
				if ( device_data->read_in_progress  && device_common_buffer_isempty( &device_data->in_buff ) ) {

					// --------do a read.
					desired_read_bytes = 1;			// 50;
					actual_read_bytes = 0;
					read_status = ReadFile(loc_comm_handle, &loc_read_data,
						desired_read_bytes, &actual_read_bytes, NULL);

					// --------got a byte
					if (actual_read_bytes > 0) {
						// fprintf(stderr, " Console bytes read %d.  Device Addr %d\n", actual_read_bytes, loc_device_addr);
						for (j = 0; j < actual_read_bytes; j++) {
							device_common_buffer_put(&device_data->in_buff, loc_read_data[j]);
							if (gbl_capture_console) {
								device_common_capture_console(loc_read_data[j]);
							}
						}

						// -------- Request ownership of the resource.
						TAKE_RESOURCE( device_data->ResourceStatusUpdate );

						// --------signal data ready.
						device_data->ctrl_status &= (~status_data_not_ready);

						// -------- Release ownership of the resource.
						GIVE_RESOURCE( device_data->ResourceStatusUpdate );

						// --------initiate DI so they can process this byte.
						if (device_data->DI_enabled) {
							cpu_request_DI(device_data->bus, device_data->pri, device_data->device_address);
						}

						// --------echo to console --- for now ignore error.
						bytes_to_write = 1;
						write_status = WriteFile(loc_comm_handle, &loc_read_data, bytes_to_write,
							&bytes_written, NULL);
					}

					// --------check for error.
					if (!read_status) {
						DWORD my_last_error = 0;
						my_last_error = GetLastError();
						fprintf(stderr, " Console read error 0x%08x\n", my_last_error);
					}
				}

				break;

			case 99:
				loc_stop_request = 1;
				break;
		}

		// --------if no read currently in progress, wait for timeout or a new request, (wait built into read.  don't need two waits.)
		if (!device_data->read_in_progress) {
			WaitOnAddress(&(device_data->ctrl_wake), &last_wake, sizeof(last_wake), (DWORD)50);
		}
	}

	// --------unset global values and deallocate memory
	iop_thread_stop_request2[loc_device_addr] = 0;

	printf(" Device comm thread exit. Device address %d\n", loc_device_addr);

	//  --  ExitThread(0);
	_endthreadex(0);

	return 0;

}



// ============================================================================================================================
DWORD WINAPI device_console_worker_thread(LPVOID lpParam) {

	SIMJ_U16 loc_device_addr = 0;
	DEVICE_CONSOLE_DATA* device_data = 0;

	// bool dev_reading = false;
	// bool dev_writing = false;
	// bool si_enabled = false;
	// bool di_enabled = false;

	SIMJ_U16 last_wake = 0;
	// SIMJ_U16 loc_status = 0;
	// SIMJ_U16 orig_status = 0;
	// SIMJ_U16 loc_cmd = 0;


	DWORD last_error = 0;
	int open_status = 1;
	int set_param_status = 1;

	DWORD comm_thread_id = 0;
	uintptr_t comm_thread_handle = 0;

	HANDLE loc_com_device = NULL;
	BOOL status;


	// -------- get local device address from calling parameter to this routine 
	loc_device_addr = *(SIMJ_U16*)lpParam;
	printf(" Starting device console at device address %d\n", loc_device_addr);
	if (loc_device_addr == 0) {
		printf(" *** ERROR *** Console device thread didn't get device address \n");
	}

	// -------- get pointer to device data.
	device_data = (DEVICE_CONSOLE_DATA*)(iop_device_buffer[loc_device_addr]);

	// -------- raise the priority of this thread.
	status = SetThreadPriority((HANDLE)iop_device_thread_handle[loc_device_addr], THREAD_PRIORITY_ABOVE_NORMAL);
	if (!status) {
		printf(" *** ERROR *** Failed to raise priority of console worker thread.  Status 0x%08x\n", GetLastError());
	}

	// --------disable si and di
	device_data->SI_enabled = false;
	device_data->DI_enabled = false;

	// -------- set initial data
	device_data->ctrl_status = (status_exists | status_data_not_ready);
	device_common_buffer_set_empty(&device_data->in_buff);  // for now don't clear the input buffer.
	device_common_buffer_set_empty(&device_data->out_buff);

	// --------Open com port
	open_status = device_common_serial_open("COM1", &loc_com_device, &last_error);

	// -------- if com port was opened..
	if (open_status == 0) {

		// --------set global com handle for other thread.
		device_data->com_handle = loc_com_device;

		// -------- set com port parameters
		set_param_status = device_common_serial_set_params(loc_com_device, &last_error, true);

		if (set_param_status != 0) {
			printf(" *** ERROR ***  Trouble setting com port parameters.\n");
		}

		// -------- Configure read and write operations to time out after 100 ms.
		COMMTIMEOUTS timeouts = { 0 };
		timeouts.ReadIntervalTimeout = 0;
		timeouts.ReadTotalTimeoutConstant = 50;	// ms -- too large on purpose to help half duplex turn around.
		timeouts.ReadTotalTimeoutMultiplier = 0;
		timeouts.WriteTotalTimeoutConstant = 1200;	// ms
		timeouts.WriteTotalTimeoutMultiplier = 0;

		BOOL success = SetCommTimeouts(loc_com_device, &timeouts);
		if (!success) {
			printf(" *** ERROR ***  Failed to set serial timeouts\n");
			// CloseHandle(port);
			// return INVALID_HANDLE_VALUE;
		}

		// -------- set comm port driver buffer sizes.
		success = SetupComm(loc_com_device, 16384, 50);
		if (!success) {
			printf(" *** ERROR ***  Failed to set serial port buffers\n");
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
		if (comm_thread_handle != 0) {
			printf(" Console device at device address  %02x communications thread created.\n", loc_device_addr);
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
		// orig_status = (loc_status = device_data->ctrl_status);

		// --------process commands
		//  while (que_uword_recv(&(device_data->ctrl_command_que), &loc_cmd)) {

		//	last_wake = device_data->ctrl_wake;


		//	device_console_process_command( SIMJ_U16 loc_cmd, DEVICE_CONSOLE_DATA* device_data );


		// }

		// --------all commands processed, indicate not busy.
		// loc_status &= (~status_busy);

		// -------- if reading and data available, update status.
		// if (dev_reading) {
		//  	if (device_common_buffer_isempty(&device_data->in_buff)) {
		//		loc_status |= status_data_not_ready;
		//	}
		//	else {
		//		loc_status &= ~status_data_not_ready;
		//	}
		//}

		// --------if writing and buffer space available, update status.
		//if (dev_writing) {
		//	if (device_common_buffer_isfull(&device_data->out_buff)) {
		//		loc_status |= status_data_not_ready;
		//	}
		//	else {
		//		loc_status &= ~status_data_not_ready;
		//	}
		//}

		// --------update device status
		//device_data->ctrl_status = loc_status;
		// if ( our_status != orig_status )
		//	printf("\n Device console status updated 0x%04x\n", our_status);

		// --------wait for timeout or a new request.
		//  WaitOnAddress(&(device_data->ctrl_wake), &last_wake, sizeof(last_wake), (DWORD)200);
		Sleep(100);
	}

	// ------- it is asked to exit on its own... No need to do this here.
	// --------request comm thread to stop
	// iop_thread_stop_request2[loc_device_addr] = 1;

	// --------wait a little for comm thread to exit.
	Sleep(200);

	// --------try to close com port
	if ( loc_com_device != NULL )
		device_common_serial_close(loc_com_device, &last_error);

	// --------initialize the resource for updating status.
	// Initialize the resource one time only.
	DELETE_RESOURCE( device_data->ResourceStatusUpdate );

	// --------unset global values and deallocate memory
	device_common_remove(loc_device_addr);
	iop_thread_stop_request[loc_device_addr] = 0;

	printf(" Device worker thread exit. Device address %d\n", loc_device_addr);

	//  --  ExitThread(0);
	_endthreadex(0);

	return 0;

}




// ============================================================================================================================
// --------initialize the device.  calls common routines.  Only custom thing is to initialize the 
// --------data buffer after it is created.
void device_console_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp) {

	DEVICE_CONSOLE_DATA* device_data = 0;
	SIMJ_U16 loc_dev_addr;
	loc_dev_addr = device_address;
	bool status = false;

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

		// --------initialize the resource for updating status.
		// Initialize the resource one time only.
		status = INIT_RESOURCE( device_data->ResourceStatusUpdate );
		if (!status) {
			printf(" *** ERROR *** Console device could not create status update locking mechanism.\n");
		}



		// --------initialize main worker thread.
		device_common_thread_init((LPVOID)device_data,
			device_console_worker_thread,
			device_console_output_data,
			device_console_output_cmd,
			device_console_input_data,
			device_console_input_status);
	}
}



// ============================================================================================================================
void device_console_process_command(SIMJ_U16 loc_cmd, DEVICE_CONSOLE_DATA* device_data) {

	SIMJ_U16 cmd_type = 0;
	SIMJ_U16 orig_status = 0;
	SIMJ_U16 loc_status = 0;
	bool old_read = false;
	bool old_write = false;
	bool need_SI = false;
	bool need_DI = false;
	bool msg_term_icb = false;
	bool msg_unexpected_cmd = false;
	int chg_di = 0;
	int chg_si = 0;
	int chg_wrt = 0;
	int chg_rd = 0;

	// --------get the type of command.
	cmd_type = loc_cmd & cmd_mask;

	// -------- Request ownership of the resource.
	TAKE_RESOURCE( device_data->ResourceStatusUpdate );

	// --------get internal status to work on...
	loc_status = device_data->ctrl_status;
	orig_status = loc_status;

	// --------process the various command types.
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

				// --------stop all I/O
				chg_rd = -1;
				chg_wrt = -1;

				// --------update status
				loc_status |= (status_data_not_ready);		// set no data ready
				loc_status &= (~status_busy);				// clear busy


				// --------TERMINATE w/ICB   
				if ((loc_cmd & ctrl_icb) != 0) {
					msg_term_icb = true;

					// --------disable interrupts --- ARE WE CERTAIN??
					// TODO: Should interrupts be disabled on terminate w/ICB?
					// chg_si = -1;
					// chg_di = -1;

					// --------clear buffers
					// device_common_buffer_set_empty(&device_data->in_buff);  // for now don't clear the input buffer.
					// device_common_buffer_set_empty(&device_data->out_buff);
				}

				// --------TERMINATE
				else {
					// fprintf(stderr, " Device console - terminate requested. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);

					// --------clear buffers
					// TODO: Console investigate clearing output buffer on terminate 
					// device_common_buffer_set_empty(&device_data->in_buff);
					// device_common_buffer_set_empty(&device_data->out_buff);
				}

				// --------generate SI if enabled.
				if (device_data->SI_enabled || ( chg_si == 1 )) {
					need_SI = true;
				}
			}


			// -------- NOOP Command
			// --	The No Op command is a control command that causes
			// --	an enable or disable of Data or SI levels as specified by
			// --	bits 2 and 3. 
			// --
			// --	When the CTC is "not busy" No Op resets all		
			// -- 	controller status indicators.
			// --
			// -- 	The NO OP command also enables or disables the
			// -- 	Break Detection logic in the controller.
			// -- 	When bit 7 is set and 13 reset, the command will cause
			// -- 	the enabling or disabling of Break Detection as specified
			// -- 	by bit 10. Enabling of Break Detection will cause the CTC
			// -- 	to set Break Detect(status bit 10) and if connected,
			// -- 	generate an SI when connected.Refer to table 3 - 1 for use
			// -- 	of bits 7, 10 and 13.
			// --
			// --		brk_sel & !brk_det & !si_rel = disable brk det
			// --		brk_sel & brk_det & !si_reg = enable brk de
			// --		brk_sel & !brk_det & si_rel = si release
			// --
			// --	Break detection is independent of the currently
			// --  	programmed transfer direction, if the Controller is busy.
			// --  	The CTC is enabled for break detection and a break is
			// --  	detected at the same time that a "normal" SI occurs.If the
			// --  	order of the two interrupts is indeterminate, the program
			// --  	must test the CTC's status to determine the cause of the
			// --  	SI.
			// --
			// --  	NOTE: With Break Detection disabled, there is no
			// --  	question as to the cause of an SI.Therefore an
			// --  	SI Release command is not needed under these
			// --  	conditions.
			// --
			// --  	When Break Detection is enabled with SI disabled the
			// --  	CTC will set Break status upon Break Detection but will
			// --  	not require an SI Release command.

			else {
				// fprintf(stderr, " Device console - NOOP Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);

				// -------- enable or disable interrupts.
				chg_si = ( (loc_cmd & ctrl_si_enable ) != 0) ? 1 : -1;
				chg_di = ( (loc_cmd & ctrl_di_enable ) != 0) ? 1 : -1;

				// -------- if not busy reset all status indications ??
				// TODO: figure this out.
				if (loc_status & (~status_busy)) {
					loc_status &= (~status_break);		// reset break...
					// TODO: Any other bits to reset??
				}

				// -------- process break detect.
				if (loc_cmd & ctrl_break_select) {
					switch (loc_cmd & (ctrl_enable_brk_det | ctrl_si_rel)) {

					// -------- disable break detect.
					case 0:
						device_data->break_detect_enabled = false;
						loc_status &= (~status_break);
						break;

					// -------- enable break detect.
					case ctrl_enable_brk_det:
						device_data->break_detect_enabled = true;
						loc_status &= (~status_break);		// in case it is on, turn it off.
						break;

					// -------- SI release
					// TODO: Does SI release do anything else?
					case ctrl_si_rel:
						loc_status &= (~status_break);		// in case it is on, turn it off.
						break;

					// -------- This is enable break detect and SI release.  (Do both??, Do nothing??)  For now, do both.
					default:
						device_data->break_detect_enabled = true;
						loc_status &= (~status_break);		// in case it is on, turn it off.
						fprintf(stderr, " Device console - invalid noop break select command 0x%04x \n", loc_cmd);
					}
				}

				// --------generate SI if enabled.
				if (device_data->SI_enabled  || ( chg_si == 1) ) {
					need_SI = true;
				}

			}
			break;

		// --------transfer initiate
		case cmd_transfer_initiate:

			// -------- enable or disable interrupts.
			chg_si = ((loc_cmd & ctrl_si_enable) != 0) ? 1 : -1;
			chg_di = ((loc_cmd & ctrl_di_enable) != 0) ? 1 : -1;

			// --------for some reason the console is backwards !		
			// --------start a write.
			if (!(loc_cmd & transinit_write)) {
				// fprintf(stderr, " Device console - transfer initiate - write requested.  Dev addr: %d, cmd 0x%04x\n",device_data->device_address, loc_cmd);

				// --------get old write status
				old_write = device_data->write_in_progress;

				// -------- indicate transfer in progress
				chg_rd = -1;
				chg_wrt = 1;
				loc_status |= status_busy;

				// -------- if ready for a byte send DI.   If we weren't already doing a write, send DI to get data.
				if ( (device_data->DI_enabled || ( chg_di == 1 )) && !old_write ) {
					need_DI = true;
				}
			}
			// --------start a read.
			else {
				// fprintf(stderr, " Device console - transfer initiate - read requested.  Dev addr: %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);

				// -------- get old read status
				old_read = device_data->read_in_progress;

				// -------- indicate transfer in progress
				chg_rd = 1;
				chg_wrt = -1;
				loc_status |= status_busy;
			}
			// --------what interrupts are caused by transfer initiate???   (Always SI to signal completion??,   DI when starting write?)

			// --------generate SI if enabled.
			//if (device_data->SI_enabled) {
			//	need_SI = true;
			//}
			break;

		// --------unexpected command...
		default:
			msg_unexpected_cmd = true;

		}		// -------- END OF COMMAND PROCESSING


		if (chg_si == 1) {
			device_data->SI_enabled = true;
		}
		if (chg_di == 1) {
			device_data->DI_enabled = true;
		}

		// --------change SI DI status
		if (chg_si == -1) {
			device_data->SI_enabled = false;
		}
		if (chg_di == -1) {
			device_data->DI_enabled = false;
		}
		if (chg_rd == -1) {
			device_data->read_in_progress = false;
		}
		if (chg_rd == 1) {
			device_data->read_in_progress = true;
		}
		if (chg_wrt == -1) {
			device_data->write_in_progress = false;
		}
		if (chg_wrt == 1) {
			device_data->write_in_progress = true;
		}


		// -------- if reading and data available, update status.
		if (device_data->read_in_progress) {
			if (device_common_buffer_isempty(&device_data->in_buff)) {
				loc_status |= status_data_not_ready; // no data to read.
			}
			else {
				loc_status &= (~status_data_not_ready); // something to read.
			}
		}

		// --------if writing and buffer space available, update status.
		if (device_data->write_in_progress) {
			if (device_common_buffer_isfull(&device_data->out_buff)) {
				loc_status |= status_data_not_ready; // cant write
			}
			else {
				loc_status &= (~status_data_not_ready); // can write
			}
		}

		// --------update device status
		device_data->ctrl_status = loc_status;


		// -------- Release ownership of the resource.
		GIVE_RESOURCE( device_data->ResourceStatusUpdate );

		if ( msg_term_icb)
			fprintf(stderr, " Device console - terminate w/ICB requested. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);

		if ( msg_unexpected_cmd )
			fprintf(stderr, " Device console - unexpected command.  Dev addr: %d,  cmd 0x%04x\n", device_data->device_address, loc_cmd);


		// -------- generate interrupts if needed.
		if (need_SI) {
			cpu_request_SI(device_data->bus, device_data->pri, device_data->device_address);
		}
		if (need_DI) {
			cpu_request_DI(device_data->bus, device_data->pri, device_data->device_address);
		}
		// if (loc_status != orig_status)
		//	printf("\n Device console status updated 0x%04x\n", our_status);

}