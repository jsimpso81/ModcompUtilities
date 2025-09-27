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

// -------- TAPE DEVICE -- Via file.

#include "simj_base.h"

#include <process.h>
#include <stdio.h>
#include <stdbool.h>


// -------- DEVICE TAPE

// --------status
#define tapestatus_exists			0x8000
#define tapestatus_overunder		0x4000		// under/over flow, dmp too small if 0x8000 too.
#define tapestatus_devparity		0x2000		// device parity error
#define tapestatus_inop				0x1000		// device offline or inoperable.
#define tapestatus_memparity		0x0800		// dmp caused parity error.
#define tapestatus_wrilock			0x0400		// illegal write or write locked.
#define tapestatus_stream			0x0200		// in stream mode, else start/stop mode
#define tapestatus_busy				0x0100		// controller busy
#define tapestatus_data_not_ready	0x0080		// data not read.
#define tapestatus_eot				0x0040		// end of tape
#define tapestatus_eof				0x0020		// end of file
#define tapestatus_bot				0x0010		// beginning of tape.
#define tapestatus_rewind			0x0008		// rewinding and online
#define tapestatus_partial			0x0004		// partial word (not even number of bytes.)
#define tapestatus_unitsel_mask		0x0003		// unit selected mask.

// --------command
#define cmd_cmd_mask			0xC6B0		// command mask
//                              1100 0110 1011 0000 -  really 128 commands...
// --------various commands.
#define cmd_cmd_ti				0x8000		// do a transfer initiate command (read or write)
#define cmd_cmd_ti_dmp			0xC000		// do a transfer initiate command w/dmp (read or write)
#define cmd_cmd_noop			0x4000		// do a no-op command
#define cmd_cmd_sel_trans		0x4200		// do a select unit command.
#define cmd_cmd_space			0x4210		// do a space fwd to eor
#define cmd_cmd_weof			0x4220		// do a weof command (gap = 1)
#define cmd_cmd_rew				0x4280		// do a rewind and stay online
#define cmd_cmd_term			0x4400		// do a terminate.
#define cmd_cmd_term_ign		0x4600		// do a terminate with ign set.. (ign??)

//#define cmd_cmd_sel_trans_cont 0x4300		// do a select unit command. with cont scan. (check in cmd)
//#define cmd_cmd_term_mpe		0x4500		// do a terminate with mpe set (generate mpe error?) (check in cmd)
//#define cmd_cmd_term_mpe_ign	0x4700		// do a terminate with both ign and mpe set. (check in cmd)

// --------other masks in command register.
#define cmd_dmp_ena				0x4000		// enable dmp
#define cmd_di_enable			0x2000
#define cmd_si_enable			0x1000
#define cmd_term_eob			0x0800		// terminate end of block modifier...
#define cmd_term_par			0x0100		// terminate mem parity error occured.
#define cmd_term_mc				0x0040		// terminate master clear.
#define cmd_read				0x0800		// read if one, else write.
#define cmd_gap_long			0x0200		// long gap if one, else normal
#define cmd_scs					0x0100		// enable single scan scs if one, else disable
#define cmd_unit_mask			0x0003		// unit mask.
#define cmd_space_mask			0x000C		// mask to get different space cmds.
#define cmd_space_fwdrec		0x0000		// do a space fwd to eor
#define cmd_space_fwdfil		0x0004		// do a space fwd to eof
#define cmd_space_revrec		0x0008		// do a space reverse to eor
#define cmd_space_revfil		0x000C		// do a space reverse to eof
#define cmd_rewind_lockout		0x0100		// do a rewind with offline

// --------internal list of IO commands
#define locIOcmd_read		1
#define locIOcmd_readDMP	2
#define locIOcmd_write		3
#define locIOcmd_writeDMP	4
#define locIOcmd_rewon		5
#define locIOcmd_rewoff		6
#define locIOcmd_bkfile		7
#define locIOcmd_fwdfile	8
#define locIOcmd_bkrec		9
#define locIOcmd_fwdrec		10
#define locIOcmd_weof		11

void device_tape_process_command(SIMJ_U16 loc_cmd, DEVICE_TAPE_DATA* device_data);
void device_tape_dismount_unit(SIMJ_U16 device_address, SIMJ_U16 unit);

volatile SIMJ_U32 junk_debug = 0;	// input_data
volatile SIMJ_U32 junk2_debug = 0;	// put in buffer



// ============================================================================================================================
void device_tape_enable_disable_SI_DI(SIMJ_U16 loc_cmd, DEVICE_TAPE_DATA* device_data, int* chg_si_ena, int* chg_di_ena) {

	// -------- enable or disable interrupts.
	// -------- Enable SI
	if (loc_cmd & cmd_si_enable) {
		if (!device_data->SI_enabled)
			*chg_si_ena = 1;
	}
	// -------- Disable SI
	else {
		if (device_data->SI_enabled)
			*chg_si_ena = -1;
	}
	// -------- Enable DI
	if (loc_cmd & cmd_di_enable) {
		if (!device_data->DI_enabled)
			*chg_di_ena = 1;
	}
	// -------- Disable SI
	else {
		if (device_data->DI_enabled)
			*chg_di_ena = -1;
	}
	return;
}

// ============================================================================================================================
void  device_tape_output_data(SIMJ_U16 device_address, SIMJ_U16 data_value) {

	// --------get the pointer to the data buffer
	DEVICE_TAPE_DATA* databuffer = (DEVICE_TAPE_DATA*)iop_device_buffer[device_address];

	// TODO: Does tape output bytes or words.  Fix...
	// --------just the low byte of the data word is used.
	SIMJ_U8 junk = data_value & 0x00ff;
	// -------- just some diagnostics.
	SIMJ_U8 junk2 = (SIMJ_U8)((data_value >> 8) & 0x00ff);
	if (junk2 != 0) {
		fprintf(stderr, " *** DEBUG *** tape output data, high byte not zero: 0x%04x\n", data_value);
	}
	// -------add the byte to the data buffer.
	device_common_buffer_put(&databuffer->out_buff, junk);

	// --------if writing and buffer isn't empty, set data not ready in status word...
	if (databuffer->write_in_progress && !device_common_buffer_isempty(&databuffer->out_buff)) {

		// -------- Request ownership of the resource.
		TAKE_RESOURCE(databuffer->ResourceStatusUpdate);
		// -------set data not ready.
		databuffer->ctrl_status |= tapestatus_data_not_ready;
		// -------- Release ownership of the resource.
		GIVE_RESOURCE(databuffer->ResourceStatusUpdate);
	}

	// --------wake up comm thread.
	databuffer->ctrl_wake++;
	WakeByAddressSingle((PVOID) & (databuffer->ctrl_wake));

	// --------allow other threads to run -- WHY IS THIS COMMENTED OUT!!!
	// SwitchToThread();

}

// ============================================================================================================================
void  device_tape_output_cmd(SIMJ_U16 device_address, SIMJ_U16 cmd_value) {

	DEVICE_TAPE_DATA* databuffer = (DEVICE_TAPE_DATA*)iop_device_buffer[device_address];

	// printf("\n device_tape output cmd -- called - %04x\n", cmd_value);

	// --------process the command
	device_tape_process_command(cmd_value, databuffer);

}

// ============================================================================================================================
SIMJ_U16  device_tape_input_data(SIMJ_U16 device_address) {

	DEVICE_TAPE_DATA* databuffer = (DEVICE_TAPE_DATA*)iop_device_buffer[device_address];
	SIMJ_U16 ourvalue = 0;
	SIMJ_U16 ourvalue1 = 0;
	//SIMJ_U16 ourvalue2 = 0;
	SIMJ_U8 ourbyte1 = 0;
	//SIMJ_U8 ourbyte2 = 0;
	bool new_data = false;

	// -------- Request ownership of the resource.   FOR DEBUG
	TAKE_RESOURCE(databuffer->ResourceStatusUpdate);
	// --------if there is a byte, get it.
	// --------ODDLY REGISTER IO (at least in the fill is BYTE oriented...)
	new_data = device_common_buffer_get(&databuffer->in_buff, &ourbyte1);
	// new_data = device_common_buffer_get(&databuffer->in_buff, &ourbyte2);

	ourvalue1 = ourbyte1;
	//ourvalue2 = ourbyte2;
	ourvalue = (ourvalue1 & 0x00ff);
	junk_debug++;

	// --------If buffer is empty,, set data_not_ready flag in status word.
	if (databuffer->read_in_progress && device_common_buffer_isempty(&databuffer->in_buff)) {
		// -------- Request ownership of the resource.
		//TAKE_RESOURCE(databuffer->ResourceStatusUpdate);
		databuffer->ctrl_status |= tapestatus_data_not_ready;
		// -------- Release ownership of the resource.
		//GIVE_RESOURCE(databuffer->ResourceStatusUpdate);
		// --------TRIGGER A READ OPERATION TO GET MORE DATA...
		// -------- set new local IO worker command.
		databuffer->io_cmd[databuffer->cur_sel_unit] = locIOcmd_read;
		databuffer->ctrl_wake++;
		WakeByAddressSingle((PVOID) & (databuffer->ctrl_wake));

		// --------allow other threads to run
		//SwitchToThread();
	}
	// -------- Release ownership of the resource.  FOR DEBUG
	GIVE_RESOURCE(databuffer->ResourceStatusUpdate);

	// fprintf(stderr," device_tape input data -- called - 0x%04x, count %d of %d\n", ourvalue,junk_debug,junk2_debug);


	return ourvalue;
}

// ============================================================================================================================
SIMJ_U16  device_tape_input_status(SIMJ_U16 device_address) {

	DEVICE_TAPE_DATA* databuffer = (DEVICE_TAPE_DATA*)iop_device_buffer[device_address];

	SIMJ_U16 loc_status;
	//SIMJ_U16 loc_status1;

	// --------allow other threads to run
	// SwitchToThread();

	// -------- Request ownership of the resource.
	//TAKE_RESOURCE(databuffer->ResourceStatusUpdate);

	// --------get current control status and return to user.
	loc_status = databuffer->ctrl_status;

	//loc_status1 = loc_status;

	// -------- if reading and data available, update status.
	//if (databuffer->read_in_progress) {
	//	if (device_common_buffer_isempty(&databuffer->in_buff)) {
	//		loc_status |= tapestatus_data_not_ready; // no data to read.
	//	}
	//	else {
	//		loc_status &= (~tapestatus_data_not_ready); // something to read.
	//	}
	//}

	// --------if writing and buffer space available, update status.
	//if (databuffer->write_in_progress) {
	//	if (!device_common_buffer_isempty(&databuffer->out_buff)) {
	//		loc_status |= tapestatus_data_not_ready; // cant write
	//	}
	//	else {
	//		loc_status &= (~tapestatus_data_not_ready); // can write
	//	}
	//}

	//if (loc_status1 != loc_status) {
	//	databuffer->ctrl_status = loc_status;
	//}
	// -------- Release ownership of the resource.
	//GIVE_RESOURCE(databuffer->ResourceStatusUpdate);

	// printf("\n device_tape input status -- called - 0x%04x\n", loc_status);

	return loc_status;
}


// ============================================================================================================================
// --------this routine interacts with the local computer tape image files,
static DWORD WINAPI device_tape_local_IO_worker_thread(LPVOID lpParam) {

	SIMJ_U16 loc_device_addr = 0;
	DEVICE_TAPE_DATA* device_data = 0;

	SIMJ_U16 last_wake = 0;
	SIMJ_U32 j = 0;
	BOOL bool_status;
	     
	DWORD last_error = 0;
	int set_param_status = 1;
	int loc_stop_request = 0;
	SIMJ_U16 loc_status = 0;

	int j_unit = 0;
	int loc_cmd = 0;


	// --------tape disk file stuff
	// TODO: move this to heap...
	union {
		unsigned __int8 ubytes[65536];
		unsigned __int16 words[32768];
	} tape_buffer = { 0 };

	// --------local things indexed by unit...
	__int64 current_file_pos[4] = { 0,0,0,0 };		// current byte pos in file...
	SIMJ_U32 bytes_read[4] = { 0,0,0,0 };
	//size_t words_read[4] = { 0,0,0,0 };
	int read_stat[4] = { 0,0,0,0 };
	//errno_t status[4] = { 0,0,0,0 };
	//bool not_done[4] = { true, true, true, true };
	//size_t start_word[4] = { 0,0,0,0 };
	//__int64 word_index[4] = { 0,0,0,0 };
	//int start_byte[4] = { 0,0,0,0 };
	// int end_of_file[4] = { 0,0,0,0 };
	// --------end tape file stuff..

	// -------------------------------------------------------------------------------------------------------------
	// --------initializtion
	// 
	// 

	// -------- get local device address from calling parameter to this routine 
	loc_device_addr = *(SIMJ_U16*)lpParam;
	printf(" Starting device Tape communications thread at device address %d\n", loc_device_addr);
	if (loc_device_addr == 0) {
		printf(" *** ERROR *** Tape device thread didn't get device address \n");
		loc_stop_request = 1;
		iop_thread_stop_request[loc_device_addr] = 1;
	}

	// -------- get pointer to device data.
	device_data = (DEVICE_TAPE_DATA*)(iop_device_buffer[loc_device_addr]);

		// -------- raise the priority of this thread.
	bool_status = SetThreadPriority((HANDLE)iop_device_thread_handle2[loc_device_addr], THREAD_PRIORITY_ABOVE_NORMAL);
	if (!bool_status) {
		printf(" *** ERROR *** Failed to raise priority of tape communications thread.  Status 0x%08x\n", GetLastError());
	}

	// --------tape file stuff
	//not_done[0] = true;
	//current_file_pos[0] = 0;
	//word_index[0] = 0;
	// ---------end tape file stuff..

	// -------------------------------------------------------------------------------------------
	// -------- do forever, until requested to stop and exit.
	while ((iop_thread_stop_request2[loc_device_addr] == 0) && (loc_stop_request == 0)) {

		last_wake = device_data->ctrl_wake;

		// --------loop over all units, see if there is a command to process..
		for (j_unit = 0; j_unit < 4; j_unit++) {

			loc_cmd = device_data->io_cmd[j_unit];
			device_data->io_cmd[j_unit] = 0;	// reset command
			switch (loc_cmd) {

				// --------do a read.  Once done say data is available and cause interrupts..
				case locIOcmd_read:
				case locIOcmd_readDMP:

					bytes_read[j_unit] = 0;
					if (device_data->tape_file_handle[j_unit] != NULL) {
						read_stat[j_unit] = device_common_tape_read_record( (FILE**)&device_data->tape_file_handle[j_unit],
							        &(current_file_pos[j_unit]), &tape_buffer, 
									65536, &(bytes_read[j_unit]), (bool*) &device_data->eof[j_unit]);

#if DEBUG_TAPE >= 1
						printf(" read tape record -- status %d bytes read %d end of file %d\n", read_stat[j_unit],
							bytes_read[j_unit], device_data->eof[j_unit]);
#endif

						// --------copy to buffer.
						if (bytes_read[j_unit] > 0) {
							device_data->eof[j_unit] = false;

							// --------if DMP process...
							if (loc_cmd == locIOcmd_readDMP) {
								//iop_finish_dmp_read(device_data->device_address, device_data->dmp, &(tape_buffer.words[0]), (int)(bytes_read[j_unit] / 2));
								iop_finish_dmp_read(device_data->dmp_virt, device_data->dmp_tc, device_data->dmp_ta,
									device_data->dmp_abs_tc_addr, iop_vdmp_miap_page[device_data->dmp], iop_vdmp_miap_length[device_data->dmp],
									&(tape_buffer.words[0]), (int)(bytes_read[j_unit] / 2));
							}
							// --------REG IO, copy to buffer.
							// TODO: There is only one tape buffer, not one for each unit !!!
							else {
								// -------- Request ownership of the resource.
								TAKE_RESOURCE(device_data->ResourceStatusUpdate);

								// device_common_buffer_set_empty(&device_data->in_buff); // set buffer empty
								// --------swap bytes...
								for (j = 0; j < bytes_read[j_unit]; j+=2) {
									device_common_buffer_put(&device_data->in_buff, tape_buffer.ubytes[j+1]);
									device_common_buffer_put(&device_data->in_buff, tape_buffer.ubytes[j]);
									junk2_debug+=2;
								}
								// -------- Release ownership of the resource.
								GIVE_RESOURCE(device_data->ResourceStatusUpdate);
							}
						}
						// --------got either end of record or end of file  -- same thing for tape
						else {
							device_data->eof[j_unit] = true;
						}
					}
					// --------shouldnt get this but....
					else {
						printf(" *** ERROR ***  read tape record -- tape file handle is bad\n");

					}


					// TODO: fix end of tape and end of file bits!!
					// --------set data ready in status word.
					// -------- Request ownership of the resource.
					TAKE_RESOURCE(device_data->ResourceStatusUpdate);
					// --------signal buffer not full -- ready for more.
					loc_status = device_data->ctrl_status;
					loc_status &= (~(tapestatus_data_not_ready|tapestatus_busy|tapestatus_bot|tapestatus_eot|tapestatus_eof) );
					if (device_data->eof[j_unit]) {
						loc_status |= tapestatus_eof;
					}
					else {
						loc_status &= (~tapestatus_eof);
					}
					device_data->ctrl_status = loc_status;
					// -------- Release ownership of the resource.
					GIVE_RESOURCE(device_data->ResourceStatusUpdate);


					// --------get current control status and return to user.
					//loc_status = databuffer->ctrl_status;

					//loc_status1 = loc_status;

					// -------- if reading and data available, update status.
					//if (databuffer->read_in_progress) {
					//	if (device_common_buffer_isempty(&databuffer->in_buff)) {
					//		loc_status |= tapestatus_data_not_ready; // no data to read.
					//	}
					//	else {
					//		loc_status &= (~tapestatus_data_not_ready); // something to read.
					//	}
					//}

					// --------if writing and buffer space available, update status.
					//if (databuffer->write_in_progress) {
					//	if (!device_common_buffer_isempty(&databuffer->out_buff)) {
					//		loc_status |= tapestatus_data_not_ready; // cant write
					//	}
					//	else {
					//		loc_status &= (~tapestatus_data_not_ready); // can write
					//	}
					//}

					//if (loc_status1 != loc_status) {
					//	databuffer->ctrl_status = loc_status;
					//}
					// -------- Release ownership of the resource.
					//GIVE_RESOURCE(databuffer->ResourceStatusUpdate);




					break;

				// --------do a read.  Once done say data is available and cause interrupts..
				case locIOcmd_write:
				case locIOcmd_writeDMP:
					printf(" device tape WRITE - IO entered -- not finished yet.....\n");
					break;

				// --------REWIND
				case locIOcmd_rewon:
#if DEBUG_TAPE >= 1
					printf(" device tape REWIND ON - IO entered\n");
#endif
					current_file_pos[j_unit] = 0;
					device_data->bot[j_unit] = true;
					device_data->eof[j_unit] = true;

					// --------see where the position was, then set it to the beginning of the file.
					if (device_data->tape_file_handle[j_unit] != NULL) {
						int stat = device_common_tape_rewind( (FILE**) &(device_data->tape_file_handle[j_unit]),
							(SIMJ_TAPE_DPI*) &(device_data->dpi[j_unit]));
#if DEBUG_TAPE >= 1
						printf(" device tape REWIND ON - cur pos %I64d, status %d\n", device_data->dpi[j_unit], stat);
#endif
					}
					TAKE_RESOURCE(device_data->ResourceStatusUpdate);
					// --------signal rewound...
					loc_status = device_data->ctrl_status;
					loc_status &= (~(tapestatus_busy | tapestatus_eot | tapestatus_eof));
					loc_status |= (tapestatus_bot | tapestatus_eof);
					device_data->ctrl_status = loc_status;
					// --------clear buffers
					device_common_buffer_set_empty(&device_data->in_buff);
					device_common_buffer_set_empty(&device_data->out_buff);
					// -------- Release ownership of the resource.
					GIVE_RESOURCE(device_data->ResourceStatusUpdate);


					break;

				case locIOcmd_rewoff:
#if DEBUG_TAPE >= 1
					printf(" device tape REWIND OFF - IO entered\n");
#endif
					current_file_pos[j_unit] = 0;
					device_data->bot[j_unit] = true;
					device_data->eof[j_unit] = true;
					device_data->online[j_unit] = false;

					// --------see where the position was, then set it to the beginning of the file.
					if (device_data->tape_file_handle[j_unit] != NULL) {
						int stat = device_common_tape_rewind( (FILE**)&(device_data->tape_file_handle[j_unit]),
							(SIMJ_TAPE_DPI*)&(device_data->dpi[j_unit]));
#if DEBUG_TAPE >= 1
						printf(" device tape REWIND ON - cur pos %I64d, status %d\n", (device_data->dpi[j_unit]), stat);
#endif
					}

					TAKE_RESOURCE(device_data->ResourceStatusUpdate);
					// --------signal rewound...
					loc_status = device_data->ctrl_status;
					loc_status &= (~( tapestatus_busy | tapestatus_eot | tapestatus_eof));
					loc_status |= (tapestatus_bot | tapestatus_eof);
					device_data->ctrl_status = loc_status;
					// --------clear buffers
					device_common_buffer_set_empty(&device_data->in_buff);
					device_common_buffer_set_empty(&device_data->out_buff);
					// -------- Release ownership of the resource.
					GIVE_RESOURCE(device_data->ResourceStatusUpdate);

					break;

				case locIOcmd_bkfile:
#if DEBUG_TAPE >= 1
					printf(" device tape BCK FILE - IO entered\n");
#endif
					printf(" device tape BCK FILE not implemented yet!\n");
					TAKE_RESOURCE(device_data->ResourceStatusUpdate);
					// --------signal not busy.
					// TODO: set other status EOF EOT BOT
					loc_status = device_data->ctrl_status;
					loc_status &= (~(tapestatus_busy ));
					device_data->ctrl_status = loc_status;
					// --------clear buffers
					device_common_buffer_set_empty(&device_data->in_buff);
					device_common_buffer_set_empty(&device_data->out_buff);
					GIVE_RESOURCE(device_data->ResourceStatusUpdate);
					break;

				case locIOcmd_fwdfile:
#if DEBUG_TAPE >= 1
					printf(" device tape FWD FILE - IO entered\n");
#endif
					printf(" device tape FWD FILE not implemented yet!\n");
					TAKE_RESOURCE(device_data->ResourceStatusUpdate);
					// --------signal not busy.
					// TODO: set other status EOF EOT BOT
					loc_status = device_data->ctrl_status;
					loc_status &= (~(tapestatus_busy));
					device_data->ctrl_status = loc_status;
					// --------clear buffers
					device_common_buffer_set_empty(&device_data->in_buff);
					device_common_buffer_set_empty(&device_data->out_buff);
					GIVE_RESOURCE(device_data->ResourceStatusUpdate);
					break;

				case locIOcmd_bkrec:
#if DEBUG_TAPE >= 1
					printf(" device tape BCK RECORD - IO entered\n");
#endif
					printf(" device tape BCK RECORD not implemented yet!\n");
					TAKE_RESOURCE(device_data->ResourceStatusUpdate);
					// --------signal not busy.
					// TODO: set other status EOF EOT BOT
					loc_status = device_data->ctrl_status;
					loc_status &= (~(tapestatus_busy));
					device_data->ctrl_status = loc_status;
					// --------clear buffers
					device_common_buffer_set_empty(&device_data->in_buff);
					device_common_buffer_set_empty(&device_data->out_buff);
					GIVE_RESOURCE(device_data->ResourceStatusUpdate);
					break;

				case locIOcmd_fwdrec:
#if DEBUG_TAPE >= 1
					printf(" device tape FWD RECORD - IO entered\n");
#endif
					printf(" device tape FWD RECORD not implemented yet!\n");
					TAKE_RESOURCE(device_data->ResourceStatusUpdate);
					// --------signal not busy.
					// TODO: set other status EOF EOT BOT
					loc_status = device_data->ctrl_status;
					loc_status &= (~(tapestatus_busy));
					device_data->ctrl_status = loc_status;
					// --------clear buffers
					device_common_buffer_set_empty(&device_data->in_buff);
					device_common_buffer_set_empty(&device_data->out_buff);
					GIVE_RESOURCE(device_data->ResourceStatusUpdate);
					break;

				case locIOcmd_weof:
#if DEBUG_TAPE >= 1
					printf(" device tape WRT EOF - IO entered\n");
#endif
					printf(" device tape WRT EOF not implemented yet!\n");
					TAKE_RESOURCE(device_data->ResourceStatusUpdate);
					// --------signal not busy.
					// TODO: set other status EOF EOT BOT
					loc_status = device_data->ctrl_status;
					loc_status &= (~(tapestatus_busy));
					device_data->ctrl_status = loc_status;
					// --------clear buffers
					device_common_buffer_set_empty(&device_data->in_buff);
					device_common_buffer_set_empty(&device_data->out_buff);
					GIVE_RESOURCE(device_data->ResourceStatusUpdate);
					break;

				// -------- no outstanding command for this unit
				case 0:
					break;

				default:
					printf(" *** ERROR *** device tape error unexpected IO command %d\n",loc_cmd);
					break;

			} // -- end of switch

		} // --- loop over all units to process commands.

		// TODO: set controller not busy

		// wait for next command...
		WaitOnAddress(&(device_data->ctrl_wake), &last_wake, sizeof(last_wake), (DWORD)20);

	} // ---- end of loop forever.  

	// --------close all units...
	if (device_data->tape_file_handle[0] != NULL) {
		device_tape_dismount_unit(device_data->device_address, (SIMJ_U16)0);
	}
	if (device_data->tape_file_handle[1] != NULL) {
		device_tape_dismount_unit(device_data->device_address, (SIMJ_U16)1);
	}
	if (device_data->tape_file_handle[2] != NULL) {
		device_tape_dismount_unit(device_data->device_address, (SIMJ_U16)2);
	}
	if (device_data->tape_file_handle[3] != NULL) {
		device_tape_dismount_unit(device_data->device_address, (SIMJ_U16)3);
	}

	// --------unset global values and deallocate memory
	iop_thread_stop_request2[loc_device_addr] = 0;

	printf(" Device file handling thread exit. Device address %d\n", loc_device_addr);

	//  --  ExitThread(0);
	_endthreadex(0);

	return 0;

}



// ============================================================================================================================
// --------this routine processes command output values.
DWORD WINAPI device_tape_worker_thread(LPVOID lpParam) {

	SIMJ_U16 loc_device_addr = 0;
	DEVICE_TAPE_DATA* device_data = 0;

	// bool dev_reading = false;
	// bool dev_writing = false;
	// bool si_enabled = false;
	// bool di_enabled = false;

	SIMJ_U16 last_wake = 0;
	// SIMJ_U16 loc_status = 0;
	// SIMJ_U16 orig_status = 0;
	// SIMJ_U16 loc_cmd = 0;


	DWORD comm_thread_id = 0;
	uintptr_t comm_thread_handle = 0;

	BOOL status;


	// -------------------------------------------------------------------------------------------------------------
	// --------initializtion
	// -------- get local device address from calling parameter to this routine 
	loc_device_addr = *(SIMJ_U16*)lpParam;
	printf(" Starting device tape at device address %d\n", loc_device_addr);
	if (loc_device_addr == 0) {
		printf(" *** ERROR *** tape device thread didn't get device address \n");
	}

	// -------- get pointer to device data.
	device_data = (DEVICE_TAPE_DATA*)(iop_device_buffer[loc_device_addr]);

	// -------- raise the priority of this thread.
	status = SetThreadPriority((HANDLE)iop_device_thread_handle[loc_device_addr], THREAD_PRIORITY_ABOVE_NORMAL);
	if (!status) {
		printf(" *** ERROR *** Failed to raise priority of tape worker thread.  Status 0x%08x\n", GetLastError());
	}

	// --------disable si and di
	device_data->SI_enabled = false;
	device_data->DI_enabled = false;

	// -------- set initial data
	device_data->ctrl_status = (tapestatus_exists | tapestatus_data_not_ready);

	// --------MOVED COM PORT OPEN TO COM WORKER THREAD...


	// --------initialize comm worker thread.
	comm_thread_handle = device_common_start_thread((LPVOID)device_data,
		device_tape_local_IO_worker_thread,
		&comm_thread_id);

	// -------- comm thread created, fill in information.
	if (comm_thread_handle != 0) {
		printf(" tape device at device address  %02x communications thread created.\n", loc_device_addr);
		iop_device_thread_handle2[loc_device_addr] = comm_thread_handle;
		iop_device_thread_id2[loc_device_addr] = comm_thread_id;
	}
	// --------trouble creating comm worker thread.
	else {
		printf("\n *** ERROR *** Trouble creating communications worker thread for device %02x.  Device not created.\n", loc_device_addr);

		// -------- ask worker thread to stop
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


		//	device_tape_process_command( SIMJ_U16 loc_cmd, DEVICE_TAPE_DATA* device_data );


		// }

		// --------all commands processed, indicate not busy.
		// loc_status &= (~tapestatus_busy);

		// -------- if reading and data available, update status.
		// if (dev_reading) {
		//  	if (device_common_buffer_isempty(&device_data->in_buff)) {
		//		loc_status |= tapestatus_data_not_ready;
		//	}
		//	else {
		//		loc_status &= ~tapestatus_data_not_ready;
		//	}
		//}

		// --------if writing and buffer space available, update status.
		//if (dev_writing) {
		//	if (device_common_buffer_isfull(&device_data->out_buff)) {
		//		loc_status |= tapestatus_data_not_ready;
		//	}
		//	else {
		//		loc_status &= ~tapestatus_data_not_ready;
		//	}
		//}

		// --------update device status
		//device_data->ctrl_status = loc_status;
		// if ( our_status != orig_status )
		//	printf("\n Device tape status updated 0x%04x\n", our_status);

		// --------wait for timeout or a new request.
		WaitOnAddress(&(device_data->ctrl_wake), &last_wake, sizeof(last_wake), (DWORD)100);
		//  Sleep(100);
	}

	// ------- it is asked to exit on its own... No need to do this here.
	// --------request comm thread to stop
	// iop_thread_stop_request2[loc_device_addr] = 1;

	// --------wait a little for comm thread to exit.
	Sleep(200);

	// --------MOVED COM PORT CLOSE TO COM WORKER THREAD...

	// --------initialize the resource for updating status.
	// Initialize the resource one time only.
	DELETE_RESOURCE(device_data->ResourceStatusUpdate);

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
void device_tape_mount_unit(SIMJ_U16 device_address, SIMJ_U16 unit, bool read_only, char* filename) {

	// --------get the pointer to the device data
	DEVICE_TAPE_DATA* device_data = NULL;
	SIMJ_TAPE_ERR last_error;
	FILE* loc_tape_file_handle = NULL;
	int file_open_status = 0;
	SIMJ_U64 loc_tape_dpi = 0;

	// --------only do things if a valid device address...
	if (device_address >= 0 && device_address <= 0x3f) {
		DEVICE_TAPE_DATA* device_data = (DEVICE_TAPE_DATA*)iop_device_buffer[device_address];

		// --------yes this is a valid device....
		if (device_data != NULL) {
			// --------check for valid unit number
			if (unit >= 0 && unit <= 3) {

				// -------- open input tape image file.
				file_open_status = device_common_tape_open( filename, read_only, 
							&loc_tape_file_handle, &loc_tape_dpi, &last_error);

				// -------- if tape file was opened..
				if (file_open_status == 0) {

					printf(" *** INFO *** device_common_tape_open - File opened:  %s...\n", filename);

					// --------set global com handle for other thread.
					device_data->tape_file_handle[unit] = loc_tape_file_handle;
					strcpy_s(device_data->tape_filename[unit], 255, filename);
					device_data->tape_readonly[unit] = read_only;
					device_data->bot[unit] = true;
					device_data->eof[unit] = false;	// end of file.
					device_data->eot[unit] = false;	// end of tape
					device_data->last_recsize[unit] = 0;
					device_data->online[unit] = true;
					device_data->tape_mounted[unit] = true;

					TAKE_RESOURCE(device_data->ResourceStatusUpdate);
					// --------clear buffers
					device_common_buffer_set_empty(&device_data->in_buff);
					device_common_buffer_set_empty(&device_data->out_buff);
					// -------- Release ownership of the resource.
					GIVE_RESOURCE(device_data->ResourceStatusUpdate);

					printf(" Device 0x%04x, Unit %d, file: %s mounted.\n", device_address, unit, filename);

				}
				// -------- error opening com port, stop thread...
				else {
					printf(" *** ERROR ***  Trouble opening tape image file: %s  Unit not mounted.\n", filename);
					// -------- set unit not mounted...
				}
			}
			// --------unit out of range.
			else {
				printf(" *** ERROR ***  Not a valid unit number (0-3) for mount, %d\n", unit);
			}
		}
		// --------not a valid device -- device is null
		else {
			printf(" *** ERROR ***  No device at this device address 0x%04X\n", device_address);
		}
	}
	// --------bad device address
	else {
		printf(" *** ERROR ***  Not a valid device address 0x%04x\n", device_address);
	}

}


// ============================================================================================================================
// --------dismount a unit.  This can only be done after the device is initialized...
void device_tape_dismount_unit(SIMJ_U16 device_address, SIMJ_U16 unit) {

	// --------get the pointer to the device data
	DEVICE_TAPE_DATA* device_data = NULL;

	// --------only do things if a valid device address...
	if (device_address >= 0 && device_address <= 0x3f) {
		DEVICE_TAPE_DATA* device_data = (DEVICE_TAPE_DATA*)iop_device_buffer[device_address];

		// --------yes this is a valid device....
		if (device_data != NULL) {

			// --------check for valid unit number
			if (unit >= 0 && unit <= 3) {

				// --------is there something mounted.
				// --------close the file
				// --------ignore the error
				if (device_data->tape_file_handle[unit] != NULL)
					device_common_tape_close( &device_data->tape_file_handle[unit], &device_data->dpi[unit]);

				device_data->tape_mounted[unit] = false;
				device_data->online[unit] = false;
				device_data->bot[unit] = true;
				device_data->eof[unit] = false;
				device_data->eot[unit] = false;
				device_data->last_recsize[unit] = 0;
				device_data->tape_readonly[unit] = true;
				device_data->tape_file_handle[unit] = NULL;

				TAKE_RESOURCE(device_data->ResourceStatusUpdate);
				// --------clear buffers
				device_common_buffer_set_empty(&device_data->in_buff);
				device_common_buffer_set_empty(&device_data->out_buff);
				// -------- Release ownership of the resource.
				GIVE_RESOURCE(device_data->ResourceStatusUpdate);

				printf(" Device 0x%04x, Unit %d, file: %s dismounted.\n", device_address, unit, device_data->tape_filename[unit]);
				strcpy_s(device_data->tape_filename[unit], 255, "");

			}
			// --------unit out of range.
			else {
				printf(" *** ERROR ***  Not a valid unit number (0-3) for mount, %d\n", unit);
			}
		}
		// --------not a valid device -- device is null
		else {
			printf(" *** ERROR ***  No device at this device address 0x%04X\n", device_address);
		}
	}
	// --------bad device address
	else {
		printf(" *** ERROR ***  Not a valid device address 0x%04X\n", device_address);
	}

}




// ============================================================================================================================
// --------initialize the device.  calls common routines.  Only custom thing is to initialize the 
// --------data buffer after it is created.
void device_tape_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp) {

	DEVICE_TAPE_DATA* device_data = 0;
	SIMJ_U16 loc_dev_addr;
	loc_dev_addr = device_address;
	bool status = false;
	int j = 0;

	// --------make certain we aren't double allocating a device and allocate buffer memory for device.
	device_data = device_common_device_buffer_allocate(device_address, sizeof(DEVICE_TAPE_DATA));

	if (device_data != NULL) {

		// -------- fill in global buffer.
		// -------- standard information for ALL controllers
		device_data->device_address = loc_dev_addr;
		// TODO: for now make tape write locked.
		device_data->ctrl_status = tapestatus_exists | tapestatus_bot | tapestatus_wrilock | tapestatus_data_not_ready;
		que_uword_init(&(device_data->ctrl_command_que));
		device_data->ctrl_wake = 0;
		device_data->bus = bus;
		device_data->pri = prio;
		device_data->dmp = ( dmp | (( bus << 4 ) & 0x00f0));	// ensure full virt dmp
		strcpy_s(device_data->info, 40, "tape");

		// --------data specific to this device.
		device_common_buffer_init(&device_data->in_buff);
		device_common_buffer_init(&device_data->out_buff);
		device_common_buffer_set_empty(&device_data->in_buff);
		device_common_buffer_set_empty(&device_data->out_buff);

		// --------initialize the resource for updating status.
		// Initialize the resource one time only.
		status = INIT_RESOURCE(device_data->ResourceStatusUpdate);
		if (!status) {
			printf(" *** ERROR *** tape device could not create status update locking mechanism.\n");
		}

		device_data->cur_sel_unit = 0;		// which unit is selected for IO
		for (j = 0; j < 4; j++) {
			device_data->io_cmd[j] = 0;		// IO commands.  1=read, 2=write, 3=rew on, 4=rew off, 5=bk file, 6=fwd file
											//				 7=bk rec, 8=fwd rec	
			device_data->tape_mounted[j] = false;
			device_data->online[j] = false;
			device_data->bot[j] = true;
			device_data->eof[j] = false;
			device_data->eot[j] = false;
			device_data->dpi[j] = 0;
			device_data->last_recsize[j] = 0;
			device_data->tape_readonly[j] = true;
			device_data->tape_file_handle[j] = NULL;
			strcpy_s(device_data->tape_filename[j], 255, "");
		}


		// --------initialize main worker thread.
		device_common_thread_init((LPVOID)device_data,
			device_tape_worker_thread,
			device_tape_output_data,
			device_tape_output_cmd,
			device_tape_input_data,
			device_tape_input_status,
			device_tape_mount_unit,
			device_tape_dismount_unit );
	}
}



// ============================================================================================================================
void device_tape_process_command(SIMJ_U16 loc_cmd, DEVICE_TAPE_DATA* device_data) {

	SIMJ_U16 cmd_type = 0;
	SIMJ_U16 orig_status = 0;		// local copy of starting controller status.
	SIMJ_U16 loc_status = 0;		// local copy of controller status
	bool old_write = false;
	bool msg_term_icb = false;
	bool msg_unexpected_cmd = false;
	int chg_di_ena = 0;		// change DI enabled status -1=disable, 1=enable, 0=nothing
	int chg_si_ena = 0;		// change SI enabled status -1=disable, 1=enable, 0=nothing
	bool need_SI = false;	// generate SI if true
	bool need_DI = false;	// generate DI if true
	int chg_wrt = 0;
	int chg_rd = 0;
	int chg_unit = 0;
	int loc_io_cmd = 0;
	int loc_unit = 0;
	int sub_cmd = 0;
	bool rew_with_lockout = false;

	// --------get the type of command.
	cmd_type = loc_cmd & cmd_cmd_mask;

	// -------- Request ownership of the resource.
	TAKE_RESOURCE(device_data->ResourceStatusUpdate);

	// --------get internal status to work on...
	loc_status = device_data->ctrl_status;
	orig_status = loc_status;

	// --------process the various command types.
	switch (cmd_type) {

		// --------terminate
		// The generation of a terminate during controller busy causes
		// the immediate terminateion of the data transfer.  The controller stays
		// Busy, until the Read Logic detects the end of the current record.
		// Data check errors detected are indicated in thestatus word after
		// the SI generation.  The The data check error is on the complete
		// recordtransferred to the controller although only a portio of
		// the record has been transferred from the controller to the CPU.
		// 
		// A Terminate issued during controller Not Busy causes the 
		// immedate generation of an SI.  Bit 7 of the Terminate command,
		// when abled, means a Memory Parity Error (MPE) has occured
		// during a DMP transfer.  The ISA instruction can check this
		// condition becuase bit 4 of the status word is set by MPE in the
		// Terminate.
		//
		// The MPE Terminate is normally hardware-generated.
		//
		//	if EOB, bit 4, a DI is generated.
		//
		//  if Bit 9, master clear, A master clear operation is issued and
		//  the controller is in an initial reset condition and must be
		//  accompanied by bits 1 and 5.
		// 
		//  SI and DI bits in command should be off (diable interrupts?)
		//
	case cmd_cmd_term:			// do a terminate.
	case cmd_cmd_term_ign:		// do a terminate. with ign set ???

		// TODO: figure out what to do for IGN bit.

#if DEBUG_TAPE >= 1
		fprintf(stderr, " Device tape - TERM Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

		// --------stop all I/O
		chg_rd = -1;
		chg_wrt = -1;

		// --------update status
		loc_status |= (tapestatus_data_not_ready);		// set no data ready
		loc_status &= (~tapestatus_busy);				// clear busy

		// --------see if MPE bit is set.
		// TODO: do something for term with MPE bit set..

		// --------TERMINATE w/ICB
		// TODO: is this a thing... Not defined by chart in cpu manual 
		if ((loc_cmd & cmd_term_mc) != 0) {
			msg_term_icb = true;

			// --------disable interrupts --- ARE WE CERTAIN??
			// TODO: Should interrupts be disabled on terminate w/ICB?
			//if (device_data->SI_enabled)
			//	chg_si_ena = -1;
			//if (device_data->DI_enabled)
			//	chg_di_ena = -1;

			// --------clear buffers
			//device_common_buffer_set_empty(&device_data->in_buff);  // for now don't clear the input buffer.
			//device_common_buffer_set_empty(&device_data->out_buff);
		}

		// --------TERMINATE no ICB
		else {
			// fprintf(stderr, " Device tape - terminate requested. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);

			// --------clear buffers
			// TODO: tape investigate clearing output buffer on terminate 
			//device_common_buffer_set_empty(&device_data->in_buff);
			//device_common_buffer_set_empty(&device_data->out_buff);
		}

		// --------generate SI if enabled.
		if (device_data->SI_enabled || (chg_si_ena == 1)) {
			need_SI = true;
		}
		break;

		// --------transfer initiate
		//  The Transfer Initiate Command generates a Read or Write operation,
		//  determines mode of transfer and selects the magnetic tape transport 
		//  to perform the operation.
		//
		//  The Transfer Initiate Command specifies whether a Single Cycle Scan
		//  is generated prior to the Read or Write.
		// 
		//  Options:
		//       Bit 1 - DMP mode = 1
		//		 Bit 2 - SI enabled = 1
		//		 Bit 3 - Di enabled = 1
		//		 Bit 4 - Read = 1, else write
		//       Bit 6 - Gap length long=1, else normal
		//		 Bit 7 - Enable SCS=1 (single scan cycle)
		//
	case cmd_cmd_ti_dmp:		//	0xC000		// do a transfer initiate command w/dmp
		if ((loc_status & tapestatus_busy) == 0) {
			// --------store tc/ta information
			iop_get_dmp_parameters(device_data->device_address, device_data->dmp, &(device_data->dmp_tc), &(device_data->dmp_ta), &(device_data->dmp_virt), &(device_data->dmp_abs_tc_addr));
		}
		// --------fall through to reset of transfer initiate.

	case cmd_cmd_ti:			//	0x8000		// do a transfer initiate command

		// --------only do if controller is not busy!!!
		if ((loc_status & tapestatus_busy) == 0) {
			// -------- select unit
			loc_unit = loc_cmd & cmd_unit_mask;
			loc_status = (loc_status & ~tapestatus_unitsel_mask) | loc_unit;		// set no data ready
			chg_unit = 1;

			// -------- enable or disable interrupts.
			device_tape_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);

			// --------start a write.
			if (!(loc_cmd & cmd_read)) {
#if DEBUG_TAPE >= 1
				fprintf(stderr, " Device tape - transfer initiate - write requested.  Dev addr: %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

				// --------get old write status
				old_write = device_data->write_in_progress;

				// -------- indicate transfer in progress
				chg_rd = -1;
				chg_wrt = 1;
				// loc_status |= (tapestatus_busy|tapestatus_data_not_ready);
				loc_status |= (tapestatus_data_not_ready);

				// -------- if ready for a byte send DI.   If we weren't already doing a write, send DI to get data.
				// TODO: Fix DI logic
				// if ((device_data->DI_enabled || (chg_di == 1)) && !old_write) {
				// 	need_DI = true;
				// }
				// --------indicate write command
				if (cmd_type == cmd_cmd_ti_dmp) {
					loc_io_cmd = locIOcmd_writeDMP;
				}
				else {
					loc_io_cmd = locIOcmd_write;
				}
			}
			// --------start a read.
			else {
#if DEBUG_TAPE >= 1
				fprintf(stderr, " Device tape - transfer initiate - read requested.  Dev addr: %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

				// -------- get old read status
				// old_read = device_data->read_in_progress;

				// -------- indicate transfer in progress
				chg_rd = 1;
				chg_wrt = -1;
				// loc_status |= (tapestatus_data_not_ready);
				loc_status |= (tapestatus_busy | tapestatus_data_not_ready);

				// --------indicate read command
				if (cmd_type == cmd_cmd_ti_dmp) {
					loc_io_cmd = locIOcmd_readDMP;
				}
				else {
					loc_io_cmd = locIOcmd_read;
				}
			}
			// --------what interrupts are caused by transfer initiate???   (Always SI to signal completion??,   DI when starting write?)

			// --------generate SI if enabled.
			//if (device_data->SI_enabled) {
			//	need_SI = true;
			//}
		}
		break;


		// --------Write End Of File
		//
		// The WEOF Command causes write and erase head currents to be turned on and
		// the currents forward tape motion to occur (if a Single Cycle Scan was
		// programmed, the scan executes before the Write).  The selected transport must
		// be operable, not rewinding, and hae a write ring in place in order for the
		// command to be functional.  If these Write operation prerequisites are not met,
		// the operation is inhibitted and an SI is generated.
		//
		// Approximately three inches of tape are erased and then an End-Of-File (EOF)
		// is written.
		//
		// Options:
		//		bit 2 - DI enable
		//		bit 3 - SI enable
		//		bit 7 - Enable Single Scan Cycle (SCS)
		//		bit 12-15 - unit
		//
	case cmd_cmd_weof:		//			0x4020		// do a weof command (gap = 1)

		// --------only do if controller is not busy!!!
		if ((loc_status & tapestatus_busy) == 0) {

#if DEBUG_TAPE >= 1
			fprintf(stderr, " Device tape - WEOF Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

			// -------- enable or disable interrupts.
			device_tape_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);

			// -------- check prerequisites
			// TODO: check prerequisites

			// -------- select unit
			loc_unit = loc_cmd & cmd_unit_mask;
			loc_status = (loc_status & ~tapestatus_unitsel_mask) | loc_unit;		// set no data ready
			chg_unit = 1;

			loc_io_cmd = locIOcmd_weof;

			// TODO: if prerequisites not met, generate SI (if enabled?)
		}
		break;

		// --------SPACE commands (reverse record, forward record, reverse file, forward file)
		//  The Space Command allows the programmer to move tape without a data transfer.
		//  The command is used to move (space) tape forwards or backwards one file (EOF) or
		//  record at a time.  The Unit and SCS bits must be selected in eah Space Record
		//  Command.  
		//  command options are:
		//		Bit 2 - DI enable
		//		Bit 3 - SI enable
		//		Bit 7 - Enable Single Scan Cycle
		//	
	case cmd_cmd_space:		//
		sub_cmd = loc_cmd & cmd_space_mask;
		switch (sub_cmd) {

			// --------SPACE record forward command
			// This command causes forward tape motion and a Write Current Turn-Off.  When
			// The selected transport is operable and rewiding, an SI is generated and 
			// the operation is inhibited.  Forward motion continus until an Inter-Record Gap (IRG)
			// is encountered.  The controller goes Not Busy, the SI is generated and the
			// dynamic gap window time-out commences, allowing new commands to be accepted.
			//
			case cmd_space_fwdrec:
				// --------only do if controller is not busy!!!
				if ((loc_status & tapestatus_busy) == 0) {

#if DEBUG_TAPE >= 1
					fprintf(stderr, " Device tape - FWD REC Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

					// -------- enable or disable interrupts.
					device_tape_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);
	
					// -------- check prerequisites
					// TODO: check prerequisites

					// -------- select unit
					loc_unit = loc_cmd & cmd_unit_mask;
					loc_status = (loc_status & ~tapestatus_unitsel_mask) | loc_unit;		// set no data ready
					chg_unit = 1;

					loc_io_cmd = locIOcmd_fwdrec;

					// TODO: if prerequisites not met, generate SI (if enabled?)
				}	
				break;

			// --------SPACE record backward command
			//  This command operates similarly to and requires the same considerations
			//  as Space Record Forward with the following exceptions:
			// 
			//	-- Motion occurs in a reverse direction
			// 
			//	-- The controller goes NOT BUSY and stops in the next IRG after the read
			//	   logic senses the first character of the respective block.
			// 
			//  -- When no IRG is detected, the transport moves the tape backwards until
			//     the Beginning of Tape (BOT) tab is reached and then stops at the load
			//     point.  BOT and EOF status bits are set.
			// 
			//	-- If the tape is positioned at the BOT tab when the command is given,
			//	   the operation is inhibited and an SI is generated.  BOT and EOF status
			//	   bits remain set.
			//
			case cmd_space_revrec:		//					// do a space reverse to eor

				// --------only do if controller is not busy!!!
				if ((loc_status & tapestatus_busy) == 0) {

#if DEBUG_TAPE >= 1
					fprintf(stderr, " Device tape - REV REC Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

					// -------- enable or disable interrupts.
					device_tape_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);		

					// -------- check prerequisites
					// TODO: check prerequisites
	
					// -------- select unit
					loc_unit = loc_cmd & cmd_unit_mask;
					loc_status = (loc_status & ~tapestatus_unitsel_mask) | loc_unit;		// set no data ready
					chg_unit = 1;

					loc_io_cmd = locIOcmd_bkrec;

					// TODO: if prerequisites not met, generate SI (if enabled?)
				}
				break;

			// --------SPACE to EOF Forward Command
			//  This command operates similarly to and requires the same considerations as
			//  Space Record Forward with the following exceptions:
			// 
			//	-- Blocks are spaced over without stopping until the Read Logic detects the
			//	   EOF tape-mark, and then stops.  The EOF status is set.
			//
			case cmd_space_revfil:		//					// do a space reverse to eof


				// --------only do if controller is not busy!!!
				if ((loc_status & tapestatus_busy) == 0) {

#if DEBUG_TAPE >= 1
					fprintf(stderr, " Device tape - REV FIL Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

					// -------- enable or disable interrupts.
					device_tape_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);

					// -------- check prerequisites
					// TODO: check prerequisites

					// -------- select unit
					loc_unit = loc_cmd & cmd_unit_mask;
					loc_status = (loc_status & ~tapestatus_unitsel_mask) | loc_unit;		// set no data ready
					chg_unit = 1;

					loc_io_cmd = locIOcmd_bkfile;

					// TODO: if prerequisites not met, generate SI (if enabled?)
				}
				break;

			// --------SPACE to EOF Backward Command
			//  This command operates similarly to and requires the same considerations as
			//  Space Record Backward with the following exception:
			//
			//	-- Blocks are spaced over without stopping until the Read Logic detects the
			//	   EOF tape-mark and then stops.  The EOF status is set.
			//
			case cmd_space_fwdfil:		//					// do a space fwd to eof

				// --------only do if controller is not busy!!!
				if ((loc_status & tapestatus_busy) == 0) {

#if DEBUG_TAPE >= 1
					fprintf(stderr, " Device tape - FWD FIL Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

					// -------- enable or disable interrupts.
					device_tape_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);

					// -------- check prerequisites
					// TODO: check prerequisites

					// -------- select unit
					loc_unit = loc_cmd & cmd_unit_mask;
					loc_status = (loc_status & ~tapestatus_unitsel_mask) | loc_unit;		// set no data ready
					chg_unit = 1;

					loc_io_cmd = locIOcmd_fwdfile;

					// TODO: if prerequisites not met, generate SI (if enabled?)
				}
				break;

			default:
				msg_unexpected_cmd = true;
				break;
		}
		break;

	// -------- NOOP Command
	//	The controller accepts the NO-OP command regardless of controller
	//	busy status.  The data or service interrupts can be enabled or disabled
	//	as specified by bits 2 and 3.  When the controller is not busy the following
	//	status bits are reset 1, 2, 4, 7, 9 13
	// **DONE**
	case cmd_cmd_noop:  //			0x4000		// do a no-op command

#if DEBUG_TAPE >= 1
		fprintf(stderr, " Device tape - NOOP Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

		// -------- enable or disable interrupts.
		device_tape_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);

		// -------- if not busy reset all status indications ??
		if ((loc_status & tapestatus_busy) == 0) {
			loc_status &= (~(tapestatus_overunder | tapestatus_devparity |
				tapestatus_memparity | tapestatus_busy |
				tapestatus_eot | tapestatus_partial));
		}

		// --------generate SI if enabled.
		// TODO: verify this.
		if (device_data->SI_enabled || (chg_si_ena == 1)) {
			need_SI = true;
		}
		break;

	// -------- REWIND command
	//  The Rewind Command causes the selected transport to go into the high-speed
	//	rewind mode.  After teh rewind operation is initiated an SI is generated.  If
	//	the transport is inoperable or already rewinding, an IS is also generated.
	//
	//  Status bit 12, Device Rewinding is set TRUE by the selected transport rewinding,
	//	the transport rejects commands until BOT is reached.  While a transport is
	//	rewinding, other transports can be selected and operation can be initiated.
	//  Options:
	//		Bit 2 - DI enable
	//		Bit 3 - SI enable

	// --------REWIND with Lockout (offline)
	//  This command causes the transport to go off-line when the rewind is initiated,
	//	generates an zSI and rewinds the tape to BOT.  Device status bit 3,
	//	inoperable is set TRUE after the SI for this command is generated.  To
	//	return the transport to a ready status requires operator intervention.
	//

	// --------REWIND command.
	//  
	case cmd_cmd_rew:		//				0x4080		// do a rewind and stay online

		rew_with_lockout = loc_cmd & cmd_rewind_lockout;

#if DEBUG_TAPE >= 1
		if (rew_with_lockout) {
			fprintf(stderr, " Device tape - REW OFL Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
		}
		else {
			fprintf(stderr, " Device tape - REW Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
		}
#endif

		// --------only do if controller is not busy!!!
		if ((loc_status & tapestatus_busy) == 0) {


			// -------- enable or disable interrupts.
			device_tape_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);

			// -------- check prerequisites
			// TODO: check prerequisites

			// -------- select unit
			loc_unit = loc_cmd & cmd_unit_mask;
			loc_status = (loc_status & ~tapestatus_unitsel_mask) | loc_unit;		// set no data ready
			chg_unit = 1;

			// --------this should already be the case, but stop other io
			chg_rd = -1;
			chg_wrt = -1;

			// --------set busy
			loc_status |= ( tapestatus_busy | tapestatus_data_not_ready);

			if (rew_with_lockout) {
				loc_io_cmd = locIOcmd_rewoff;
			}
			else {
				loc_io_cmd = locIOcmd_rewon;
			}

			// TODO: if prerequisites not met, generate SI (if enabled?)
		}
		// ------- controller is busy, rewind not done..
		else {
#if DEBUG_TAPE >= 1
			fprintf(stderr, " Device tape - REWIND not done.  controller busy \n");
#endif
		}
		break;

	// --------Transport Select Command
	//  The Transport Select Command causes the controller to store the specified unit
	//	number (bits 14 and 15) in the unit register, which in turn selects the 
	//	transport.  The transport unit bits are coded in binary as transports 0 through 3.  
	//  The command does not cause the controller to go busy or cause an SI.  The
	//	Transport Select Command resets the following status bits:
	//		1, 2, 4, 7, 9, 13
	//	To determine the number of the transport presently being selected, initate n ISA
	//	instruction at any time.
	//	Options:
	//		Bit 2 - DI enable
	//		Bit 3 - SI enable
	//		Bit 7 - Continuous Scan
	//
	case cmd_cmd_sel_trans:		//		0x4200		// do a select unit command.

#if DEBUG_TAPE >= 1
		fprintf(stderr, " Device tape - SEL TRANS Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif
		// -------- enable or disable interrupts.
		device_tape_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);

		// -------- select unit
		loc_unit = loc_cmd & cmd_unit_mask;
		loc_status = (loc_status & ~tapestatus_unitsel_mask) | loc_unit;		// set no data ready
		chg_unit = 1;

		// -------- reset status bits
		loc_status &= (~(tapestatus_overunder | tapestatus_devparity |
			tapestatus_memparity | tapestatus_busy |
			tapestatus_eot | tapestatus_partial));

		break;


		// --------unexpected command...
	default:
		msg_unexpected_cmd = true;
		break;

	}		// -------- END OF COMMAND PROCESSING


	// --------SEE if SI enable changed... 
	if (chg_si_ena == 1) {
		device_data->SI_enabled = true;
		chg_si_ena = 0;
	}
	else if (chg_si_ena == -1) {
		device_data->SI_enabled = false;
		chg_si_ena = 0;
		need_SI = false;
		cpu_reset_SI(device_data->bus, device_data->pri, device_data->device_address);
	}

	// --------See if DI enable changed
	if (chg_di_ena == 1) {
		device_data->DI_enabled = true;
		chg_di_ena = 0;
	}
	else if (chg_di_ena == -1) {
		device_data->DI_enabled = false;
		chg_si_ena = 0;
		need_DI = false;
		cpu_reset_DI(device_data->bus, device_data->pri, device_data->device_address);
	}

	if (chg_rd == -1) {
		device_data->read_in_progress = false;
	}
	else if (chg_rd == 1) {
		device_data->read_in_progress = true;
	}
	if (chg_wrt == -1) {
		device_data->write_in_progress = false;
	}
	else if (chg_wrt == 1) {
		device_data->write_in_progress = true;
	}
	if (chg_unit == 1) {
		device_data->cur_sel_unit = loc_unit;
	}

	// -------- if reading and data available, update status.
	if (device_data->read_in_progress) {
		if (device_common_buffer_isempty(&device_data->in_buff)) {
			loc_status |= tapestatus_data_not_ready; // no data to read.
		}
		else {
			loc_status &= (~tapestatus_data_not_ready); // something to read.
		}
	}

	// --------if writing and buffer space available, update status.
	if (device_data->write_in_progress) {
		if (device_common_buffer_isfull(&device_data->out_buff)) {
			loc_status |= tapestatus_data_not_ready; // cant write
		}
		else {
			loc_status &= (~tapestatus_data_not_ready); // can write
		}
	}

	// --------update device status
	device_data->ctrl_status = loc_status;

	// -------- Release ownership of the resource.
	GIVE_RESOURCE(device_data->ResourceStatusUpdate);

	// -------- generate interrupts if needed.
	if (need_SI) {
		cpu_request_SI(device_data->bus, device_data->pri, device_data->device_address);
	}
	if (need_DI) {
		cpu_request_DI(device_data->bus, device_data->pri, device_data->device_address);
	}

	// -------- set new local IO worker command.
	if (loc_io_cmd != 0) {
		boolean not_okay_to_send = true;
		int max_wait_count = 0;
		while (not_okay_to_send) {
			if ((device_data->io_cmd[device_data->cur_sel_unit] != 0) && max_wait_count < 8) {
				//printf(" *** ISSUE *** Device tape - io command over written 0x%04x \n", device_data->io_cmd[device_data->cur_sel_unit]);
				Sleep(1);
				max_wait_count++;
			}
			else {
				if ( max_wait_count >= 8 )
					printf(" *** ISSUE *** Device tape - io command over written 0x%04x \n", device_data->io_cmd[device_data->cur_sel_unit]);
				device_data->io_cmd[device_data->cur_sel_unit] = loc_io_cmd;
				device_data->ctrl_wake++;
				WakeByAddressSingle((PVOID) & (device_data->ctrl_wake));

				// --------allow other threads to run
				SwitchToThread();
				not_okay_to_send = false;
				max_wait_count = 0;
			}
		}
	}

	// --------diag/debug messages....
#if DEBUG_TAPE >= 1
	if (msg_term_icb)
		fprintf(stderr, " Device tape - terminate w/ICB requested. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

	if (msg_unexpected_cmd)
		fprintf(stderr, " Device tape - unexpected command.  Dev addr: %d,  cmd 0x%04x\n", device_data->device_address, loc_cmd);
}
