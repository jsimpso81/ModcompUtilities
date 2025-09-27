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

// -------- DISC DEVICE -- Via file.

#include "simj_base.h"

#include <process.h>
#include <stdio.h>
#include <stdbool.h>


// -------- DEVICE DISC


// -------- 
// -------- status
// -------- 0 1 2 3 4 5 6 7 8 9 a b c d e f
// -------- 0					error
// -------- 1					no error
// -------- x 1					under / over flow err
// -------- x x 1					crc error
// -------- x x x 1					inop
// -------- x x x x 1				mpe
// -------- x x x x x 1				write lock ? WLD
// -------- x x x x x x 1				seek(ing ? )
// -------- x x x x x x x 1				ctrl busy
// -------- x x x x x x x x 0			data ready
// -------- x x x x x x x x x EOD			EOD(assume 1)
// -------- x x x x x x x x x x EOF			EOF(assume 1)
// -------- x x x x x x x x x x x EOR		EOR(assume 1)
// -------- x x x x x x x x x x x x 1		Dev Seek
// -------- x x x x x x x x x x x x x 1		Seek Comp
// -------- x x x x x x x x x x x x x x A A		Unit 0 - 3
// -------- 
// -------- if Bit 0 = 1, then bits 2, 4, 5, 6 have the following meaning :
// -------- 2 = 200 TPI
// -------- 4 = 5727 ctrl
// -------- 5 = write protected
// -------- 6 = dual platter

// --------status
#define discstatus_noerror			0x8000
#define discstatus_underoverfl		0x4000	//under / over flow err
#define discstatus_crcerror			0x2000  // crc error
#define discstatus_inop				0x1000	// inop
#define discstatus_mpe				0x0800	// mem parity error
#define discstatus_wld				0x0400	// write lock ??
#define discstatus_seek				0x0200	// seek(ing ? )
#define discstatus_ctrlbusy			0x0100	// ctrl busy
#define discstatus_datanotready		0x0080	// data ready
#define discstatus_eod				0x0040	// EOD(assume 1)  (only when bit 0 = 0 )
#define discstatus_dualplatter		0x0040	// dual platter  - only when bit 0 = 1 )
#define discstatus_eof				0x0020	// EOF(assume 1)  (only when bit 0 = 0 )
#define discstatus_writeprot		0x0020	// write protect - only when bit 0 = 1
#define discstatus_eor				0x0010	// EOR(assume 1)
#define discstatus_devseek			0x0008	// Dev Seek
#define discstatus_seekcomp			0x0004	// Seek Comp
#define discstatus_unitmask			0x0003	// Unit 0 - 3
#define discstatus_typemask			0x0014	// type of disc mask
#define discstatus_type_200tpi		0x0004	// 200 tpi disc ( when bit 0 = 1 )
#define discstatus_type_5727		0x0010	// 5727 controller ? ( when bit 0 = 1 )


// -------- Moving Head Disk(200TPI and others)

// -------- Uses MH - HAN(Sysgened as CX)

// -------- Disk commands

// -------- 0 1 2 3 4 5 6 7 8 9 a b c d e f

// -------- 1 0 x x 0 x x x x x x x x x x x   transfer initiate write programmed IO
// -------- 1 0 x x 1 x x x x x x x x x x x   transfer initiate read programmed IO

// -------- 1 1 x x 0 x x x x x x x x x x x   transfer initiate write dmp
// -------- 1 1 x x 1 x x x x x x x x x x x   transfer initiate read dmp

// -------- 0 1 x x 1 0 x x x x x x x x x x   eob
// -------- 0 1 x x 0 1 x x x x x x x x x x   term
// -------- 0 1 x x 1 1 x x x x x x x x x x   eob and term
// -------- 0 1 x x 0 0 1 x x x x x x x x x   cyl select
// -------- 0 1 x x 0 0 0 x x x x x x x x x   no - op

// -------- 0 0 x x x x x x x x x x x x x x   head.drive select
// 
// --------command
#define cmd_cmd_mask			0xC000		// command mask
// --------various commands.
#define cmd_cmd_ti				0x8000		// do a transfer initiate command
#define cmd_cmd_ti_dmp			0xC000		// do a transfer initiate command w/dmp
#define cmd_cmd_ctrl			0x4000		// do a control command (parse sub commands.)
#define cmd_cmd_select			0x0000		// do a head/drive select 

// --------sub commands for ctrl.
#define cmd_ctrl_mask			0x0C00		// ctrl sub command mask
#define cmd_ctrl_eobterm		0x0C00		// eob and term
#define cmd_ctrl_eob			0x0800		// eob
#define cmd_ctrl_term			0x0400		// eob
#define cmd_ctrl_noop			0x0000		// no-op or cyl select

// --------other masks in command register.
#define cmd_di_enable			0x2000
#define cmd_si_enable			0x1000
#define cmd_term_eob			0x0800		// terminate end of block modifier...
//#define cmd_read				0x0800		// read if one, else write.
//#define cmd_gap_long			0x0200		// long gap if one, else normal
#define cmd_term_par			0x0100		// terminate mem parity error occured.
//#define cmd_scs				0x0100		// enable single scan scs if one, else disable
//#define cmd_term_mc			0x0040		// terminate master clear.
#define cmd_unit_mask			0x0003		// unit mask.

// --------various things with in transfer initiate command
#define cmd_ti_read				0x0800		// read = true, write=false
#define cmd_ti_eod				0x0400		// eod - write only
#define cmd_ti_eof				0x0200		// eof - write only - write eof ??
#define cmd_ti_ieor				0x0100		// ieor - write only
#define cmd_ti_scs				0x0080		// scs 
#define cmd_ti_sectormask		0x001F		// sector mask in TI cmd

// --------various things for select command
#define cmd_sel_ign				0x2000	// ??
#define cmd_sel_plat			0x0200
#define cmd_sel_head			0x0100
#define cmd_sel_contscan		0x0080	// ??
#define cmd_sel_prepmode		0x0004	// not supported...
#define cmd_sel_unitmask		0x0003

// --------various bits for noop/cyl select
#define cmd_noop_cylsel			0x0200		// cyl select subcommand
#define cmd_cylsel_cylmask		0x01ff		// cyl mask

// --------internal list of IO commands
// IO commands.  1=read, 2=write, 3=rew on, 4=rew off, 5=bk file, 6=fwd file
											//				 7=bk rec, 8=fwd rec
#define locIOcmd_read		1
#define locIOcmd_readDMP	2
#define locIOcmd_write		3
#define locIOcmd_writeDMP	4
//#define locIOcmd_rewon		5
//#define locIOcmd_rewoff		6
//#define locIOcmd_bkfile		7
//#define locIOcmd_fwdfile		8
//#define locIOcmd_bkrec		9
//#define locIOcmd_fwdrec		10
#define locIOcmd_weof		5
#define locIOcmd_weod		6
#define locIOcmd_wieor		7

void device_disc_mh_process_command(SIMJ_U16 loc_cmd, DEVICE_DISC_DATA* device_data);
void device_disc_mh_dismount_unit(SIMJ_U16 device_address, SIMJ_U16 unit);

// ============================================================================================================================
void device_disc_mh_enable_disable_SI_DI(SIMJ_U16 loc_cmd, DEVICE_DISC_DATA* device_data, int* chg_si_ena, int* chg_di_ena) {

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
static void  device_disc_mh_output_data(SIMJ_U16 device_address, SIMJ_U16 data_value) {

	// --------get the pointer to the data buffer
	DEVICE_DISC_DATA* databuffer = (DEVICE_DISC_DATA*)iop_device_buffer[device_address];

	// TODO: word output.

	// --------if writing and buffer isn't empty, set data not ready in status word...
	if (databuffer->write_in_progress && !device_common_buffer_isempty(&databuffer->out_buff)) {

		// -------- Request ownership of the resource.
		TAKE_RESOURCE(databuffer->ResourceStatusUpdate);
		// -------set data not ready.
		databuffer->ctrl_status |= discstatus_datanotready;
		// -------- Release ownership of the resource.
		GIVE_RESOURCE(databuffer->ResourceStatusUpdate);
	}

	// --------wake up IO worker thread.
	databuffer->ctrl_wake++;
	WakeByAddressSingle((PVOID) & (databuffer->ctrl_wake));

	// --------allow other threads to run -- WHY IS THIS COMMENTED OUT!!!
	// SwitchToThread();

}

// ============================================================================================================================
static void  device_disc_mh_output_cmd(SIMJ_U16 device_address, SIMJ_U16 cmd_value) {

	DEVICE_DISC_DATA* databuffer = (DEVICE_DISC_DATA*)iop_device_buffer[device_address];

#if DEBUG_DISC_MH >= 4
	printf("\n device_disc_mh output cmd -- called - %04x\n", cmd_value);
#endif
	// --------process the command
	device_disc_mh_process_command(cmd_value, databuffer);

}

// ============================================================================================================================
static SIMJ_U16  device_disc_mh_input_data(SIMJ_U16 device_address) {

	DEVICE_DISC_DATA* databuffer = (DEVICE_DISC_DATA*)iop_device_buffer[device_address];
	SIMJ_U16 ourvalue = 0;
	SIMJ_U16 ourvalue1 = 0;
	SIMJ_U16 ourvalue2 = 0;
	SIMJ_U8 ourbyte1 = 0;
	SIMJ_U8 ourbyte2 = 0;
	bool new_data = false;

	// --------if there is a byte, get it.
	new_data = device_common_buffer_get(&databuffer->in_buff, &ourbyte1);
	new_data = device_common_buffer_get(&databuffer->in_buff, &ourbyte2);

	ourvalue1 = ourbyte1;
	ourvalue2 = ourbyte2;
	ourvalue = (ourvalue1 << 8 | (ourvalue2 & 0x00ff));

	// --------If buffer is empty,, set data_not_ready flag in status word.
	if (databuffer->read_in_progress && device_common_buffer_isempty(&databuffer->in_buff)) {
		// -------- Request ownership of the resource.
		TAKE_RESOURCE(databuffer->ResourceStatusUpdate);
		databuffer->ctrl_status |= discstatus_datanotready;
		// -------- Release ownership of the resource.
		GIVE_RESOURCE(databuffer->ResourceStatusUpdate);
	}

#if DEBUG_DISC_MH >= 3
	fprintf(stderr, " device_disc_mh input data -- called - 0x%04x, index %d, new: %s \n", ourvalue, databuffer->in_buff.last_byte_read_index, (new_data ? "New  " : "Empty"));
#endif

	// --------wake up comm thread.  WHY 
	// TODO: figure out if this is needed.
	// databuffer->ctrl_wake++;
	// WakeByAddressSingle((PVOID) & (databuffer->ctrl_wake));

	return ourvalue;
}

// ============================================================================================================================
static SIMJ_U16  device_disc_mh_input_status(SIMJ_U16 device_address) {

	DEVICE_DISC_DATA* databuffer = (DEVICE_DISC_DATA*)iop_device_buffer[device_address];

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
	//		loc_status |= discstatus_datanotready; // no data to read.
	//	}
	//	else {
	//		loc_status &= (~discstatus_datanotready); // something to read.
	//	}
	//}

	// --------if writing and buffer space available, update status.
	//if (databuffer->write_in_progress) {
	//	if (!device_common_buffer_isempty(&databuffer->out_buff)) {
	//		loc_status |= discstatus_datanotready; // cant write
	//	}
	//	else {
	//		loc_status &= (~discstatus_datanotready); // can write
	//	}
	//}

	//if (loc_status1 != loc_status) {
	//	databuffer->ctrl_status = loc_status;
	//}
	// -------- Release ownership of the resource.
	//GIVE_RESOURCE(databuffer->ResourceStatusUpdate);

#if DEBUG_DISC_MH >= 4
	printf("\n device_disc_mh input status -- called - 0x%04x\n", loc_status);
#endif

	return loc_status;
}


// ============================================================================================================================
// --------this routine interacts with the local computer disc image files,
static DWORD WINAPI device_disc_mh_local_IO_worker_thread(LPVOID lpParam) {

	SIMJ_U16 loc_device_addr = 0;
	DEVICE_DISC_DATA* device_data = 0;

	SIMJ_U16 last_wake = 0;
	SIMJ_U64 j = 0;
	BOOL bool_status;

	DWORD last_error = 0;
	int set_param_status = 1;
	int loc_stop_request = 0;
	SIMJ_U16 loc_status = 0;

	int j_unit = 0;
	int loc_cmd = 0;


	// --------disc disk file stuff
	// TODO: move this to heap...
	union {
		unsigned __int8 ubytes[256];
		unsigned __int16 words[128];
	} disc_sector_buffer[128] = { 0 };

	// --------local things indexed by unit...
	__int64 current_file_pos[4] = { 0,0,0,0 };		// current byte pos in file...
	size_t bytes_read[4] = { 0,0,0,0 };
	size_t words_read[4] = { 0,0,0,0 };
	int stat[4] = { 0,0,0,0 };
	errno_t status[4] = { 0,0,0,0 };
	bool not_done[4] = { true, true, true, true };
	size_t start_word[4] = { 0,0,0,0 };
	__int64 word_index[4] = { 0,0,0,0 };
	int start_byte[4] = { 0,0,0,0 };
	int end_of_file[4] = { 0,0,0,0 };
	// --------end disc file stuff..

	// -------------------------------------------------------------------------------------------------------------
	// --------initializtion
	// 
	// 

	// -------- get local device address from calling parameter to this routine 
	loc_device_addr = *(SIMJ_U16*)lpParam;
	printf(" Starting device Disc MH communications thread at device address %d\n", loc_device_addr);
	if (loc_device_addr == 0) {
		printf(" *** ERROR *** Disc MH device thread didn't get device address \n");
		loc_stop_request = 1;
		iop_thread_stop_request[loc_device_addr] = 1;
	}

	// -------- get pointer to device data.
	device_data = (DEVICE_DISC_DATA*)(iop_device_buffer[loc_device_addr]);

	// -------- raise the priority of this thread.
	bool_status = SetThreadPriority((HANDLE)iop_device_thread_handle2[loc_device_addr], THREAD_PRIORITY_ABOVE_NORMAL);
	if (!bool_status) {
		printf(" *** ERROR *** Failed to raise priority of disc_mh communications thread.  Status 0x%08x\n", GetLastError());
	}

	// --------disc file stuff
	not_done[0] = true;
	current_file_pos[0] = 0;
	word_index[0] = 0;
	// ---------end disc file stuff..

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

				if (device_data->disc_file_handle[j_unit] != NULL) {

					SIMJ_U16 sectors_to_read = 1;

					if (loc_cmd == locIOcmd_readDMP) {
						SIMJ_U16 dmp_words_requested = 0;
						// --------find out how many words to read..
						iop_get_dmp_word_count(device_data->dmp_virt, device_data->dmp_tc, device_data->dmp_ta,
							device_data->dmp_abs_tc_addr, iop_vdmp_miap_page[device_data->dmp], iop_vdmp_miap_length[device_data->dmp],
							&dmp_words_requested);

						sectors_to_read = dmp_words_requested / 128;
						if ((dmp_words_requested % 128) != 0)
							sectors_to_read++;
					}
#if DEBUG_DISC_MH >= 1
					printf(" device_disc_mh - sectors to read %d\n", sectors_to_read);
#endif
					SIMJ_U64 j = 0;
					SIMJ_U16 flags = 0;

					bytes_read[j_unit] = 0;
					end_of_file[j_unit] = -1;

					for (j = 0; j < sectors_to_read; j++) {
						stat[j_unit] = device_common_disc_read_sector(device_data->disc_file_handle[j_unit],
							device_data->dpi[j_unit]+j,
							&(disc_sector_buffer[j]), &flags);

#if DEBUG_DISC_MH >= 1
						printf(" device_disc_mh - read record -- status %d flags %d sector %zd\n", 
								stat[j_unit], flags, (device_data->dpi[j_unit] + j) );
#endif
						if (flags != 0) {
							end_of_file[j_unit] = true;
							break;
						}
						// -------- KLUDGE
						if (stat[j_unit] == 0) {
							bytes_read[j_unit] += 256;
						}
					}

					// --------copy to buffer.
					if (bytes_read[j_unit] > 0) {

						device_data->eof[j_unit] = false;

						// --------if DMP process...
						if (loc_cmd == locIOcmd_readDMP) {
							// iop_finish_dmp_read(device_data->device_address, device_data->dmp, &(disc_sector_buffer.words[0]), (int)(bytes_read[j_unit] / 2));
							iop_finish_dmp_read(device_data->dmp_virt, device_data->dmp_tc, device_data->dmp_ta,
								device_data->dmp_abs_tc_addr, iop_vdmp_miap_page[device_data->dmp], iop_vdmp_miap_length[device_data->dmp],
								&(disc_sector_buffer[0].words[0]), (int)(bytes_read[j_unit] / 2));

						}
						// --------REG IO, copy to buffer.
						else {
							device_common_buffer_set_empty(&device_data->in_buff); // set buffer empty
							for (j = 0; j < bytes_read[j_unit]; j++) {
								device_common_buffer_put(&device_data->in_buff, disc_sector_buffer[0].ubytes[j]);
							}
						}
						device_data->eof[j_unit] = false;
					}
					// --------got either end of record or end of file  -- same thing for tape
					else {
						device_data->eof[j_unit] = true;
					}
				}

				// --------try to read with no image file open
				else {
					printf(" *** ERROR *** disk try to read without image file.\n");
				}


				// TODO: fix end of disc and end of file bits!!
				// --------set data ready in status word.
				// -------- Request ownership of the resource.
				TAKE_RESOURCE(device_data->ResourceStatusUpdate);
				// --------signal buffer not full -- ready for more.
				loc_status = device_data->ctrl_status;
				loc_status &= (~(discstatus_datanotready | discstatus_ctrlbusy | discstatus_eof));
				if (device_data->eof[j_unit]) {
					loc_status |= discstatus_eof;
				}
				else {
					loc_status &= (~discstatus_eof);
				}
				device_data->ctrl_status = loc_status;
				// -------- Release ownership of the resource.
				GIVE_RESOURCE(device_data->ResourceStatusUpdate);

				// --------generate DI if enabled.
				// TODO: MH END OF READ DI.  do we need SI
				if (device_data->DI_enabled ) {
					cpu_request_DI(device_data->bus, device_data->pri, device_data->device_address);
				}
				if (device_data->SI_enabled) {
					cpu_request_SI(device_data->bus, device_data->pri, device_data->device_address);
				}

				break;

				// --------do a write.  Once done say data is available and cause interrupts..
			case locIOcmd_write:
			case locIOcmd_writeDMP:
				break;

			//case locIOcmd_rewon:
			//	current_file_pos[j_unit] = 0;
			//	device_data->eof[j_unit] = true;
			//	break;

			//case locIOcmd_rewoff:
			//	current_file_pos[j_unit] = 0;
			//	device_data->eof[j_unit] = true;
			//	device_data->online[j_unit] = false;
			//	break;

			//case locIOcmd_bkfile:
			//	break;

			//case locIOcmd_fwdfile:
			//	break;

			//case locIOcmd_bkrec:
			//	break;

			//case locIOcmd_fwdrec:
			//	break;

			case locIOcmd_weof:
				break;

				// -------- no outstanding command for this unit
			case 0:
				break;

			default:
				break;

				// --------init -- do flush, start read.
				// --disc-- device_data->case 0:

				// --------set next normal state
				// --disc-- com_state = 1;
				// --disc-- break;

				// -------- DO IO.
				// --disc-- case 1:

				// --------if write in progress, do a write
				// --------for now do any outstanding writes regardless of IO in progress status.  
				// --------cpu may have turned around IO operation before buffer is cleared...
				//if (device_data->write_in_progress) {

				// --------if a write is needed, do the write.
				// --disc-- bytes_to_write = 0;

				// -------- for now limit bytes written to 500
				//--disc--do--while (!device_common_buffer_isempty(&device_data->out_buff) && bytes_to_write < 500) {
				//--disc--do--	if (device_common_buffer_get(&device_data->out_buff, &loc_write_data[bytes_to_write])) {
				//--disc--do--		// --------DEBUG
				//--disc--do--		// fprintf(stderr,"%c", loc_write_data[bytes_to_write]);
				//--disc--do--		// --------END DEBUG
				//--disc--do--		bytes_to_write++;
				//--disc--do--	}
				//--disc--do--}

				//--disc--do--if (bytes_to_write > 0) {
				//--disc--do--	write_status = WriteFile(loc_comm_handle, &loc_write_data, bytes_to_write,
				//--disc--do--		&bytes_written, NULL);
				//--disc--do--	// fprintf(stderr, " disc bytes write requested %d, written %d.  Device Addr %d\n", bytes_to_write, bytes_written, loc_device_addr);

				//--disc--do--	// -------- Request ownership of the resource.
				//--disc--do--	TAKE_RESOURCE(device_data->ResourceStatusUpdate);

				//--disc--do--	// --------signal buffer not full -- ready for more.
				//--disc--do--	if (device_data->write_in_progress) {
				//--disc--do--		device_data->ctrl_status &= (~discstatus_datanotready);
				//--disc--do--	}
				//--disc--do--	// -------- Release ownership of the resource.
				//--disc--do--	GIVE_RESOURCE(device_data->ResourceStatusUpdate);

				//--disc--do--	// --------initiate DI to get more
				//--disc--do--	if (device_data->write_in_progress && device_data->DI_enabled) {
				//--disc--do--		cpu_request_DI(device_data->bus, device_data->pri, device_data->device_address);
				//--disc--do--	}
				//--disc--do--}

				// -------- Request ownership of the resource.
				//--disc--do--TAKE_RESOURCE(device_data->ResourceStatusUpdate);

				// --------signal buffer not full -- ready for more. -- JUST IN CASE.
				//--disc--do--if (device_data->write_in_progress && device_common_buffer_isempty(&device_data->out_buff)) {
				//--disc--do--	device_data->ctrl_status &= (~discstatus_datanotready);
				//--disc--do--}
				// -------- Release ownership of the resource.
				//--disc--do--GIVE_RESOURCE(device_data->ResourceStatusUpdate);

				//}

				// --------if read in progress, do a read.  --- only do a read if buffer is empty.  This way interrupts can keep up!
				// --disc-- if (device_data->read_in_progress && device_common_buffer_isempty(&device_data->in_buff)) {

					// --------do a read. -- non blocking...
					// desired_read_bytes = 1;			// 50;
					// actual_read_bytes = 0;
					//--disc--do--read_status = ReadFile(loc_comm_handle, &loc_read_data,
					//--disc--do--	desired_read_bytes, &actual_read_bytes, NULL);

					// --------got a byte
					//--disc--do--if (actual_read_bytes > 0) {
					//--disc--do--	// fprintf(stderr, " disc bytes read %d.  Device Addr %d\n", actual_read_bytes, loc_device_addr);
					//--disc--do--	for (j = 0; j < actual_read_bytes; j++) {
					//--disc--do--		device_common_buffer_put(&device_data->in_buff, loc_read_data[j]);
					//--disc--do--		if (gbl_capture_disc) {
					//--disc--do--			device_common_capture_disc(loc_read_data[j]);
					//--disc--do--		}
					//--disc--do--	}

					//--disc--do--	// -------- Request ownership of the resource.
					//--disc--do--	TAKE_RESOURCE(device_data->ResourceStatusUpdate);

					//--disc--do--	// --------signal data ready.
					//--disc--do--	device_data->ctrl_status &= (~discstatus_datanotready);

					//--disc--do--	// -------- Release ownership of the resource.
					//--disc--do--	GIVE_RESOURCE(device_data->ResourceStatusUpdate);

					//--disc--do--	// --------initiate DI so they can process this byte.
					//--disc--do--	if (device_data->DI_enabled) {
					//--disc--do--		cpu_request_DI(device_data->bus, device_data->pri, device_data->device_address);
					//--disc--do--	}

					//--disc--do--	// --------echo to disc --- for now ignore error.
					//--disc--do--	bytes_to_write = 1;
					//--disc--do--	write_status = WriteFile(loc_comm_handle, &loc_read_data, bytes_to_write,
					//--disc--do--		&bytes_written, NULL);
					//--disc--do--}

					// --------check for error.
					//--disc--do--if (!read_status) {
					//--disc--do--	DWORD my_last_error = 0;
					//--disc--do--	my_last_error = GetLastError();
					//--disc--do--	fprintf(stderr, " disc read error 0x%08x\n", my_last_error);
					//--disc--do--}
					// --disc-- }

					// --disc-- break;

					// --disc-- case 99:
					// --disc-- loc_stop_request = 1;
					// --disc-- break;


			} // -- end of switch

		} // --- loop over all units to process commands.

		// TODO: set controller not busy

		// wait for next command...
		WaitOnAddress(&(device_data->ctrl_wake), &last_wake, sizeof(last_wake), (DWORD)50);
	} // ---- end of loop forever.  

	// --------close all units...
	if (device_data->disc_file_handle[0] != NULL) {
		device_disc_mh_dismount_unit(device_data->device_address, (SIMJ_U16)0);
	}
	if (device_data->disc_file_handle[1] != NULL) {
		device_disc_mh_dismount_unit(device_data->device_address, (SIMJ_U16)1);
	}
	if (device_data->disc_file_handle[2] != NULL) {
		device_disc_mh_dismount_unit(device_data->device_address, (SIMJ_U16)2);
	}
	if (device_data->disc_file_handle[3] != NULL) {
		device_disc_mh_dismount_unit(device_data->device_address, (SIMJ_U16)3);
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
static DWORD WINAPI device_disc_mh_worker_thread(LPVOID lpParam) {

	SIMJ_U16 loc_device_addr = 0;
	DEVICE_DISC_DATA* device_data = 0;

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
	printf(" Starting device disc_mh at device address %d\n", loc_device_addr);
	if (loc_device_addr == 0) {
		printf(" *** ERROR *** disc_mh device thread didn't get device address \n");
	}

	// -------- get pointer to device data.
	device_data = (DEVICE_DISC_DATA*)(iop_device_buffer[loc_device_addr]);

	// -------- raise the priority of this thread.
	status = SetThreadPriority((HANDLE)iop_device_thread_handle[loc_device_addr], THREAD_PRIORITY_ABOVE_NORMAL);
	if (!status) {
		printf(" *** ERROR *** Failed to raise priority of disc_mh worker thread.  Status 0x%08x\n", GetLastError());
	}

	// --------disable si and di
	device_data->SI_enabled = false;
	device_data->DI_enabled = false;

	// -------- set initial data
	device_data->ctrl_status = (discstatus_noerror | discstatus_datanotready);
	device_common_buffer_set_empty(&device_data->in_buff);  // for now don't clear the input buffer.
	device_common_buffer_set_empty(&device_data->out_buff);

	// --------MOVED COM PORT OPEN TO COM WORKER THREAD...


	// --------initialize comm worker thread.
	comm_thread_handle = device_common_start_thread((LPVOID)device_data,
		device_disc_mh_local_IO_worker_thread,
		&comm_thread_id);

	// -------- comm thread created, fill in information.
	if (comm_thread_handle != 0) {
		printf(" disc_mh device at device address  %02x communications thread created.\n", loc_device_addr);
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


		//	device_disc_mh_process_command( SIMJ_U16 loc_cmd, DEVICE_DISC_DATA* device_data );


		// }

		// --------all commands processed, indicate not busy.
		// loc_status &= (~discstatus_ctrlbusy);

		// -------- if reading and data available, update status.
		// if (dev_reading) {
		//  	if (device_common_buffer_isempty(&device_data->in_buff)) {
		//		loc_status |= discstatus_datanotready;
		//	}
		//	else {
		//		loc_status &= ~discstatus_datanotready;
		//	}
		//}

		// --------if writing and buffer space available, update status.
		//if (dev_writing) {
		//	if (device_common_buffer_isfull(&device_data->out_buff)) {
		//		loc_status |= discstatus_datanotready;
		//	}
		//	else {
		//		loc_status &= ~discstatus_datanotready;
		//	}
		//}

		// --------update device status
		//device_data->ctrl_status = loc_status;
		// if ( our_status != orig_status )
		//	printf("\n Device disc_mh status updated 0x%04x\n", our_status);

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
static void device_disc_mh_mount_unit(SIMJ_U16 device_address, SIMJ_U16 unit, bool read_only, char* filename) {

	// --------get the pointer to the device data
	DEVICE_DISC_DATA* device_data = NULL;
	//char* open_flags;
	//char* open_flags_read = "rb";
	//char* open_flags_write = "wb";
	FILE* loc_disc_file_handle = NULL;
	int file_open_status = 0;
	DWORD last_error = 0;

	// --------only do things if a valid device address...
	if (device_address >= 0 && device_address <= 0x3f) {
		DEVICE_DISC_DATA* device_data = (DEVICE_DISC_DATA*)iop_device_buffer[device_address];

		// --------yes this is a valid device....
		if (device_data != NULL) {
			// --------check for valid unit number
			if (unit >= 0 && unit <= 3) {

				// --------mount the file...
				//if (read_only) {
				//	open_flags = open_flags_read;
				//}
				//else {
				//	open_flags = open_flags_write;
				//}
				// -------- open input disc image file.
				//file_open_status = fopen_s(&loc_disc_file_handle, filename, open_flags);
				file_open_status = device_common_disc_open(filename, read_only, &loc_disc_file_handle, &last_error);

					// -------- if disc file was opened..
				if (file_open_status == 0) {

					// --------set global com handle for other thread.
					device_data->disc_file_handle[unit] = loc_disc_file_handle;
					strcpy_s(device_data->disc_filename[unit], 255, filename);
					device_data->disc_readonly[unit] = read_only;
					device_data->eof[unit] = false;	// end of file.
					device_data->last_recsize[unit] = 0;
					device_data->online[unit] = true;
					device_data->disc_mounted[unit] = true;

					printf(" Device 0x%04x, Unit %d, file: %s mounted.\n", device_address, unit, filename);

				}
				// -------- error opening com port, stop thread...
				else {
					printf(" *** ERROR ***  Trouble opening disc image file: %s  Unit not mounted.\n", filename);
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
void device_disc_mh_dismount_unit(SIMJ_U16 device_address, SIMJ_U16 unit) {

	// --------get the pointer to the device data
	DEVICE_DISC_DATA* device_data = NULL;

	// --------only do things if a valid device address...
	if (device_address >= 0 && device_address <= 0x3f) {
		DEVICE_DISC_DATA* device_data = (DEVICE_DISC_DATA*)iop_device_buffer[device_address];

		// --------yes this is a valid device....
		if (device_data != NULL) {

			// --------check for valid unit number
			if (unit >= 0 && unit <= 3) {

				// --------is there something mounted.
				// --------close the file
				// --------ignore the error
				if (device_data->disc_file_handle[unit] != NULL)
					fclose(device_data->disc_file_handle[unit]);

				device_data->disc_mounted[unit] = false;
				device_data->online[unit] = false;
				device_data->eof[unit] = false;
				device_data->dpi[unit] = 0;
				device_data->last_recsize[unit] = 0;
				device_data->disc_readonly[unit] = true;
				device_data->disc_file_handle[unit] = NULL;
				strcpy_s(device_data->disc_filename[unit], 255, "");

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
void device_disc_mh_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp) {

	DEVICE_DISC_DATA* device_data = 0;
	SIMJ_U16 loc_dev_addr;
	loc_dev_addr = device_address;
	bool status = false;
	int j = 0;

	// --------make certain we aren't double allocating a device and allocate buffer memory for device.
	device_data = device_common_device_buffer_allocate(device_address, sizeof(DEVICE_DISC_DATA));

	if (device_data != NULL) {

		// -------- fill in global buffer.
		// -------- standard information for ALL controllers
		device_data->device_address = loc_dev_addr;
		// TODO: for now make disc write locked.
		device_data->ctrl_status = discstatus_noerror | discstatus_wld | discstatus_datanotready |
				discstatus_dualplatter | discstatus_writeprot | discstatus_type_200tpi;
		que_uword_init(&(device_data->ctrl_command_que));
		device_data->ctrl_wake = 0;
		device_data->bus = bus;
		device_data->pri = prio;
		device_data->dmp = dmp;
		strcpy_s(device_data->info, 40, "disc");

		// --------data specific to this device.
		device_common_buffer_init(&device_data->in_buff);
		device_common_buffer_init(&device_data->out_buff);

		// --------initialize the resource for updating status.
		// Initialize the resource one time only.
		status = INIT_RESOURCE(device_data->ResourceStatusUpdate);
		if (!status) {
			printf(" *** ERROR *** disc_mh device could not create status update locking mechanism.\n");
		}

		device_data->cur_sel_unit = 0;		// which unit is selected for IO
		for (j = 0; j < 4; j++) {
			device_data->io_cmd[j] = 0;		// IO commands.  1=read, 2=write, 3=rew on, 4=rew off, 5=bk file, 6=fwd file
			//				 7=bk rec, 8=fwd rec	
			device_data->dpi[j] = 0;
			device_data->plat[j] = 0;		// saved platter
			device_data->head[j] = 0;		// saved head
			device_data->cyl[j] = 0;		// saved cyl
			device_data->disc_mounted[j] = false;
			device_data->online[j] = false;
			device_data->eof[j] = false;
			device_data->last_recsize[j] = 0;
			device_data->disc_readonly[j] = true;
			device_data->disc_file_handle[j] = NULL;
			strcpy_s(device_data->disc_filename[j], 255, "");
		}


		// --------initialize main worker thread.
		device_common_thread_init((LPVOID)device_data,
			device_disc_mh_worker_thread,
			device_disc_mh_output_data,
			device_disc_mh_output_cmd,
			device_disc_mh_input_data,
			device_disc_mh_input_status,
			device_disc_mh_mount_unit,
			device_disc_mh_dismount_unit);
	}
}



// ============================================================================================================================
// -------- Disk commands

// -------- 0 1 2 3 4 5 6 7 8 9 a b c d e f

// -------- 1 0 x x 0 x x x x x x x x x x x   transfer initiate write programmed IO
// -------- 1 0 x x 1 x x x x x x x x x x x   transfer initiate read programmed IO

// -------- 1 1 x x 0 x x x x x x x x x x x   transfer initiate write dmp
// -------- 1 1 x x 1 x x x x x x x x x x x   transfer initiate read dmp

// -------- 0 1 x x 1 0 x x x x x x x x x x   eob
// -------- 0 1 x x 0 1 x x x x x x x x x x   term
// -------- 0 1 x x 1 1 x x x x x x x x x x   eob and term
// -------- 0 1 x x 0 0 1 x x x x x x x x x   cyl select
// -------- 0 1 x x 0 0 0 x x x x x x x x x   no - op

// -------- 0 0 x x x x x x x x x x x x x x   head.drive select
// 
// --------command
//#define cmd_cmd_mask			0xC000		// command mask
// --------various commands.
//#define cmd_cmd_ti				0x8000		// do a transfer initiate command
//#define cmd_cmd_ti_dmp			0xC000		// do a transfer initiate command w/dmp
//#define cmd_cmd_ctrl			0x4000		// do a control command (parse sub commands.)
//#define cmd_cmd_select			0x0000		// do a head/drive select 

//#define cmd_ctrl_mask			0x0C00		// ctrl sub command mask
//#define cmd_ctrl_eobterm		0x0C00		// eob and term
//#define cmd_ctrl_eob			0x0800		// eob
//#define cmd_ctrl_term			0x0400		// eob
//#define cmd_ctrl_noop			0x0000		// no-op or cyl select


void device_disc_mh_process_command(SIMJ_U16 loc_cmd, DEVICE_DISC_DATA* device_data) {

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
	//int chg_unit = 0;
	int loc_io_cmd = 0;
	int loc_unit = 0;

	// 200TPI disk has: (I think)
	//		1632 tracks/disk
	//		2 platters
	//		816 tracks/platter
	//		2 heads
	//		408 tracks/head
	//		24 sec/track 
	SIMJ_U16 subcmd_type = 0;
	SIMJ_U64 loc_sector = 0;	// sector within track
	SIMJ_U64 loc_device_sector = 0;	// abs disk sector.
#define SEC_PER_TRACK	24
#define TRK_PER_HEAD	408
#define TRK_PER_PLAT	816


	// --------get the type of command.
	cmd_type = loc_cmd & cmd_cmd_mask;

	// -------- Request ownership of the resource.
	TAKE_RESOURCE(device_data->ResourceStatusUpdate);

	// --------get internal status to work on...
	loc_status = device_data->ctrl_status;
	orig_status = loc_status;

	// --------process the various command types.
	switch (cmd_type) {

		// --------transfer initiate
		//  The Transfer Initiate Command generates a Read or Write operation,
		//  determines mode of transfer.
		//
		// TODO: what is SCS?
		//  The Transfer Initiate Command specifies whether a Single Cycle Scan
		//  is generated prior to the Read or Write.
		// 
		//  Options:
		//       Bit 1 - DMP mode = 1
		//		 Bit 2 - SI enabled = 1
		//		 Bit 3 - Di enabled = 1
		//		 Bit 4 - Read = 1, else write
		//		 Bit 5 - on WRITE - set EOD
		//       Bit 6 - on WRITE - set EOF
		//		 Bit 7 - on WRITE - set IEOR
		//		 Bit 8 - Enable SCS=1 
		//		 Bits 11-15 - sector
		//
		case cmd_cmd_ti_dmp:	//	0xC000		// do a transfer initiate command w/dmp
			// --------store tc/ta information
			iop_get_dmp_parameters(device_data->device_address, device_data->dmp, &(device_data->dmp_tc), 
						&(device_data->dmp_ta), &(device_data->dmp_virt), &(device_data->dmp_abs_tc_addr));
			// --------fall through to reset of transfer initiate.

		case cmd_cmd_ti:		//	0x8000		// do a transfer initiate command

			// -------- get local sector number from command, then form abs sector number
			loc_sector = loc_cmd & cmd_ti_sectormask;
			loc_unit = device_data->cur_sel_unit;
			loc_device_sector = loc_sector + ( device_data->cyl[loc_unit] * 2 + ((device_data->head[loc_unit] != 0) ? 1 : 0)) * SEC_PER_TRACK +
				((device_data->plat[loc_unit] != 0) ? TRK_PER_PLAT : 0);
#if DEBUG_DISC_MH >= 2
			fprintf(stderr, " device_disc_mh - transfer initiate - Dev addr: %d, cmd 0x%04x, unit %d, device sector %I64u\n",
				device_data->device_address, loc_cmd, loc_unit, loc_device_sector);
			fprintf(stderr, " device_disc_mh - transfer initiate - plat: %zd, head: %zd, cyl: %zd, sect: %zd\n",
				device_data->plat[loc_unit], device_data->head[loc_unit], device_data->cyl[loc_unit], loc_sector);
#endif
			device_data->dpi[loc_unit] = loc_device_sector;

			// -------- enable or disable interrupts.
			device_disc_mh_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);

			// --------start a write.  -- notice the ! in the if...
			if (!(loc_cmd & cmd_ti_read)) {
#if DEBUG_DISC_MH >= 2
				fprintf(stderr, " device_disc_mh - TRANSFER INITIATE - write requested.  Dev addr: %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif
				// --------get old write status
				old_write = device_data->write_in_progress;

				// -------- indicate transfer in progress
				chg_rd = -1;
				chg_wrt = 1;
				loc_status |= (discstatus_ctrlbusy|discstatus_datanotready);

				// -------- if ready for a byte send DI.   If we weren't already doing a write, send DI to get data.
				// TODO: Fix DI logic
				// if ((device_data->DI_enabled || (chg_di == 1)) && !old_write) {
				// 	need_DI = true;
				// }
				
				// --------parse special types of writes..... EOD ??, EOF - end of file, IEOR ??  are there combinations ??
				if (loc_cmd & cmd_ti_eod) {
					loc_io_cmd = locIOcmd_weod;
				}
				else if (loc_cmd & cmd_ti_eof) {
					loc_io_cmd = locIOcmd_weof;
				}
				else if (loc_cmd & cmd_ti_ieor) {
					loc_io_cmd = locIOcmd_wieor;
				}
				else {
					// --------indicate write command
					if (cmd_type == cmd_cmd_ti_dmp) {
						loc_io_cmd = locIOcmd_writeDMP;
					}
					else {
						loc_io_cmd = locIOcmd_write;
					}
				}
			}
			// --------start a read.
			else {
#if DEBUG_DISC_MH >= 2
				fprintf(stderr, " device_disc_mh - TRANSFER INITIATE - read requested.  Dev addr: %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif
				// -------- get old read status
				// old_read = device_data->read_in_progress;

				// -------- indicate transfer in progress
				chg_rd = 1;
				chg_wrt = -1;
				loc_status |= (discstatus_ctrlbusy | discstatus_datanotready);

				// --------indicate write command
				if (cmd_type == cmd_cmd_ti_dmp) {
					loc_io_cmd = locIOcmd_readDMP;
				}
				else {
					loc_io_cmd = locIOcmd_read;
				}
			}

			// --------what interrupts are caused by transfer initiate???   (Always SI to signal completion??,   DI when starting write?)

			// --------generate SI if enabled.
			// ADD SI FOR TESTING.. NOT ORIGINAL.
			if (device_data->SI_enabled) {
				need_SI = true;
			}
			break;

		
		// --------select the head and drive unit
		// --------nothing is sent to the IO thread.  data stored globally
		case cmd_cmd_select:	//  0x0000		// do a head/drive select
			loc_unit = loc_cmd & cmd_sel_unitmask;
			// TODO: need to process PREP MODE, CONT SCAN, IGN bits...
			// --------update global data.
			device_data->cur_sel_unit = loc_unit;
			device_data->head[loc_unit] = ((loc_cmd & cmd_sel_head) ? 1 : 0);
			device_data->plat[loc_unit] = ((loc_cmd & cmd_sel_plat) ? 1 : 0);
#if DEBUG_DISC_MH >= 2
			fprintf(stderr, " device_disc_mh - UNIT HEAD SEL Command. Dev Addr %d, cmd 0x%04x unit: %d plat: %d, head: %d\n",
				device_data->device_address, loc_cmd, loc_unit, (loc_cmd & cmd_sel_plat), (loc_cmd & cmd_sel_head) );
#endif
			loc_status &= (~(discstatus_ctrlbusy | discstatus_unitmask));
			loc_status |= ( discstatus_datanotready|(loc_unit & discstatus_unitmask));

			// --------generate SI if enabled.
			if (device_data->SI_enabled) {
				need_SI = true;
			}

			break;

		// --------various control commands.
		case cmd_cmd_ctrl:		//	0x4000		// do a control command (parse sub commands.)
			subcmd_type = loc_cmd & cmd_ctrl_mask;

			switch (subcmd_type) {

				// --------terminate and eob
				case cmd_ctrl_eobterm:		// 0x0C00  eob and term
#if DEBUG_DISC_MH >= 2
					fprintf(stderr, " device_disc_mh - TERM/EOB Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif
					// --------stop all I/O
					chg_rd = -1;
					chg_wrt = -1;

					// --------update status
					loc_status |= (discstatus_datanotready);		// set no data ready
					loc_status &= (~discstatus_ctrlbusy);			// clear busy

					// TODO: process IGN and MPE

					// --------generate DI if enabled. for EOB
					if (device_data->DI_enabled || (chg_di_ena == 1)) {
						need_DI = true;
					}

					// --------generate SI if enabled.
					if (device_data->SI_enabled || (chg_si_ena == 1)) {
						need_SI = true;
					}
					break;

				// --------eob
				case cmd_ctrl_eob:			// 0x0800  eob
#if DEBUG_DISC_MH >= 2
					fprintf(stderr, " device_disc_mh - EOB Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif
					// --------stop all I/O
					chg_rd = -1;
					chg_wrt = -1;

					// --------update status
					loc_status |= (discstatus_datanotready);		// set no data ready
					loc_status &= (~discstatus_ctrlbusy);			// clear busy

					// TODO: process IGN and MPE

					// --------generate DI if enabled. for EOB
					if (device_data->DI_enabled || (chg_di_ena == 1)) {
						need_DI = true;
					}

					// --------maybe no SI for just EOB...
					// --------generate SI if enabled.
					if (device_data->SI_enabled || (chg_si_ena == 1)) {
						need_SI = true;
					}
					break;

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
				//  SI and DI bits in command should be off (ignored anyway)
				//
				case cmd_ctrl_term:			// 0x0400  eob

#if DEBUG_DISC_MH >= 2
					fprintf(stderr, " device_disc_mh - TERM Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif
					// --------stop all I/O
					chg_rd = -1;
					chg_wrt = -1;

					// --------update status
					loc_status |= (discstatus_datanotready);		// set no data ready
					loc_status &= (~discstatus_ctrlbusy);			// clear busy

					// TODO: process IGN and MPE

					// --------generate SI if enabled.
					if (device_data->SI_enabled || (chg_si_ena == 1)) {
						need_SI = true;
					}
					break;

				// --------cyl select or noop
				//	The controller accepts the NO-OP command regardless of controller
				//	busy status.  The data or service interrupts can be enabled or disabled
				//	as specified by bits 2 and 3.  
				case cmd_ctrl_noop:			// 0x0000  no-op or cyl select

					// -------- enable or disable interrupts.
					device_disc_mh_enable_disable_SI_DI(loc_cmd, device_data, &chg_si_ena, &chg_di_ena);

					// --------cylinder select
					// --------store cylinder for later...
					if (loc_cmd & cmd_noop_cylsel) {
#if DEBUG_DISC_MH >= 2
						fprintf(stderr, " device_disc_mh - CYL SEL Command. Dev Addr %d, cmd 0x%04x cyl: %d \n",
								device_data->device_address, loc_cmd, loc_cmd & cmd_cylsel_cylmask);
#endif
						device_data->cyl[device_data->cur_sel_unit] = loc_cmd & cmd_cylsel_cylmask;

					}
					// --------noop ---- nothing to do...
					else {
#if DEBUG_DISC_MH >= 2
						fprintf(stderr, " device_disc_mh - NOOP Command. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif
					}
					// --------generate SI if enabled.
					// ADD SI FOR TESTING.. NOT ORIGINAL.
					if (device_data->SI_enabled) {
						need_SI = true;
					}

					break;

				// --------something unexpected....
				default:
#if DEBUG_DISC_MH >= 1
					fprintf(stderr, " device_disc_mh - CTRL ??? Command - ignored. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif
					break;
			}
			break;


		// --------unexpected command...
	default:
		msg_unexpected_cmd = true;

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
	//if (chg_unit == 1) {
	//	device_data->cur_sel_unit = loc_unit;
	//}

	// -------- if reading and data available, update status.
	if (device_data->read_in_progress) {
		if (device_common_buffer_isempty(&device_data->in_buff)) {
			loc_status |= discstatus_datanotready; // no data to read.
		}
		else {
			loc_status &= (~discstatus_datanotready); // something to read.
		}
	}

	// --------if writing and buffer space available, update status.
	if (device_data->write_in_progress) {
		if (device_common_buffer_isfull(&device_data->out_buff)) {
			loc_status |= discstatus_datanotready; // cant write
		}
		else {
			loc_status &= (~discstatus_datanotready); // can write
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
		device_data->io_cmd[device_data->cur_sel_unit] = loc_io_cmd;
		device_data->ctrl_wake++;
		WakeByAddressSingle((PVOID) & (device_data->ctrl_wake));

		// --------allow other threads to run
		SwitchToThread();
	}

	// --------diag/debug messages....
#if DEBUG_DISC_MH >= 2
	if (msg_term_icb)
		fprintf(stderr, " device_disc_mh - terminate w/ICB requested. Dev Addr %d, cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif

#if DEBUG_DISC_MH >= 1
	if (msg_unexpected_cmd)
		fprintf(stderr, " device_disc_mh - unexpected command.  Dev addr: %d,  cmd 0x%04x\n", device_data->device_address, loc_cmd);
#endif
}
