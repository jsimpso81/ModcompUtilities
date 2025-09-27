// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			modcomp_sim_globals.h
//
//	Description:	Defines global memory variables.
//
//	Externally accessible routines:
// 
// Notes:
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================

#ifndef SIMJ_MAIN
#define SIMJ_SCOPE extern 
#define SIMJ_INIT( A )
#else
#define SIMJ_SCOPE
#define SIMJ_INIT( A )  = A
#endif

#include "modcomp_opcode_string.h"

// -------- main memory
SIMJ_SCOPE volatile SIMJ_U16  gbl_mem[2097152];

// -------- items for front panel
SIMJ_SCOPE volatile SIMJ_U16 gbl_fp_switches SIMJ_INIT( 0 );
//SIMJ_SCOPE volatile SIMJ_U16 gbl_fp_datalights SIMJ_INIT( 0 );
//SIMJ_SCOPE volatile SIMJ_U16 gbl_fp_addrlights SIMJ_INIT( 0 );
//SIMJ_SCOPE volatile SIMJ_U16 gbl_regselectswitches SIMJ_INIT( 0 );
//SIMJ_SCOPE volatile bool gbl_fp_powerlight SIMJ_INIT( false );
//SIMJ_SCOPE volatile bool gbl_fp_standbylight SIMJ_INIT( false );
//SIMJ_SCOPE volatile bool gbl_fp_backupfailure SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_runlight SIMJ_INIT( false );
//SIMJ_SCOPE volatile bool gbl_fp_cc_n_light SIMJ_INIT( false );
//SIMJ_SCOPE volatile bool gbl_fp_cc_z_light SIMJ_INIT( false );
//SIMJ_SCOPE volatile bool gbl_fp_cc_o_light SIMJ_INIT( false );
//SIMJ_SCOPE volatile bool gbl_fp_cc_c_light SIMJ_INIT( false );
//SIMJ_SCOPE volatile bool gbl_fp_io_light SIMJ_INIT( false );
//SIMJ_SCOPE volatile bool gbl_fp_task_light SIMJ_INIT( false );
//SIMJ_SCOPE volatile bool gbl_fp_memerror_light SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_single_step SIMJ_INIT( false );

SIMJ_SCOPE volatile bool gbl_fp_keylocked SIMJ_INIT( false );

// -------- not done more front panel info

// -------- CPU ITEMS (for debug)
SIMJ_SCOPE volatile SIMJ_U32 cpu_inst_used[NUMB_OPSI] SIMJ_INIT( { 0 } );

// -------- IO DEVICES

// -------- these are all indexed by device address..
// -------- procedure for ODx instruction (output data)
SIMJ_SCOPE DEVICE_OUTPUT_DATA  iop_output_data_proc[64] SIMJ_INIT( { 0 } );
// -------- procedure for OCx instruction (output command)
SIMJ_SCOPE DEVICE_OUTPUT_CMD   iop_output_cmd_proc[64] SIMJ_INIT( { 0 } );
// -------- procedure for IDx instruction (input data)
SIMJ_SCOPE DEVICE_INPUT_DATA   iop_input_data_proc[64] SIMJ_INIT( { 0 } );
// -------- procedure for ISx instruction (input status)
SIMJ_SCOPE DEVICE_INPUT_STATUS iop_input_status_proc[64] SIMJ_INIT( { 0 } );
// -------- procedure for mounting image files to device units.  This can be done on the fly.
SIMJ_SCOPE DEVICE_MOUNT_UNIT   iop_mount_unit_proc[64] SIMJ_INIT( { 0 } );
// -------- procedure for dismount image files from device units.  This can be done on the fly.
SIMJ_SCOPE DEVICE_DISMOUNT_UNIT iop_dismount_unit_proc[64] SIMJ_INIT( { 0 } );

// -------- buffer to store device specific data
SIMJ_SCOPE volatile void* iop_device_buffer[64];
// -------- request word to stop the main device thread.
SIMJ_SCOPE volatile int iop_thread_stop_request[64];
// -------- request word to stop the secondary device (IO worker) thread.
SIMJ_SCOPE volatile int iop_thread_stop_request2[64];
// -------- store the main thread handle
SIMJ_SCOPE volatile uintptr_t  iop_device_thread_handle[64];
// -------- store the secondary thread (io worker) thread handle 
SIMJ_SCOPE volatile uintptr_t  iop_device_thread_handle2[64];
// -------- thread ID for main device thread.
SIMJ_SCOPE volatile DWORD   iop_device_thread_id[64];
// -------- thread ID for secondary (IO worker) thread.
SIMJ_SCOPE volatile DWORD   iop_device_thread_id2[64];

// -------- variables, indexed by DMP number, to store parameters from DMPI instructions.
// -------- page number in actual memory of MAP image
SIMJ_SCOPE volatile SIMJ_U16 iop_vdmp_miap_page[64];
// -------- number of pages in MAP image.
SIMJ_SCOPE volatile SIMJ_U16 iop_vdmp_miap_length[64];


// -------- execution options
SIMJ_SCOPE VERBOSE_DEBUGGING volatile gbl_verbose_debug SIMJ_INIT( debugging_off );
SIMJ_SCOPE bool volatile gbl_capture_console SIMJ_INIT( false );


// ================================================================================================
