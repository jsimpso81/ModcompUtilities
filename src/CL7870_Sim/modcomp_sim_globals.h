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

// -------- main memory
SIMJ_SCOPE volatile SIMJ_U16  gbl_mem[2097152];

// -------- items for front panel
SIMJ_SCOPE volatile SIMJ_U16 gbl_fp_switches SIMJ_INIT( 0 );
SIMJ_SCOPE volatile SIMJ_U16 gbl_fp_datalights SIMJ_INIT( 0 );
SIMJ_SCOPE volatile SIMJ_U16 gbl_fp_addrlights SIMJ_INIT( 0 );
SIMJ_SCOPE volatile SIMJ_U16 gbl_regselectswitches SIMJ_INIT( 0 );
SIMJ_SCOPE volatile bool gbl_fp_powerlight SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_standbylight SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_backupfailure SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_runlight SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_cc_n_light SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_cc_z_light SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_cc_o_light SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_cc_c_light SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_io_light SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_task_light SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_memerror_light SIMJ_INIT( false );
SIMJ_SCOPE volatile bool gbl_fp_single_step SIMJ_INIT( false );

SIMJ_SCOPE volatile bool gbl_fp_keylocked SIMJ_INIT( false );

// -------- not done more front panel info

// -------- CPU ITEMS (for debug)
SIMJ_SCOPE volatile SIMJ_U32 cpu_inst_used[256] SIMJ_INIT( { 0 } );

// -------- IO DEVICES
//SIMJ_SCOPE  volatile SIMJ_U16 iop_last_dev_status[64] SIMJ_INIT( { 0 } );

SIMJ_SCOPE DEVICE_OUTPUT_DATA  iop_output_data_proc[64] SIMJ_INIT( { 0 } );
SIMJ_SCOPE DEVICE_OUTPUT_CMD   iop_output_cmd_proc[64] SIMJ_INIT( { 0 } );
SIMJ_SCOPE DEVICE_INPUT_DATA   iop_input_data_proc[64] SIMJ_INIT( { 0 } );
SIMJ_SCOPE DEVICE_INPUT_STATUS iop_input_status_proc[64] SIMJ_INIT( { 0 } );

SIMJ_SCOPE volatile void* iop_device_buffer[64];
SIMJ_SCOPE volatile int iop_thread_stop_request[64];
SIMJ_SCOPE volatile int iop_thread_stop_request2[64];
SIMJ_SCOPE volatile uintptr_t  iop_device_thread_handle[64];
SIMJ_SCOPE volatile uintptr_t  iop_device_thread_handle2[64];
SIMJ_SCOPE volatile DWORD   iop_device_thread_id[64];
SIMJ_SCOPE volatile DWORD   iop_device_thread_id2[64];


// -------- execution options
SIMJ_SCOPE bool volatile gbl_verbose_debug SIMJ_INIT( false);
SIMJ_SCOPE bool volatile gbl_capture_console SIMJ_INIT( false );


// ================================================================================================
