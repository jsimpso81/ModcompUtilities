// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			generic_device_variables.h
//
//	Description:	defines variables used by all deivces.
//
//	Externally accessible routines:
// 
// Notes:			Custom variables for each device are added after these.
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================

SIMJ_U16 device_address;
volatile SIMJ_U16 ctrl_wake;
volatile SIMJ_U16 ctrl_status;
volatile SIMJ_U16 bus;	// IO bus 0-3
volatile SIMJ_U16 dmp;	// for non dmp devices, specify 0
volatile SIMJ_U16 pri;	// priority 0 - 15 (or is it 1 - 15)
volatile bool SI_enabled;
volatile bool DI_enabled;
volatile bool write_in_progress;
volatile bool read_in_progress;
char info[40];	// short description
volatile DEVICE_BUFFER in_buff;
volatile DEVICE_BUFFER out_buff;
volatile QUEUE_UWORD ctrl_command_que;		// may be obsolete
