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

// --------mount devices.  
void user_cmd_mount_device(SIMJ_U16 device_address, SIMJ_U16 unit, char* filename, bool readonly) {

	DEVICE_NULL_DATA* databuffer = NULL;

	// -------- see if device address is good
	if (device_address < 0 || device_address > 0x3f) {
		printf(" *** ERROR *** Mount command.  Not a valid device address (0-0x3f) %d\n", device_address);
		return;
	}

	// -------- see if unit number is good
	if (unit < 0 || unit > 3) {
		printf(" *** ERROR *** Mount command.  Not a valid unit number (0-3) %d\n", unit);
		return;
	}

	// -------- see if file exists (maybe...)
	// TODO: check if file exists.

	// -------- see if device is configured..
	databuffer = (DEVICE_NULL_DATA*)iop_device_buffer[device_address];
	if (databuffer == NULL) {
		printf(" *** ERROR *** Mount command.  No device configured at this device address %d\n", device_address);
		return;
	}
	if (iop_mount_unit_proc[device_address] == NULL) {
		printf(" *** ERROR *** Mount command.  No mount procedure configured for this device address %d\n", device_address);
		return;
	}

	// -------- call the configuration routine.
	(*iop_mount_unit_proc[device_address])(device_address, unit, readonly, filename );

	return;
}
