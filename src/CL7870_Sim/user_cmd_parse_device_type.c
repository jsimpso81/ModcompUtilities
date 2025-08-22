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

// -------- see if the device specified in the attach command is a valid device.  return the device index value.
bool user_cmd_parse_device_type(char* in_device, SIMJ_U16* out_device_type) {

	const char* valid_devices[10] = {
			"null",
			"console",
			"consoletcp",
			"tape",
			"disk_lx",
			"disk_ips2",
			"a4811",
			"a4808",
			"modacsIII",
			"modacs1600"
	};

	const SIMJ_U16 valid_device_count = 8;

	SIMJ_U16 j;
	SIMJ_U16 found_device = 9999;

	bool retval = false;

	for (j = 0; j < valid_device_count; j++) {
		if (strcmp(in_device, valid_devices[j]) == 0) {
			found_device = j;
			*out_device_type = j+1;
			retval = true;
			break;
		}
	}

	if (!retval) {
		printf(" *** ERROR *** Not a valid device type: %s\n", in_device);
	}

	return retval;

}