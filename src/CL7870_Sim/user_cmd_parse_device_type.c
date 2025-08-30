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

	// --------list of devices.
	// --------THIS LIST MUST MATCH THE TYPES DEFINED in simj_base.h
	const char* valid_devices[VALID_DEVICE_COUNT] = {
			"null",
			"console",
			"consoletcp",
			"tape",
			"disc_mh",
			"disc_lx",
			"disc_ips2",
			"a4811",
			"a4808",
			"modacsIII",
			"modacs1600"
	};


	SIMJ_U16 j;
	SIMJ_U16 found_device = 9999;

	// --------set we didn't find a device.
	bool retval = false;

	// -------- loop through all the device types searching for a match.
	for (j = 0; j < VALID_DEVICE_COUNT; j++) {
		// -------- did we find a match.
		if (strcmp(in_device, valid_devices[j]) == 0) {
			// -------- found a match, set the device and return=true
			found_device = j;
			*out_device_type = found_device +1;
			retval = true;
			break;
		}
	}

	// -------- not a valid device, print error.
	if (!retval) {
		printf(" *** ERROR *** Not a valid device type: %s\n", in_device);
	}

	return retval;

}