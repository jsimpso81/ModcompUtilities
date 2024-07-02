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

void user_cmd_attach_device(SIMJ_U16 device_type, SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp, SIMJ_U16 extra_count, char* extra1, char* extra2) {
	 

	switch (device_type) {
		case 1:  //  "null",
			device_null_init(device_address, bus, prio, dmp);
			Sleep(1000);
			break;

		case 2:  //  "console",
			device_console_init(device_address, bus, prio, dmp);
			Sleep(1000);
			break;

		case 	3:  //  "consoletcp",
			break;

		case 	4:  //  "tape",
			break;

		case 	5:  //  "disk_lx",
			break;

		case 	6:  //  "disk_ips2",
			break;

		case 	7:  //  "a4811",
			break;

		case 	8:  //  "modacs"
			break;

		default:
			break;
	}



	return;
}