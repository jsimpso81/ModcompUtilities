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

// --------attach devices.  
// --------where devices have extra parameters, this routine will parse and error check them before 
// --------calling the routine to configure the device.
void user_cmd_attach_device(SIMJ_U16 device_type, SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp, 
							SIMJ_U16 extra_count, char* extra1, char* extra2, char* extra3) {
	 
	bool goode1 = false;
	SIMJ_U16 port = 0;


	switch (device_type) {
		case 1:  //  "null",
			device_null_init(device_address, bus, prio, dmp);
			Sleep(1000);
			break;

		case 2:  //  "console", da, bus, prio, dmp, <com1>
				 // console com port is set to 19200, 8, N, 1, currently this is hard coded.
			if (extra_count <= 0) {
				device_console_init(device_address, bus, prio, dmp, "COM1");
			}
			else {
				device_console_init(device_address, bus, prio, dmp, extra1);
			}
			Sleep(1000);
			break;

		case 	3:  //  "consoletcp", da, bus, prio, dmp, port<3000>
			if (extra_count <= 0) {
				device_console_tcp_init(device_address, bus, prio, dmp, (SIMJ_U16)3000);
			}
			else {
			goode1 = user_cmd_parse_u16(extra1, &port, 0, 0xffff);
			if (!goode1) {
				printf(" *** ERROR *** Not a valid TCP PORT: %s\n", extra1);
			}
				device_console_tcp_init(device_address, bus, prio, dmp, port);
			}
			Sleep(1000);
			break;

		case 	4:  //  "tape", da, bus, prio, dmp, unit, filename
			printf(" *** ERROR *** tape device not yet supported\n");
			break;

		case 	5:  //  "disk_lx", da, bus, prio, dmp, unit, filename,
			printf(" *** ERROR *** disk_lx device not yet supported\n");
			break;

		case 	6:  //  "disk_ips2", da, bus, prio, dmp, unit, filename,
			printf(" *** ERROR *** disk_ips2 device not yet supported\n");
			break;

		case 	7:  //  "a4811", da, bus, prio, dmp, start_port
			printf(" *** ERROR *** a4811 device not yet supported\n");
			break;

		case 	8:  //  "a4808", da, bus, prio, dmp, start_port
			printf(" *** ERROR *** a4808 device not yet supported\n");
			break;

		case 	9:  //  "modacsIII" da, bus, prio, dmp
			printf(" *** ERROR *** modacsIII device not yet supported\n");
			break;

		case 	10:  //  "modacs1600" da_start, bus, prio, dmp, card_model
			// --------some cards use more than one consecutive da.  other devices will be
			// --------automatically created.
			printf(" *** ERROR *** modacs1600 device not yet supported\n");
			break;

		default:
			break;
	}



	return;
}