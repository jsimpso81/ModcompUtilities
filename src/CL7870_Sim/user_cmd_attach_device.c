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
	bool goode2 = false;
	SIMJ_U16 port = 0;
	SIMJ_U16 console_baud = 9600;

//#define 			0
//#define 			1
//#define 		2
//#define 			3
//#define 			4
//#define DEVICE_TYPE_DISC_LX			5
//#define DEVICE_TYPE_DISC_IPS2		6
//#define DEVICE_TYPE_A4811			7
//#define DEVICE_TYPE_A4804			8
//#define DEVICE_TYPE_MODACSIII		9
//#define DEVICE_TYPE_MODACS1600		10

	switch (device_type) {
		case DEVICE_TYPE_NULL:  //  "null",
			device_null_init(device_address, bus, prio, dmp);
			Sleep(1000);
			break;

		case DEVICE_TYPE_CONSOLE:  //  "console", da, bus, prio, dmp, <com1>
				 // console com port is set to 19200, 8, N, 1, currently this is hard coded.
			console_baud = 9600;
			if (extra_count <= 0) {
				device_console_init(device_address, bus, prio, dmp, "COM1", console_baud);
			}
			else if (extra_count == 1 ) {
				device_console_init(device_address, bus, prio, dmp, extra1, console_baud);
			}
			else {
				goode2 = user_cmd_parse_u16(extra2, &console_baud, 0, 0xffff);
				if (!goode2) {
					printf(" *** ERROR *** Not a valid baud rate: %s\n", extra2);
				}
				else if (console_baud != 300 && console_baud != 1200 && console_baud != 2400 &&
					console_baud != 4800 && console_baud != 9600 && console_baud != 19200) {
					printf(" *** ERROR *** Not a valid baud rate: %s\n", extra2);
				}
				else {
					device_console_init(device_address, bus, prio, dmp, extra1, console_baud);
				}
			}
			Sleep(1000);
			break;

		case DEVICE_TYPE_CONSOLETCP:  //  "consoletcp", da, bus, prio, dmp, port<3000>
			if (extra_count <= 0) {
				device_console_tcp_init(device_address, bus, prio, dmp, (SIMJ_U16)3000);
			}
			else {
				goode1 = user_cmd_parse_u16(extra1, &port, 0, 0xffff);
				if (!goode1) {
					printf(" *** ERROR *** Not a valid TCP PORT: %s\n", extra1);
				}
				else {
					device_console_tcp_init(device_address, bus, prio, dmp, port);
				}
			}
			Sleep(1000);
			break;

		case DEVICE_TYPE_TAPE:  //  "tape", da, bus, prio, dmp
			device_tape_init(device_address, bus, prio, dmp);
			break;

		case DEVICE_TYPE_DISC_MH:  //  "disc_mh", da, bus, prio, dmp
			device_disc_mh_init(device_address, bus, prio, dmp);
			break;

		case DEVICE_TYPE_DISC_LX:  //  "disc_lx", da, bus, prio, dmp
			printf(" *** ERROR *** disc_lx device not yet supported\n");
			break;

		case DEVICE_TYPE_DISC_IPS2:  //  "disc_ips2", da, bus, prio, dmp
			printf(" *** ERROR *** disc_ips2 device not yet supported\n");
			break;

		case DEVICE_TYPE_A4811:  //  "a4811", da, bus, prio, dmp, start_port
			printf(" *** ERROR *** a4811 device not yet supported\n");
			break;

		case DEVICE_TYPE_A4808:  //  "a4808", da, bus, prio, dmp, start_port
			printf(" *** ERROR *** a4808 device not yet supported\n");
			break;

		case DEVICE_TYPE_MODACSIII:  //  "modacsIII" da, bus, prio, dmp, cardfilename
			printf(" *** ERROR *** modacsIII device not yet supported\n");
			break;

		case DEVICE_TYPE_MODACS1600:  //  "modacs1600" da_start, bus, prio, dmp, card_model
			// --------some cards use more than one consecutive da.  other devices will be
			// --------automatically created.
			printf(" *** ERROR *** modacs1600 device not yet supported\n");
			break;

		default:
			break;
	}



	return;
}