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
// #include <winsock2.h>
// #pragma comment(lib, "ws2_32.lib") // Link with ws2_32.lib

// -------- open a modcomp emulator disc image file.
int device_common_disc_open(char* disc_filename, bool read_only, FILE** disc_file_handle, DWORD* last_error) {


    errno_t loc_status;
    FILE* loc_disc_file_handle;

    // -------- open input disc image file.
    // -------- read only
    if (read_only) {
        loc_status = fopen_s(&loc_disc_file_handle, disc_filename, "rb");
    }
    // -------- read / write
    else {
        // TODO: for now the disc file must exist first to write....
        loc_status = fopen_s(&loc_disc_file_handle, disc_filename, "r+b");
    }

    if (loc_status != 0) {
        fprintf(stderr, " *** ERROR *** device_common_disc_open.  Could not open disc image file: %d, %s\n\n", loc_status, disc_filename);
        *last_error = loc_status;
        return 1;
    }

    // TODO: Add more info on connection to message..
    printf(" *** INFO *** device_common_disc_open - File opened:  %s...\n", disc_filename);

    // --------return the socket info and set no error
    *disc_file_handle = loc_disc_file_handle;
    *last_error = 0;
    return 0;

}





