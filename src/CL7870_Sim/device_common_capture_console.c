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

static volatile bool files_open = false;
static FILE* text_file;
static FILE* bin_file;
static volatile int item_count = 0;

void device_common_capture_console(SIMJ_U8 in_data) {

	errno_t text_open_error;
	errno_t bin_open_error;
	BOOL bin_write_status = false;
	DWORD bytes_to_write;
	DWORD text_bytes_written = 0;
	DWORD bin_bytes_written = 0;
	SIMJ_U8 loc_data = 0;

	loc_data = in_data;

	if (!files_open) {
		text_open_error = fopen_s(&text_file, "console_capture.txt", "w+t");
		bin_open_error = fopen_s(&bin_file, "console_capture.dat", "w+b");
		if (text_open_error != 0) {
			char myerror[200] = { 0 };
			strerror_s(&myerror[0], sizeof(myerror), text_open_error);
			printf(" *** ERROR ***  Could not open 'console_capture.txt', error = %d - %s\n", text_open_error, &myerror[0]);
		}
		if (bin_open_error != 0) {
			char myerror[200] = { 0 };
			strerror_s(&myerror[0], sizeof(myerror), bin_open_error);
			printf(" *** ERROR ***  Could not open 'console_capture.dat', error = %d - %s\n", bin_open_error, &myerror[0]);
		}
		if (text_open_error == 0 && bin_open_error == 0) {
			files_open = true;
		}
	}
	if (files_open) {
		bytes_to_write = 1;
		fprintf(text_file, " 0x%02X, ", in_data);
		item_count++;
		if (item_count >= 16) {
			fprintf(text_file, "\n");
			item_count = 0;
		}
		//bytes_to_write = 1;
		fwrite(&loc_data, sizeof(loc_data), 1, bin_file);
		//bin_write_status = WriteFile(bin_file, &loc_data, bytes_to_write,
		//	&bin_bytes_written, NULL);
		//if (bin_bytes_written != 1) {
		//	printf(" *** ERROR *** Didnt write to console_capture.dat - %s\n",GetLastError());
		//}
	}
}

void device_common_capture_console_close() {
	if (files_open) {
		fclose(text_file);
		fclose(bin_file);
	}
}