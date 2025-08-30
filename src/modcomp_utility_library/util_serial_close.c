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

#include "modcomp_utility_library.h"

// -------- close an open com port, 0 = no error
int util_serial_close(HANDLE com_handle, DWORD* last_error) {

	BOOL status;

	status = CloseHandle(com_handle);

	if (!status) {
		*last_error = GetLastError();
		return(1);
	}

	*last_error = 0;
	return(0);
}