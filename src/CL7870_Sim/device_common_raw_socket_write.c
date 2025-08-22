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

// -------- write data on a raw tcp socket port.  This function waits., 0 = no error
int device_common_raw_socket_write(SOCKET tcp_socket, DWORD desired_write_bytes,
	SIMJ_U8* loc_write_data, DWORD* actual_written_bytes, DWORD* last_error) {

	// BOOL read_status = false;
	int bytesWritten = 0;
	int buffer_size = 0;

	buffer_size = desired_write_bytes;
	bytesWritten = send(tcp_socket, loc_write_data, desired_write_bytes, 0);

	// -------- check for error or socket closure.
	if (bytesWritten == 0 || ( bytesWritten == SOCKET_ERROR) ) {
		*last_error = WSAGetLastError();
		*actual_written_bytes = 0;
		return 1;
	}

	*actual_written_bytes = bytesWritten;
	*last_error = 0;
	return 0;
}
