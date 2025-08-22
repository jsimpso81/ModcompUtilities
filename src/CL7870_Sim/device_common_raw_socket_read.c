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

// -------- read data on a raw tcp socket port.  This function waits., 0 = no error
int device_common_raw_socket_read(SOCKET tcp_socket, DWORD desired_read_bytes, 
						SIMJ_U8* loc_read_data, DWORD* actual_read_bytes, DWORD* last_error) {

	BOOL read_status = false;
	int bytesReceived = 0;
	int buffer_size = 0;

	// read_status = ReadFile(tcp_socket, &loc_read_data,
	// 	desired_read_bytes, &actual_read_bytes, NULL);

	buffer_size = desired_read_bytes;
	bytesReceived = recv( tcp_socket, loc_read_data, buffer_size, 0);

	// -------- check for error or socket closure.
	if ( bytesReceived == 0 || ( bytesReceived == SOCKET_ERROR ) ) {
		*last_error = WSAGetLastError();
		*actual_read_bytes = 0;
		return 1;
	}

	*actual_read_bytes = bytesReceived;
	*last_error = 0;
	return 0;
}
