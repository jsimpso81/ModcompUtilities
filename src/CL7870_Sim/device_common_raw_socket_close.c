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

// -------- close an open com port, 0 = no error
int device_common_raw_socket_close(SOCKET tcp_socket, DWORD* last_error) {

	int status;

	// -------- 7. close socket
	status = closesocket(tcp_socket);

	if (status != 0) {
	 	*last_error = WSAGetLastError();
	 	return 1;
	}

	*last_error = 0;
	return 0;
}
