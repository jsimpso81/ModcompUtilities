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

int isDataAvailable(SOCKET sock) {

    fd_set readfds;
    struct timeval timeout;

    // Initialize the set of sockets to monitor
    FD_ZERO(&readfds);
    FD_SET(sock, &readfds);

    // Set timeout to 0 for non-blocking check
    timeout.tv_sec = 0;
    timeout.tv_usec = 0;

    // Check if the socket is ready for reading
    int result = select(0, &readfds, NULL, NULL, &timeout);

    if (result > 0 && FD_ISSET(sock, &readfds)) {
        return 1; // Data is available
    }
    else if (result == 0) {
        return 0; // No data available
    }
    else {
        perror("select failed");
        return -1; // Error occurred
    }
}


// -------- read data on a raw tcp socket port.  This function does NOT wait., 0 = no error
int device_common_raw_socket_read(SOCKET tcp_socket, DWORD desired_read_bytes, 
						SIMJ_U8* loc_read_data, DWORD* actual_read_bytes, DWORD* last_error) {

	BOOL read_status = false;
	int bytesReceived = 0;
	int buffer_size = 0;
    int data_avail = 0;

    if ((data_avail = isDataAvailable(tcp_socket)) == 1) {
        // read_status = ReadFile(tcp_socket, &loc_read_data,
        // 	desired_read_bytes, &actual_read_bytes, NULL);

        buffer_size = desired_read_bytes;
        bytesReceived = recv(tcp_socket, loc_read_data, buffer_size, 0);

        // -------- check for error or socket closure.
        if (bytesReceived == 0 || (bytesReceived == SOCKET_ERROR)) {
            *last_error = WSAGetLastError();
            *actual_read_bytes = 0;
            return 1;
        }
    }
    // --------error
    else if (data_avail == -1) {
        *last_error = WSAGetLastError();
        *actual_read_bytes = 0;
        return 1;
    }
    // --------no data avail yet...
    else {
        bytesReceived = 0;
    }

	*actual_read_bytes = bytesReceived;
	*last_error = 0;
	return 0;
}
