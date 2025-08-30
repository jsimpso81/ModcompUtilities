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

// -------- open com port 0 = good status
//      in - com_port char*
//      out - handle*
//          - DWORD* last_error
//      return - 0 - no error, others error
//
int util_serial_open(char* com_port, HANDLE* com_handle, DWORD* last_error ) {

    HANDLE hCom;
    LPCSTR pcCommPort;

    char loc_com_port[1024];

    strcpy_s(loc_com_port, sizeof(loc_com_port), "\\\\.\\");
    strcat_s(loc_com_port, sizeof(loc_com_port), com_port);

    pcCommPort = loc_com_port;

    // -------- Open a handle to the specified com port.
    hCom = CreateFileA(pcCommPort,
        GENERIC_READ | GENERIC_WRITE,
        0,                                 //  must be opened with exclusive-access
        NULL,                       //  default security attributes
        OPEN_EXISTING,             //  must use OPEN_EXISTING
        0, // FILE_FLAG_OVERLAPPED,       //  overlapped I/O
        NULL);                           //  hTemplate must be NULL for comm devices

    *com_handle = hCom;

    // -------- Handle the error.
    if (hCom == INVALID_HANDLE_VALUE) {
        *last_error = GetLastError();
        return (1);
    }
    *last_error = 0;
    return(0);

}