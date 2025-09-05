// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			device_common_serial_functions.c
//
//	Description:	Routines to xxxxxxx.
//
//  Routines:
//          int device_common_serial_close(HANDLE com_handle, DWORD* last_error)
//          int device_common_serial_open(char* com_port, HANDLE* com_handle, DWORD* last_error )
//          void device_common_serial_print_settings(DCB this_dcb)
//          int device_common_serial_set_params(HANDLE hCom, DWORD* last_error, 
//                  bool USE_HDWR_OUTPUT_HANDSHAKE, SIMJ_U16 baud)
//
//  Internal routines:
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

#include <stdio.h>
#include <stdbool.h>


// -------- open com port 0 = good status
int device_common_serial_open(char* com_port, HANDLE* com_handle, DWORD* last_error ) {

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

// ================================================================================================

// -------- close an open com port, 0 = no error
int device_common_serial_close(HANDLE com_handle, DWORD* last_error) {

    BOOL status;

    status = CloseHandle(com_handle);

    if (!status) {
        *last_error = GetLastError();
        return(1);
    }

    *last_error = 0;
    return(0);
}

// ================================================================================================

void device_common_serial_print_settings(DCB this_dcb) {

    // -------- Print some of the DCB structure values
    fprintf(stderr, "BaudRate = %d, ByteSize = %d, Parity = %d, StopBits = %d ",
        this_dcb.BaudRate,
        this_dcb.ByteSize,
        this_dcb.Parity,
        this_dcb.StopBits);

    // --------this must be true -- windows only supports binery
    fprintf(stderr, "  fBinary = %d\n", this_dcb.fBinary);

    // --------Set DTR to always be on.(input hardware handshaking)
    fprintf(stderr, "  fRtsControl = %d ", this_dcb.fRtsControl);
    fprintf(stderr, "  fDtrControl = %d ", this_dcb.fDtrControl);

    // --------it sounds like this is input
    fprintf(stderr, "  fDsrSensitivity = %d\n", this_dcb.fDsrSensitivity);

    // --------turn off any input xon/xoff handshaking
    fprintf(stderr, "  fInX = %d ", this_dcb.fInX);

    // --------Turn off any output Xon/Xoff handshaking
    fprintf(stderr, "  fOutX = %d ", this_dcb.fOutX);

    // --------hardware output handshaking
    fprintf(stderr, "  fOutxCtsFlow = %d ", this_dcb.fOutxCtsFlow);
    fprintf(stderr, "  fOutxDsrFlow = %d\n", this_dcb.fOutxDsrFlow);

}

// ================================================================================================

// -------- set com parameters -- for now 19200,8,N,1 - hardware output hand shaking.
int device_common_serial_set_params(HANDLE hCom, DWORD* last_error, bool USE_HDWR_OUTPUT_HANDSHAKE, SIMJ_U16 baud) {

    DCB dcb = { 0 };
    BOOL fSuccess;

    // -------- Initialize the DCB structure.
    SecureZeroMemory(&dcb, sizeof(DCB));
    dcb.DCBlength = sizeof(DCB);

    // -------- Build on the current configuration by first retrieving all current
    // -------- settings.
    fSuccess = GetCommState(hCom, &dcb);

    // -------- Handle the error.
    if (!fSuccess) {
        *last_error = GetLastError();
        return (1);
    }

    // -------- debug print out the current settings
    device_common_serial_print_settings(dcb);

    // -------- Fill in some DCB values and set the com state: 
    // -------- 9600 bps, 8 data bits, no parity, and 1 stop bit.
    dcb.BaudRate = CBR_9600;        //  baud rate -- set default.
    if (baud == 300) {
        dcb.BaudRate = CBR_300;     //  baud rate
    }
    else if (baud == 1200) {
        dcb.BaudRate = CBR_1200;    //  baud rate
    }
    else if (baud == 2400) {
        dcb.BaudRate = CBR_2400;    //  baud rate
    }
    else if (baud == 4800) {
        dcb.BaudRate = CBR_4800;    //  baud rate
    }
    else if (baud == 9600) {
        dcb.BaudRate = CBR_9600;    //  baud rate
    }
    else if (baud == 19200) {
        dcb.BaudRate = CBR_19200;   //  baud rate
    }
    dcb.ByteSize = 8;               //  data size, xmit and rcv
    dcb.Parity = NOPARITY;          //  parity bit
    dcb.StopBits = ONESTOPBIT;      //  stop bit

    // --------this must be true -- windows only supports binery
    dcb.fBinary = true;

    // --------Set DTR to always be on.(input hardware handshaking)
    dcb.fRtsControl = RTS_CONTROL_ENABLE;
    dcb.fDtrControl = DTR_CONTROL_ENABLE;

    // --------it sounds like this is input
    dcb.fDsrSensitivity = false;

    // --------turn off any input xon/xoff handshaking
    dcb.fInX = false;

    // --------Turn off any output Xon/Xoff handshaking
    dcb.fOutX = false;

    // --------hardware output handshaking
    dcb.fOutxCtsFlow = USE_HDWR_OUTPUT_HANDSHAKE;
    dcb.fOutxDsrFlow = false;

    // --------update the settings.
    fSuccess = SetCommState(hCom, &dcb);

    // -------- Handle the error.
    if (!fSuccess) {
        *last_error = GetLastError();
        return (2);
    }

    // -------- Get the comm config again.
    fSuccess = GetCommState(hCom, &dcb);

    // --------- Handle the error.
    if (!fSuccess) {
        *last_error = GetLastError();
        return (3);
    }

    // -------- debug print out the current settings
    device_common_serial_print_settings(dcb);

    *last_error = 0;
    return (0);

}
