#include "simj_base.h"

#include <stdio.h>
#include <stdbool.h>

// -------- set com parameters -- for now 19200,8,N,1 - hardware output hand shaking.
int device_common_serial_set_params( HANDLE hCom, DWORD *last_error, bool USE_HDWR_OUTPUT_HANDSHAKE ) {

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
    // dcb.BaudRate = CBR_9600;     //  baud rate
    dcb.BaudRate = CBR_19200;       //  baud rate
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
        *last_error =  GetLastError();
        return (3);
    }

    // -------- debug print out the current settings
    device_common_serial_print_settings(dcb);

    *last_error = 0;
    return (0);

}
