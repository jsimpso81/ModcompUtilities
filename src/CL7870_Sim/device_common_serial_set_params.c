#include <windows.h>
#include <stdio.h>
#include <stdbool.h>

int device_common_serial_set_params( HANDLE hCom ) {

    DCB dcb;
    BOOL fSuccess;
    LPCSTR pcCommPort = "COM1"; //  Most systems have a COM1 port //TODO: pass as a parameter

    // -------- Initialize the DCB structure.
    SecureZeroMemory(&dcb, sizeof(DCB));
    dcb.DCBlength = sizeof(DCB);

    // -------- Build on the current configuration by first retrieving all current
    // -------- settings.
    fSuccess = GetCommState(hCom, &dcb);

    // -------- Handle the error.
    if (!fSuccess) {
        printf("\n *** ERROR *** GetCommState failed with error 0x%08x.\n", GetLastError());
        return (2);
    }

    // -------- debug print out the current settings
    serial_common_serial_print_settings(dcb);

    // -------- Fill in some DCB values and set the com state: 
    // -------- 9600 bps, 8 data bits, no parity, and 1 stop bit.
    dcb.BaudRate = CBR_9600;     //  baud rate
    dcb.ByteSize = 8;             //  data size, xmit and rcv
    dcb.Parity = NOPARITY;      //  parity bit
    dcb.StopBits = ONESTOPBIT;    //  stop bit

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
    dcb.fOutxCtsFlow = false;
    dcb.fOutxDsrFlow = false;

    // --------update the settings.
    fSuccess = SetCommState(hCom, &dcb);

    // -------- Handle the error.
    if (!fSuccess) {
        printf("\n *** ERROR *** SetCommState failed with error %d.\n", GetLastError());
        return (3);
    }

    // -------- Get the comm config again.
    fSuccess = GetCommState(hCom, &dcb);

    // --------- Handle the error.
    if (!fSuccess) {
        printf("\n *** ERROR *** GetCommState failed with error %d.\n", GetLastError());
        return (2);
    }

    // PrintCommState(dcb);       //  Output to console

    CloseHandle(hCom);

    //  printf("Serial port %s successfully reconfigured.\n", pcCommPort);
    return (0);

}
