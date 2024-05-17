// Com_port_test.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <windows.h>
//#include <tchar.h>
#include <stdio.h>
#include <stdbool.h>

// --------------------------------------------------------------------------------
void PrintCommState(DCB dcb)
{
    //  Print some of the DCB structure values
    printf("\nBaudRate = %d, ByteSize = %d, Parity = %d, StopBits = %d\n",
        dcb.BaudRate,
        dcb.ByteSize,
        dcb.Parity,
        dcb.StopBits);

    // --------this must be true -- windows only supports binery
    printf("  fBinary = %d\n",    dcb.fBinary );

    // --------Set DTR to always be on.(input hardware handshaking)
    printf("  fRtsControl = %d\n", dcb.fRtsControl);
    printf("  fDtrControl = %d\n", dcb.fDtrControl);

    // --------it sounds like this is input
    printf("  fDsrSensitivity = %d\n", dcb.fDsrSensitivity);

    // --------turn off any input xon/xoff handshaking
    printf("  fInX = %d\n", dcb.fInX);

    // --------Turn off any output Xon/Xoff handshaking
    printf("  fOutX = %d\n", dcb.fOutX);

    // --------hardware output handshaking
    printf("  fOutxCtsFlow = %d\n", dcb.fOutxCtsFlow);
    printf("  fOutxDsrFlow = %d\n", dcb.fOutxDsrFlow);


}


int device_common_serial_set_params() {

    DCB dcb;
    HANDLE hCom;
    BOOL fSuccess;
    LPCSTR pcCommPort = "COM1"; //  Most systems have a COM1 port //TODO: pass as a parameter


    // -------- Open a handle to the specified com port.
    hCom = CreateFileA(pcCommPort,
        GENERIC_READ | GENERIC_WRITE,
        0,      //  must be opened with exclusive-access
        NULL,   //  default security attributes
        OPEN_EXISTING, //  must use OPEN_EXISTING
        0,      //  not overlapped I/O
        NULL); //  hTemplate must be NULL for comm devices

    // -------- Handle the error.
    if (hCom == INVALID_HANDLE_VALUE) {
        printf("\n *** ERROR *** CreateFile failed with error %d.\n", GetLastError());
        return (1);
    }

    // -------- Initialize the DCB structure.
    SecureZeroMemory(&dcb, sizeof(DCB));
    dcb.DCBlength = sizeof(DCB);

    // -------- Build on the current configuration by first retrieving all current
    // -------- settings.
    fSuccess = GetCommState(hCom, &dcb);

    // -------- Handle the error.
    if (!fSuccess) {
        printf("\n *** ERROR *** GetCommState failed with error %d.\n", GetLastError());
        return (2);
    }

    PrintCommState(dcb);       //  Output to console

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

    PrintCommState(dcb);       //  Output to console

    CloseHandle(hCom);

    //  printf("Serial port %s successfully reconfigured.\n", pcCommPort);
    return (0);

}

// --------------------------------------------------------------------------------
int main(int argc, char* argv[])
{

    int stat;

    stat = device_common_serial_set_params();

    printf("\n stat %d\n", stat);

    return (0);
}