
#include <windows.h>
#include <stdio.h>

int device_common_serial_open(char* com_port, HANDLE* com_handle ) {

    HANDLE hCom;
    LPCSTR pcCommPort;


    pcCommPort = com_port;

    // -------- Open a handle to the specified com port.
    hCom = CreateFileA(pcCommPort,
        GENERIC_READ | GENERIC_WRITE,
        0,      //  must be opened with exclusive-access
        NULL,   //  default security attributes
        OPEN_EXISTING, //  must use OPEN_EXISTING
        0,      //  not overlapped I/O
        NULL); //  hTemplate must be NULL for comm devices

    *com_handle = hCom;

    // -------- Handle the error.
    if (hCom == INVALID_HANDLE_VALUE) {
        printf("\n *** ERROR *** CreateFile failed with error %d.\n", GetLastError());
        return (1);
    }
    return(0);

}

