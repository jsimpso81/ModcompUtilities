
#include <windows.h>

// -------- open com port 0 = good status
int device_common_serial_open(char* com_port, HANDLE* com_handle, DWORD* last_error ) {

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
        *last_error = GetLastError();
        return (1);
    }
    *last_error = 0;
    return(0);

}
