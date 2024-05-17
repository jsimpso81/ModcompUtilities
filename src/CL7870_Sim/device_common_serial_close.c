#include <windows.h>
#include <stdio.h>


int device_common_close(HANDLE com_handle) {

	BOOL status;
	DWORD last_error;

	status = CloseHandle(com_handle);

	if (!status) {
		last_error = GetLastError();
		printf("\n *** ERROR *** Trouble closinig serial port, status = 0x%08x\n", last_error);
		return(1);
	}

	return(0);
}
