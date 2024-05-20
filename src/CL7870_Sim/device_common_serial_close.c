#include <windows.h>

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
