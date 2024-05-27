#include <windows.h>
#include <stdio.h>

#include "modcomp_sim_external_globals.h"


void device_common_stop_all() {

	int j;

	for (j = 0; j < 64; j++) {

		// -------- if we have a device here, set terminate request
		if (iop_device_buffer[j] != NULL) {
			iop_thread_stop_request[j] = 1;
			iop_thread_stop_request2[j] = 1;
		}
	}
	// -------- wait 2 seconds for devices to respond.
	Sleep(2000);

	// -------- if the thread is still there, terminate it.
	for (j = 0; j < 64; j++) {
		if (iop_thread_stop_request[j] != 0) {
			TerminateThread(iop_device_thread_handle[j], 0); // Dangerous source of errors!
			CloseHandle(iop_device_thread_handle[j]);
			printf("\n *** ERROR *** Device thread didnt respond normally.  It was forcefully terminated.\n");
		}
		if (iop_thread_stop_request2[j] != 0 && iop_device_thread_handle2[j] != 0) {
			TerminateThread(iop_device_thread_handle2[j], 0); // Dangerous source of errors!
			CloseHandle(iop_device_thread_handle2[j]);
			printf("\n *** ERROR *** Device communications thread didnt respond normally.  It was forcefully terminated.\n");
		}
	}

}