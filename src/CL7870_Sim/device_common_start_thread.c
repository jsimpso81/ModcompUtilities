//  util_start_thread
#include <windows.h>

#include "modcomp_sim_types.h"


HANDLE device_common_start_thread(LPVOID data_buffer,  DEVICE_WORKER_THREAD thread_proc, LPDWORD thread_id ) {

	HANDLE device_thread = NULL;

	device_thread = CreateThread(
		NULL,			// default security attributes
		0,					// use default stack size  
		thread_proc,		// thread function name
		(LPVOID)data_buffer,	// argument to thread function 
		0,					// use default creation flags 
		thread_id);	// returns the thread identifier 

	return device_thread;
}