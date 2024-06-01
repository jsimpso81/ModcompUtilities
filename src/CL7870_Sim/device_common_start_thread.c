//  util_start_thread
#include <windows.h>
#include <process.h>

#include "modcomp_sim_types.h"


uintptr_t device_common_start_thread(void* data_buffer,  DEVICE_WORKER_THREAD thread_proc, unsigned* thread_id ) {

	uintptr_t device_thread = 0;

	device_thread = _beginthreadex(NULL,
				0,
				thread_proc,
				(void*)data_buffer,
				0,
				thread_id);

	//  --  device_thread = CreateThread(
	//  --  	NULL,			// default security attributes
	//  --  	0,					// use default stack size  
	//  --  	thread_proc,		// thread function name
	//  --  	(LPVOID)data_buffer,	// argument to thread function 
	//  --  	0,					// use default creation flags 
	//  --  	thread_id);	// returns the thread identifier 

	return device_thread;
}