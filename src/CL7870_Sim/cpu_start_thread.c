#include <windows.h>
#include <stdio.h>
#include <process.h>

#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"

uintptr_t   cpu_thread_handle;
DWORD   cpu_thread_id;

volatile static int cpu_thread_stop_request = 0;


DWORD WINAPI cpu_thread_proc(LPVOID lpParam);

// =============================================================================================================
void cpu_start_thread() {


	cpu_thread_stop_request = 0;

	cpu_thread_handle = _beginthreadex(NULL,
		0,
		cpu_thread_proc,
		NULL,
		0,
		&cpu_thread_id);

	//  --  cpu_thread_handle = CreateThread(
	//  --  	NULL,			// default security attributes
	//  --  	0,					// use default stack size  
	//  --  	cpu_thread_proc,	// thread function name
	//  --  	NULL,					// argument to thread function 
	//  --  	0,					// use default creation flags 
	//  --  	&cpu_thread_id);		// returns the thread identifier 


	// Check the return value for success.
	// If CreateThread fails, terminate execution. 
	// This will automatically clean up threads and memory. 

	if (cpu_thread_handle == 0)
	{
		printf("\n *** ERROR *** Trouble creating cpu thread.\n");
	}
	else {
		printf(" CPU thread created.  ID = %d\n", cpu_thread_id);
	}

}


// =============================================================================================================
void cpu_stop_thread() {

	int last_value = 0;

	// --------request cpu thread to terminate
	cpu_thread_stop_request = 1;
	last_value = 1;

	gbl_fp_runlight = false;
	gbl_fp_single_step = false;

	// -------- wait a little bit for the clock to exit.
	WaitOnAddress(&cpu_thread_stop_request, &last_value, sizeof(cpu_thread_stop_request), 1000);

	// -------- it didn't stop -- force it to terminate 
	if ( cpu_thread_stop_request != 0) {
		TerminateThread((HANDLE)cpu_thread_handle, 0); // Dangerous source of errors!
		CloseHandle((HANDLE)cpu_thread_handle);
		printf("\n *** ERROR *** Cpu thread didnt respond normally.  It was forcefully terminated.\n");
	}
	else {
		printf(" Cpu thread stopped.\n");
	}

}


// =============================================================================================================
DWORD WINAPI cpu_thread_proc(LPVOID lpParam) {

	bool other = false;
	bool status = false;

	// -------- lower the priority of this thread.
	status = SetThreadPriority((HANDLE)cpu_thread_handle, THREAD_PRIORITY_BELOW_NORMAL);
	if (!status) {
		printf(" *** ERROR *** Failed to lower priority of cpu thread.  Status 0x%08x\n", GetLastError());
	}

	// --------run the cpu until requested to stop.
	while (cpu_thread_stop_request == 0) {

		// -------- if running or single stepping call the cpu to run.
		if (gbl_fp_runlight || gbl_fp_single_step) {
			cpu_classic_7860();
		}

		// --------wait for single step to be triggered.
		else {
			WaitOnAddress(&gbl_fp_single_step, &other, sizeof(gbl_fp_single_step), 10);
		}
	}

	cpu_thread_stop_request = 0;

	//  --  ExitThread(0);
	_endthreadex(0);

	return 0;

}
