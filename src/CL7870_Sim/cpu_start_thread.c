#include <windows.h>
#include <stdio.h>

#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"

HANDLE  cpu_thread;
DWORD   cpu_thread_id;
static int cpu_thread_stop = 0;


DWORD WINAPI cpu_thread_proc(LPVOID lpParam);

void cpu_start_thread() {


	cpu_thread_stop = 0;

	cpu_thread = CreateThread(
		NULL,			// default security attributes
		0,					// use default stack size  
		cpu_thread_proc,	// thread function name
		NULL,					// argument to thread function 
		0,					// use default creation flags 
		&cpu_thread_id);		// returns the thread identifier 


	// Check the return value for success.
	// If CreateThread fails, terminate execution. 
	// This will automatically clean up threads and memory. 

	if (cpu_thread == NULL)
	{
		printf("\n *** ERROR *** Trouble creating cpu thread.\n");
	}
	else {
		printf("\n CPU thread created.  ID = %d\n", cpu_thread_id);
	}

}


void cpu_stop_thread() {

	cpu_thread_stop = 1;
	gbl_fp_runlight = false;
	gbl_fp_single_step = false;
	Sleep(500);

	// -------- it didn't stop -- force it to terminate 
	if ( cpu_thread_stop != 0) {
		TerminateThread(cpu_thread, 0); // Dangerous source of errors!
		CloseHandle(cpu_thread);
		printf("\n *** ERROR *** Cpu thread didnt respond normally.  It was forcefully terminated.\n");
	}
	else {
		printf("\nCpu thread stopped.\n");
	}

}


DWORD WINAPI cpu_thread_proc(LPVOID lpParam) {

	bool other = false;

	while (cpu_thread_stop == 0) {

		if (gbl_fp_runlight || gbl_fp_single_step) {
			cpu_classic_7860();
		}
		else {
			WaitOnAddress(&gbl_fp_single_step, &other, sizeof(gbl_fp_single_step), 10);
		}
	}

	cpu_thread_stop = 0;

	ExitThread(0);

	return 0;

}
