
#include <windows.h>
#include <synchapi.h>
#include <stdio.h>
#include <stdbool.h>
#include <process.h>

#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"


#include <windows.h>
#include <stdio.h>

#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"

uintptr_t   rtclock_thread_handle;
DWORD   rtclock_thread_id;

volatile static int rtclock_thread_stop_request = 0;

HANDLE rtclock_timer_handle = NULL;

DWORD WINAPI rtclock_thread_proc(LPVOID lpParam);

typedef struct _MYDATA {
	LPCTSTR szText;
	DWORD dwValue;
} MYDATA;

MYDATA unused_data = { 0 };


// =============================================================================================================
void rtclock_start_thread() {

	bool status = false;
	const LARGE_INTEGER due_time = { .QuadPart = -50000LL };	// 5 milliseconds
	const LONG periodic_time = 5;			// 5 milliseconds

	// -------- initialize the stop request
	rtclock_thread_stop_request = 0;

	// -------- create timer
	rtclock_timer_handle = 
		// -- CreateWaitableTimerW(
		// -- NULL,
		// -- false,
		// -- NULL);
		CreateWaitableTimerExW(
			NULL,			// security attributes
			NULL,				// timer name
			CREATE_WAITABLE_TIMER_HIGH_RESOLUTION,	// flags
			TIMER_ALL_ACCESS | TIMER_MODIFY_STATE  );		// 

// -------- check and deal with error
	if (rtclock_timer_handle == NULL) {
		printf(" *** ERROR *** Real-time Clock CreateWaitableTimerEx failed: 0x%08x\n", GetLastError());
	}
	else {

		// -------- Schedule the wait
		status = SetWaitableTimer(
			rtclock_timer_handle,		// handle
			&due_time,				// due time 
			periodic_time,			// periodic time
			NULL,			// [in, optional] PTIMERAPCROUTINE    pfnCompletionRoutine,
			NULL,		// [in, optional] LPVOID              lpArgToCompletionRoutine,
			FALSE);					// [in] BOOL                fResume

		// -------- timer scheduled okay.
		if (status) {

			rtclock_thread_handle = _beginthreadex(NULL,
				0,
				rtclock_thread_proc,
				NULL,
				0,
				&rtclock_thread_id);

			//  --  rtclock_thread_handle = CreateThread(
			//  --  		NULL,			// default security attributes
			//  --  		0,					// use default stack size	
			//  --  		rtclock_thread_proc,	// thread function name			
			//  --  		NULL,					// argument to thread function 
			//  --  		0,					// use default creation flags 
			//  --  		&rtclock_thread_id);		// returns the thread identifier 


				// Check the return value for success.
				// If C	reateThread fails, terminate execution. 
				// This will automatically clean up threads and memory. 

			if (rtclock_thread_handle == 0) {
				printf("\n *** ERROR *** Trouble creating real-time clock thread.  Error: 0x%08x\n",errno);
				rtclock_stop_thread();
			}
			else {

				printf(" Real time CPU clock started.\n");
				printf(" Real-time clock thread created.  ID = %d\n", rtclock_thread_id);
			}
		}
		else {
			printf("SetWaitableTimer failed with error %d\n", GetLastError());
			rtclock_stop_thread();
		}
	}
}


// =============================================================================================================
void rtclock_stop_thread() {

	int stop_compare = 0;

	// -------- request the thread to exit
	rtclock_thread_stop_request = 1;
	stop_compare = rtclock_thread_stop_request;

	// -------- wait a little bit for the clock to exit.
	WaitOnAddress(&rtclock_thread_stop_request, &stop_compare, sizeof(rtclock_thread_stop_request), 500);

	// -------- it didn't stop -- force it to terminate 
	if (rtclock_thread_stop_request != 0) {
		TerminateThread((HANDLE)rtclock_thread_handle, 0); // Dangerous source of errors!
		CloseHandle((HANDLE)rtclock_thread_handle);
		printf("\n *** ERROR *** Real-time clock thread didnt respond normally.  It was forcefully terminated.\n");
	}
	else {
		printf(" Real-time clock thread stopped.\n");
	}

}


// =============================================================================================================
DWORD WINAPI rtclock_thread_proc(LPVOID lpParam) {

	bool status = false;

	// -------- raise the priority of this thread.
	status = SetThreadPriority((HANDLE)rtclock_thread_handle, THREAD_PRIORITY_TIME_CRITICAL);
	if (!status) {
		printf(" *** ERROR *** Failed to raise priority of real-time clock thread.  Status 0x%08x\n", GetLastError());
	}


	// -------- loop until requested to stop
	while (rtclock_thread_stop_request == 0) {

		WaitForSingleObject(rtclock_timer_handle, 20);
		cpu_trigger_clock_interrupt();
	}

	// -------- waitable timer clock the clock handle
	if ( rtclock_timer_handle  != NULL) {
		CloseHandle(rtclock_timer_handle);
	}

	printf(" Real-time clock thread exiting.\n");

	// -------- indicate we are exiting.
	rtclock_thread_stop_request = 0;

	// --------exit thread.
	//  --  ExitThread(0);
	_endthreadex(0);

	return 0;

}


