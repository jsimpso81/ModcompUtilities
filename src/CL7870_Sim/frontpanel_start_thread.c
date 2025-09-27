// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			XXXX.c
//
//	Description:	Routines to xxxxxxx.
//
//	Externally accessible routines:
//					XXXXXXX
// 
// Internal only routines:
//					XXXXXXX
//
// Notes:
//		Format of UDP packet sent to front panel (16 bit words):
//			word	bit		data
//			0		----	16 bit data value
//			1		----	16 bit address value
//			2		----	16 bit switch value
//			3		0x8000	Cond Code N
//			3		0x4000	Cond Code Z
//			3		0x2000	Cond Code O
//			3		0x1000	Cond Code C
//			3		0x0800	Mem	Protect
//			3		0x0400	Privmode
//			3		0x0200	PM	- Pipeline mode
//			3		0x0100	Virtual mode
//			3		0x0080	IO interrupt active (DI,SI)
//			3		0x0040	Task interrupt active (15)
//			3		0x0020	Memory ECC error
//			3		0x001f	EMA - Extended Memory address (5 bits for 7870, 4 bits for 7830)
//			4
//			5		
// 
// ================================================================================================
//	Revision history:
//		6/28/2024	JAS		Added new header
// ================================================================================================

#include "simj_base.h"

#include <stdio.h>
#include <stdbool.h>
#include <process.h>

uintptr_t   frontpanel_thread_handle;
DWORD   frontpanel_thread_id;

volatile static int frontpanel_thread_stop_request = 0;

HANDLE frontpanel_timer_handle = NULL;

DWORD WINAPI frontpanel_thread_proc(LPVOID lpParam);

// --  typedef struct _MYDATA {
// --  	LPCTSTR szText;
// --  	DWORD dwValue;
// --  } MYDATA;

// --  MYDATA unused_data = { 0 };


// =============================================================================================================
void frontpanel_start_thread() {

	bool status = false;
	// const LARGE_INTEGER due_time = { .QuadPart = -500000LL };	// 50 milliseconds
	// const LONG periodic_time = 50;			// 50 milliseconds
	const LARGE_INTEGER due_time = { .QuadPart = -040000LL };	// 4 milliseconds
	const LONG periodic_time = 4;			// 4 milliseconds

	// -------- initialize the stop request
	frontpanel_thread_stop_request = 0;

	// -------- create timer
	frontpanel_timer_handle =
		// -- CreateWaitableTimerW(
		// -- NULL,
		// -- false,
		// -- NULL);
		CreateWaitableTimerExW(
			NULL,			// security attributes
			NULL,				// timer name
			CREATE_WAITABLE_TIMER_HIGH_RESOLUTION,	// flags
			TIMER_ALL_ACCESS | TIMER_MODIFY_STATE);		// 

	// -------- check and deal with error
	if (frontpanel_timer_handle == NULL) {
		printf(" *** ERROR *** Front Panel Communications CreateWaitableTimerEx failed: 0x%08x\n", GetLastError());
	}
	else {

		// -------- Schedule the wait
		status = SetWaitableTimer(
			frontpanel_timer_handle,	// handle
			&due_time,				// due time 
			periodic_time,			// periodic time
			NULL,			// [in, optional] PTIMERAPCROUTINE    pfnCompletionRoutine,
			NULL,		// [in, optional] LPVOID              lpArgToCompletionRoutine,
			FALSE);					// [in] BOOL                fResume

		// -------- timer scheduled okay.
		if (status) {

			frontpanel_thread_handle = _beginthreadex(NULL,
				0,
				frontpanel_thread_proc,
				NULL,
				0,
				&frontpanel_thread_id);

			//  --  frontpanel_thread_handle = CreateThread(
			//  --  		NULL,			// default security attributes
			//  --  		0,					// use default stack size	
			//  --  		frontpanel_thread_proc,	// thread function name			
			//  --  		NULL,					// argument to thread function 
			//  --  		0,					// use default creation flags 
			//  --  		&frontpanel_thread_id);		// returns the thread identifier 


				// Check the return value for success.
				// If C	reateThread fails, terminate execution. 
				// This will automatically clean up threads and memory. 

			if (frontpanel_thread_handle == 0) {
				printf("\n *** ERROR *** Trouble creating front panel communications thread.  Error: 0x%08x\n", errno);
				frontpanel_stop_thread();
			}
			else {

				printf(" Front Panel communications started.\n");
				printf(" Front Panel communications thread created.  ID = %d\n", frontpanel_thread_id);
			}
		}
		else {
			printf("SetWaitableTimer failed with error %d\n", GetLastError());
			frontpanel_stop_thread();
		}
	}
}


// =============================================================================================================
void frontpanel_stop_thread() {

	int stop_compare = 0;

	// -------- request the thread to exit
	frontpanel_thread_stop_request = 1;
	stop_compare = frontpanel_thread_stop_request;

	// -------- wait a little bit for the clock to exit.
	WaitOnAddress(&frontpanel_thread_stop_request, &stop_compare, sizeof(frontpanel_thread_stop_request), 500);

	// -------- it didn't stop -- force it to terminate 
	if (frontpanel_thread_stop_request != 0) {
		TerminateThread((HANDLE)frontpanel_thread_handle, 0); // Dangerous source of errors!
		CloseHandle((HANDLE)frontpanel_thread_handle);
		printf("\n *** ERROR *** Front Panel Communications thread didnt respond normally.  It was forcefully terminated.\n");
	}
	else {
		printf(" Front Panel communications thread stopped.\n");
	}

}


// =============================================================================================================
DWORD WINAPI frontpanel_thread_proc(LPVOID lpParam) {

	bool status = false;

	// --------local data for sending to front panel
	SIMJ_U16 send_words[45] = { 0 };
#define BYTES_TO_SEND 90
#define MAX_COUNT 25

	// --------local temporary data 
	boolean loc_vir_mode = false;
	boolean loc_cpu_run = false;
	boolean loc_cpu_power = false;
	boolean loc_mem_error = false;
	boolean loc_standby = false;
	boolean loc_standby_power_fail = false;
	PSW loc_psw = { 0 };
	SIMJ_U16 loc_int_active = 0;
	SIMJ_U32 loc_loop_count = 0;
	int j = 0;

#define BRIGHT_INC_HIGH 0x0400
#define BRIGHT_INC 4

// --------- buffer word indexes...
#define BRIGHT_ADDR_START 5
#define BRIGHT_DATA_START (BRIGHT_ADDR_START+8)
#define BRIGHT_SWITCH_START (BRIGHT_DATA_START+8)
#define BRIGHT_MISC3_START (BRIGHT_SWITCH_START+8)
#define BRIGHT_MISC4_START (BRIGHT_MISC3_START+8)

	// -------- raise the priority of this thread.
	status = SetThreadPriority((HANDLE)frontpanel_thread_handle, THREAD_PRIORITY_TIME_CRITICAL);
	if (!status) {
		printf(" *** ERROR *** Failed to raise priority of Front Panel communications thread.  Status 0x%08x\n", GetLastError());
	}

	// --------Open the output UDP socket.
	WSADATA		wsaData;
	u_short		loc_send_port = 57830;
	SOCKET		loc_send_udp_socket = INVALID_SOCKET;
	DWORD		loc_last_error = 9999;
	struct sockaddr_in loc_udp_send_serverAddr = { 0 };
	int			loc_send_addr_len = sizeof(loc_udp_send_serverAddr);

	// -------- 1. Initialize Winsock library
	if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
			fprintf(stderr, " *** ERROR *** front_panel_thread_proc - WSAStartup failed: %d\n", WSAGetLastError());
			loc_last_error = WSAGetLastError();
			printf(" Front Panel communications thread exiting.\n");
			// -------- indicate we are exiting.
			frontpanel_thread_stop_request = 0;
			// --------exit thread.
			//  --  ExitThread(0);
			_endthreadex(0);
			return 0;
	}

	// -------- 2. Create a UDP socket
	loc_send_udp_socket = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
	if (loc_send_udp_socket == INVALID_SOCKET) {
			fprintf(stderr, " *** ERROR *** front_panel_thread_proc - socket failed: %d\n", WSAGetLastError());
			loc_last_error = WSAGetLastError();
			WSACleanup();
			// -------- indicate we are exiting.
			frontpanel_thread_stop_request = 0;
			// --------exit thread.
			//  --  ExitThread(0);
			_endthreadex(0);
			return 0;
	}

	// -------- 3. Set the send buffer size to 4096 (this should be much bigger than the 90 bytes needed each time
	int sndBufferSize = 4096;
	int iResult = setsockopt(loc_send_udp_socket, SOL_SOCKET, SO_SNDBUF, (char*)&sndBufferSize, sizeof(sndBufferSize));
	if (iResult == SOCKET_ERROR) {
		fprintf(stderr, " *** ERROR *** front_panel_thread_proc setsockopt SO_SNDBUF failed: %ld\n", WSAGetLastError());
		closesocket(loc_send_udp_socket);
		WSACleanup();
		_endthreadex(0);
		return 0;
	}

	// --------4. set the send socket to be non-blocking..
	unsigned long nonBlocking = 1;
	int ioctl_return = 0;
	ioctl_return = ioctlsocket(loc_send_udp_socket, FIONBIO, &nonBlocking);
	if (ioctl_return == SOCKET_ERROR) {
		fprintf(stderr, " *** ERROR *** front_panel_thread_proc - socket non block set failed: %d\n", WSAGetLastError());
		loc_last_error = WSAGetLastError();
		closesocket(loc_send_udp_socket);
		WSACleanup();
		// -------- indicate we are exiting.
		frontpanel_thread_stop_request = 0;
		// --------exit thread.
		//  --  ExitThread(0);
		_endthreadex(0);
		return 0;
	}


	// -------- 3. Set up the server address structure
	loc_udp_send_serverAddr.sin_family = AF_INET;
	loc_udp_send_serverAddr.sin_port = htons(loc_send_port); // Destination port
	loc_udp_send_serverAddr.sin_addr.s_addr = inet_addr("127.0.0.1"); //INADDR_BROADCAST;	//    


	// -------- loop until requested to stop
	while (frontpanel_thread_stop_request == 0) {

		WaitForSingleObject(frontpanel_timer_handle, 10);

		// --------increment loop count
		loc_loop_count++;

		// -------- pack the data for sending to the front panel
		// -------- data buffer
		// 
		// -------- word 0 16 bit virt address.
		send_words[0] = cpu_get_program_counter();
		if (send_words[0] & 0x8000) send_words[BRIGHT_ADDR_START + 0] += BRIGHT_INC_HIGH;
		if (send_words[0] & 0x4000) send_words[BRIGHT_ADDR_START + 0] += BRIGHT_INC;
		if (send_words[0] & 0x2000) send_words[BRIGHT_ADDR_START + 1] += BRIGHT_INC_HIGH;
		if (send_words[0] & 0x1000) send_words[BRIGHT_ADDR_START + 1] += BRIGHT_INC;
		if (send_words[0] & 0x0800) send_words[BRIGHT_ADDR_START + 2] += BRIGHT_INC_HIGH;
		if (send_words[0] & 0x0400) send_words[BRIGHT_ADDR_START + 2] += BRIGHT_INC;
		if (send_words[0] & 0x0200) send_words[BRIGHT_ADDR_START + 3] += BRIGHT_INC_HIGH;
		if (send_words[0] & 0x0100) send_words[BRIGHT_ADDR_START + 3] += BRIGHT_INC;
		if (send_words[0] & 0x0080) send_words[BRIGHT_ADDR_START + 4] += BRIGHT_INC_HIGH;
		if (send_words[0] & 0x0040) send_words[BRIGHT_ADDR_START + 4] += BRIGHT_INC;
		if (send_words[0] & 0x0020) send_words[BRIGHT_ADDR_START + 5] += BRIGHT_INC_HIGH;
		if (send_words[0] & 0x0010) send_words[BRIGHT_ADDR_START + 5] += BRIGHT_INC;
		if (send_words[0] & 0x0008) send_words[BRIGHT_ADDR_START + 6] += BRIGHT_INC_HIGH;
		if (send_words[0] & 0x0004) send_words[BRIGHT_ADDR_START + 6] += BRIGHT_INC;
		if (send_words[0] & 0x0002) send_words[BRIGHT_ADDR_START + 7] += BRIGHT_INC_HIGH;
		if (send_words[0] & 0x0001) send_words[BRIGHT_ADDR_START + 7] += BRIGHT_INC;


		// -------- word 1 16 bit data
		// TODO: Send 16 bit data value...
		send_words[1] = cpu_read_internal_register(0x0019);	// imap value.
		if (send_words[1] & 0x8000) send_words[BRIGHT_DATA_START + 0] += BRIGHT_INC_HIGH;
		if (send_words[1] & 0x4000) send_words[BRIGHT_DATA_START + 0] += BRIGHT_INC;
		if (send_words[1] & 0x2000) send_words[BRIGHT_DATA_START + 1] += BRIGHT_INC_HIGH;
		if (send_words[1] & 0x1000) send_words[BRIGHT_DATA_START + 1] += BRIGHT_INC;
		if (send_words[1] & 0x0800) send_words[BRIGHT_DATA_START + 2] += BRIGHT_INC_HIGH;
		if (send_words[1] & 0x0400) send_words[BRIGHT_DATA_START + 2] += BRIGHT_INC;
		if (send_words[1] & 0x0200) send_words[BRIGHT_DATA_START + 3] += BRIGHT_INC_HIGH;
		if (send_words[1] & 0x0100) send_words[BRIGHT_DATA_START + 3] += BRIGHT_INC;
		if (send_words[1] & 0x0080) send_words[BRIGHT_DATA_START + 4] += BRIGHT_INC_HIGH;
		if (send_words[1] & 0x0040) send_words[BRIGHT_DATA_START + 4] += BRIGHT_INC;
		if (send_words[1] & 0x0020) send_words[BRIGHT_DATA_START + 5] += BRIGHT_INC_HIGH;
		if (send_words[1] & 0x0010) send_words[BRIGHT_DATA_START + 5] += BRIGHT_INC;
		if (send_words[1] & 0x0008) send_words[BRIGHT_DATA_START + 6] += BRIGHT_INC_HIGH;
		if (send_words[1] & 0x0004) send_words[BRIGHT_DATA_START + 6] += BRIGHT_INC;
		if (send_words[1] & 0x0002) send_words[BRIGHT_DATA_START + 7] += BRIGHT_INC_HIGH;
		if (send_words[1] & 0x0001) send_words[BRIGHT_DATA_START + 7] += BRIGHT_INC;

		// --------switches don't change much, only update once per send cycle.
		if (loc_loop_count >= MAX_COUNT) {
			// -------- word 2 16 bit switches
			send_words[2] = gbl_fp_switches;
			// TODO: This may not be worth doing.  Switches don't change that fast...
			if (send_words[2] & 0x8000) send_words[BRIGHT_SWITCH_START + 0] |= 0x6400; //  += BRIGHT_INC_HIGH;
			if (send_words[2] & 0x4000) send_words[BRIGHT_SWITCH_START + 0] |= 100; //  += BRIGHT_INC;
			if (send_words[2] & 0x2000) send_words[BRIGHT_SWITCH_START + 1] |= 0x6400; //  += BRIGHT_INC_HIGH;
			if (send_words[2] & 0x1000) send_words[BRIGHT_SWITCH_START + 1] |= 100; //  += BRIGHT_INC;
			if (send_words[2] & 0x0800) send_words[BRIGHT_SWITCH_START + 2] |= 0x6400; //  += BRIGHT_INC_HIGH;
			if (send_words[2] & 0x0400) send_words[BRIGHT_SWITCH_START + 2] |= 100; //  += BRIGHT_INC;
			if (send_words[2] & 0x0200) send_words[BRIGHT_SWITCH_START + 3] |= 0x6400; //  += BRIGHT_INC_HIGH;
			if (send_words[2] & 0x0100) send_words[BRIGHT_SWITCH_START + 3] |= 100; //  += BRIGHT_INC;
			if (send_words[2] & 0x0080) send_words[BRIGHT_SWITCH_START + 4] |= 0x6400; //  += BRIGHT_INC_HIGH;
			if (send_words[2] & 0x0040) send_words[BRIGHT_SWITCH_START + 4] |= 100; //  += BRIGHT_INC;
			if (send_words[2] & 0x0020) send_words[BRIGHT_SWITCH_START + 5] |= 0x6400; //  += BRIGHT_INC_HIGH;
			if (send_words[2] & 0x0010) send_words[BRIGHT_SWITCH_START + 5] |= 100; //  += BRIGHT_INC;
			if (send_words[2] & 0x0008) send_words[BRIGHT_SWITCH_START + 6] |= 0x6400; //  += BRIGHT_INC_HIGH;
			if (send_words[2] & 0x0004) send_words[BRIGHT_SWITCH_START + 6] |= 100; //  += BRIGHT_INC;
			if (send_words[2] & 0x0002) send_words[BRIGHT_SWITCH_START + 7] |= 0x6400; //  += BRIGHT_INC_HIGH;
			if (send_words[2] & 0x0001) send_words[BRIGHT_SWITCH_START + 7] |= 100; //  += BRIGHT_INC;
		}
		// -------- word 3	bit values
		send_words[3] = 0;		// init bits.
		// -------- 8000 CC N
		loc_psw = cpu_get_current_PSW();
		if (loc_psw.sep.cc_n) {
			send_words[3] |= 0x8000;
			send_words[BRIGHT_MISC3_START + 0] += BRIGHT_INC_HIGH;
		}

		// -------- 4000 CC Z
		if (loc_psw.sep.cc_z) {
			send_words[3] |= 0x4000;
			send_words[BRIGHT_MISC3_START + 0] += BRIGHT_INC;
		}

		// -------- 2000 CC O
		if (loc_psw.sep.cc_o) {
			send_words[3] |= 0x2000;
			send_words[BRIGHT_MISC3_START + 1] += BRIGHT_INC_HIGH;
		}

		// -------- 1000 CC C
		if (loc_psw.sep.cc_c) {
			send_words[3] |= 0x1000;
			send_words[BRIGHT_MISC3_START + 1] += BRIGHT_INC;
		}

		// -------- 0800 Mem Protect
		//			send_words[BRIGHT_MISC3_START + 2] += BRIGHT_INC_HIGH;

		// -------- 0400 Priv mode
		if (loc_psw.sep.prv) {
			send_words[3] |= 0x0400;
			send_words[BRIGHT_MISC3_START + 2] += BRIGHT_INC;
		}

		// -------- 0200 PM --------pipeline mode
		if (cpu_get_pipeline_mode()) {
			send_words[3] |= 0x0200;
			send_words[BRIGHT_MISC3_START + 3] += BRIGHT_INC_HIGH;
		}

		// -------- 0100 Virt mode
		if (cpu_get_virtual_mode()) {
			send_words[3] |= 0x0100;
			send_words[BRIGHT_MISC3_START + 3] += BRIGHT_INC;
		}

		// -------- 0080 IO interrupt active (DI, SI)
		cpu_get_active_interrupt(&loc_int_active);
		// -------- if either SI or DI are on set light.
		if (loc_int_active & 0x000c) {
			send_words[3] |= 0x0080;
			send_words[BRIGHT_MISC3_START + 4] += BRIGHT_INC_HIGH;
		}

		// -------- 0040 Task interrupt active (15)
		if (loc_int_active & 0x0001) {
			send_words[3] |= 0x0040;
			send_words[BRIGHT_MISC3_START + 4] += BRIGHT_INC;
		}

		// --------		001F EMA (5 bits for 7870, 4 bits for 7860
		// TODO: Base the address on the switch value....???? Look in manual to see if any clarity..
		SIMJ_U32 tmp3 = ((cpu_get_last_abs_addr() >> 16) & 0x0000001f);
		SIMJ_U16 tmp4 = tmp3;		// implicit 32 to 16 bit conversion.
		send_words[3] |= tmp4;
		// --debug-- if ( tmp4 != 0 )
		// --debug-- 	fprintf(stderr, " EMA 0x%04x \n", tmp4);
		// -------- Unused...
		//     send_words[BRIGHT_MISC3_START + 5] += BRIGHT_INC_HIGH;
		if (send_words[3] & 0x0010) send_words[BRIGHT_MISC3_START + 5] += BRIGHT_INC;
		if (send_words[3] & 0x0008) send_words[BRIGHT_MISC3_START + 6] += BRIGHT_INC_HIGH;
		if (send_words[3] & 0x0004) send_words[BRIGHT_MISC3_START + 6] += BRIGHT_INC;
		if (send_words[3] & 0x0002) send_words[BRIGHT_MISC3_START + 7] += BRIGHT_INC_HIGH;
		if (send_words[3] & 0x0001) send_words[BRIGHT_MISC3_START + 7] += BRIGHT_INC;

		// -------- word 4	more bit values
		// -------- these don't change fast, just update on udp send cycle.
		if (loc_loop_count >= MAX_COUNT) {
			send_words[4] = 0;
			//		8000 Power
			if (cpu_get_power_on()) {
				send_words[4] |= 0x8000;
				send_words[BRIGHT_MISC4_START + 0] |= 0x6400; // += BRIGHT_INC_HIGH;
			}

			// -------- 4000 Standby
			//		send_words[BRIGHT_MISC4_START + 0] += BRIGHT_INC;

			// -------- 2000 Backup Failure
			//		send_words[BRIGHT_MISC4_START + 1] += BRIGHT_INC_HIGH;

			// -------- 1000 Run
			if (gbl_fp_runlight) {
				send_words[4] |= 0x1000;
				send_words[BRIGHT_MISC4_START + 1] |= 100; //  += BRIGHT_INC;
			}

			// -------- 0fff --future use--
		}

		// -------- Send data to the front panel
		// --------only do this every 100 (4*25) milliseconds....
		if (loc_loop_count >= MAX_COUNT) {
			// --------set loop count back to 0, so it starts at 1 at the beginning of the loop.
			loc_loop_count = 0;

			// -------- send udp packet (without waiting)
			if (sendto(loc_send_udp_socket, (const char*)(&send_words[0]), BYTES_TO_SEND, 0, (struct sockaddr*)&loc_udp_send_serverAddr, loc_send_addr_len) == SOCKET_ERROR) {
				// TODO: for now ignore error...
				//printf(" frontpanel_thread_proc  Failed to send data. Error Code: %d\n", WSAGetLastError());
				//closesocket(loc_send_udp_socket);
				//WSACleanup();
				//return 1;
			}

			// TODO: does sendto copy the buffer when sending async ?? Yes as long as it fits in the buffer...
			// -------- zero brightness counters.
			for (j = 0; j < 8; j++) {
				send_words[BRIGHT_ADDR_START+j] = 0;
				send_words[BRIGHT_DATA_START+j] = 0;
				send_words[BRIGHT_SWITCH_START+j] = 0;
				send_words[BRIGHT_MISC3_START + j] = 0;
				send_words[BRIGHT_MISC4_START + j] = 0;
			}

		}

	}	// -------- processing loop

	// -------- waitable timer clock the clock handle
	if (frontpanel_timer_handle != NULL) {
		CloseHandle(frontpanel_timer_handle);
	}
	
	closesocket(loc_send_udp_socket);
	WSACleanup();

	printf(" Front Panel communications thread exiting.\n");

	// -------- indicate we are exiting.
	frontpanel_thread_stop_request = 0;

	// --------exit thread.
	//  --  ExitThread(0);
	_endthreadex(0);

	return 0;

}
