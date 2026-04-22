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
//			4		0x8000	Power on
//			4		0x4000	Standby (7870 only)
//			4		0x2000	Backup Failure (7870 only)
//			4		0x1000	Run
//			4		0x0fff	--future use
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

uintptr_t   frontpanel_writethread_handle;
uintptr_t   frontpanel_readthread_handle;
DWORD   frontpanel_writethread_id;
DWORD   frontpanel_readthread_id;

volatile static int frontpanel_writethread_stop_request = 0;
volatile static int frontpanel_readthread_stop_request = 0;
HANDLE frontpanel_readthread_stop_event = 0;

HANDLE frontpanel_writetimer_handle = NULL;
// --no-- HANDLE frontpanel_readtimer_handle = NULL;

DWORD WINAPI frontpanel_writethread_proc(LPVOID lpParam);
DWORD WINAPI frontpanel_readthread_proc(LPVOID lpParam);

// --  typedef struct _MYDATA {
// --  	LPCTSTR szText;
// --  	DWORD dwValue;
// --  } MYDATA;

// --  MYDATA unused_data = { 0 };
#define RECV_CMD_BUFFER_LEN 1024
SIMJ_U8		recvCmdBuffer[RECV_CMD_BUFFER_LEN] = { 0 };
int			recvCmdBytes = 0;


// =============================================================================================================
void frontpanel_start_thread() {

	bool status = false;

	// const LARGE_INTEGER write_due_time = { .QuadPart = -500000LL };	// 50 milliseconds
	// const LONG write_periodic_time = 50;			// 50 milliseconds
	const LARGE_INTEGER write_due_time = { .QuadPart = -040000LL };	// 4 milliseconds
	const LONG write_periodic_time = 4;			// 4 milliseconds

	// --no-- const LARGE_INTEGER read_due_time = { .QuadPart = -200000LL };	// 20 milliseconds
	// --no-- const LONG read_periodic_time = 20;			// 20 milliseconds

	// -------- initialize the stop request
	frontpanel_writethread_stop_request = 0;
	// --no-- frontpanel_readthread_stop_request = 0;

	// -------- WRITE 
	// -------- create timer
	frontpanel_writetimer_handle =
		CreateWaitableTimerExW(
			NULL,			// security attributes
			NULL,				// timer name
			CREATE_WAITABLE_TIMER_HIGH_RESOLUTION,	// flags
			TIMER_ALL_ACCESS | TIMER_MODIFY_STATE);		// 

	// -------- check and deal with error
	if (frontpanel_writetimer_handle == NULL) {
		printf(" *** ERROR *** Front Panel Write Communications CreateWaitableTimerEx failed: 0x%08x\n", GetLastError());
	}
	else {

		// -------- Schedule the wait
		status = SetWaitableTimer(
			frontpanel_writetimer_handle,	// handle
			&write_due_time,				// due time 
			write_periodic_time,			// periodic time
			NULL,			// [in, optional] PTIMERAPCROUTINE    pfnCompletionRoutine,
			NULL,		// [in, optional] LPVOID              lpArgToCompletionRoutine,
			FALSE);					// [in] BOOL                fResume

		// -------- timer scheduled okay.
		if (status) {

			frontpanel_writethread_handle = _beginthreadex(NULL,
				0,
				frontpanel_writethread_proc,
				NULL,
				0,
				&frontpanel_writethread_id);

				// Check the return value for success.
				// If C	reateThread fails, terminate execution. 
				// This will automatically clean up threads and memory. 

			if (frontpanel_writethread_handle == 0) {
				printf("\n *** ERROR *** Trouble creating front panel write communications thread.  Error: 0x%08x\n", errno);
				frontpanel_stop_thread();
			}
			else {

				printf(" Front Panel write communications started.\n");
				printf(" Front Panel write communications thread created.  ID = %d\n", frontpanel_writethread_id);
			}
		}
		else {
			printf("SetWaitableTimer failed with error %d\n", GetLastError());
			frontpanel_stop_thread();
		}
	}

	// -------- READ
	// --no-- // -------- create timer
	// --no-- frontpanel_readtimer_handle =
	// --no-- 	CreateWaitableTimerExW(
	// --no-- 		NULL,			// security attributes
	// --no-- 		NULL,				// timer name
	// --no-- 		CREATE_WAITABLE_TIMER_HIGH_RESOLUTION,	// flags
	// --no-- 		TIMER_ALL_ACCESS | TIMER_MODIFY_STATE);		// 

	// --no-- // -------- check and deal with error
	// --no-- if (frontpanel_readtimer_handle == NULL) {
	// --no-- 	printf(" *** ERROR *** Front Panel Read Communications CreateWaitableTimerEx failed: 0x%08x\n", GetLastError());
	// --no-- }
	// --no-- else {

	// --no-- 	// -------- Schedule the wait
	// --no-- 	status = SetWaitableTimer(
	// --no-- 		frontpanel_readtimer_handle,	// handle
	// --no-- 		&read_due_time,				// due time 
	// --no-- 		read_periodic_time,			// periodic time
	// --no-- 		NULL,			// [in, optional] PTIMERAPCROUTINE    pfnCompletionRoutine,
	// --no-- 		NULL,		// [in, optional] LPVOID              lpArgToCompletionRoutine,
	// --no-- 		FALSE);					// [in] BOOL                fResume

	// --no-- 	// -------- timer scheduled okay.
	// --no-- 	if (status) {

			frontpanel_readthread_handle = _beginthreadex(NULL,
				0,
				frontpanel_readthread_proc,
				NULL,
				0,
				&frontpanel_readthread_id);

				// Check the return value for success.
				// If C	reateThread fails, terminate execution. 
				// This will automatically clean up threads and memory. 

			if (frontpanel_readthread_handle == 0) {
				printf("\n *** ERROR *** Trouble creating front panel read communications thread.  Error: 0x%08x\n", errno);
				frontpanel_stop_thread();
			}
			else {

				printf(" Front Panel read communications started.\n");
				printf(" Front Panel read communications thread created.  ID = %d\n", frontpanel_readthread_id);
			}
	// --no-- 	}
	// --no-- 	else {
	// --no-- 		printf("SetWaitableTimer failed with error %d\n", GetLastError());
	// --no-- 		frontpanel_stop_thread();
	// --no-- 	}
	// --no-- }


}


// =============================================================================================================
void frontpanel_stop_thread() {

	int stop_writecompare = 0;
	int stop_readcompare = 0;

	// -------- request the thread to exit
	frontpanel_writethread_stop_request = 1;
	// --no-- frontpanel_readthread_stop_request = 1;
	SetEvent(frontpanel_readthread_stop_event);

	stop_writecompare = 1; // frontpanel_writethread_stop_request;
	stop_readcompare = 1; // frontpanel_readthread_stop_request;

	// -------- wait a little bit for the write thread to exit.
	WaitOnAddress(&frontpanel_writethread_stop_request, &stop_writecompare, 
		sizeof(frontpanel_writethread_stop_request), 1000);

	// -------- it didn't stop -- force it to terminate 
	if (frontpanel_writethread_stop_request != 0) {
		TerminateThread((HANDLE)frontpanel_writethread_handle, 0); // Dangerous source of errors!
		CloseHandle((HANDLE)frontpanel_writethread_handle);
		printf("\n *** ERROR *** Front Panel Write Communications thread didnt respond normally.  It was forcefully terminated.\n");
	}
	else {
		printf(" Front Panel write communications thread stopped.\n");
	}

	// -------- wait a little bit for the read thread to exit.
	WaitOnAddress(&frontpanel_readthread_stop_request, &stop_readcompare,
		sizeof(frontpanel_readthread_stop_request), 500);

	// -------- it didn't stop -- force it to terminate 
	if (frontpanel_readthread_stop_request != 0) {
		TerminateThread((HANDLE)frontpanel_readthread_handle, 0); // Dangerous source of errors!
		CloseHandle((HANDLE)frontpanel_readthread_handle);
		printf("\n *** ERROR *** Front Panel Read Communications thread didnt respond normally.  It was forcefully terminated.\n");
	}
	else {
		printf(" Front Panel read communications thread stopped.\n");
	}

}


// =============================================================================================================
void proc_recv_cmd() {

	SIMJ_U16 cmdvalue;
	SIMJ_U16 cmdHi;
	SIMJ_U16 cmdLo;
	SIMJ_U16 datavalue;
	SIMJ_U16 dataHi;
	SIMJ_U16 dataLo;

	if (recvCmdBytes == 4 || recvCmdBytes == 5 ) {
		printf(" --------received %d %d %d %d %d \n", recvCmdBuffer[0], recvCmdBuffer[1], recvCmdBuffer[2], recvCmdBuffer[3], recvCmdBuffer[4]);
		cmdLo = recvCmdBuffer[0];
		cmdHi = recvCmdBuffer[1];
		dataLo = recvCmdBuffer[2];
		dataHi = recvCmdBuffer[3];
		cmdvalue = cmdHi << 8 | (0x00ff & cmdLo);
		datavalue = dataHi << 8 | (0x00ff & dataLo);

		switch (cmdvalue) {
			// --------0: noop
			case 0:
				break;

			// --------1: master clear
			case 1:
				cpu_master_clear();
				break;

			// --------2: fill <device address>
			case 2:
				if (!cpu_get_power_on()) {
					printf(" *** ERROR ***  Cant perform fill.  CPU is not powered on.\n");
				}
				else if (gbl_fp_runlight) {
					printf(" *** ERROR ***  Cant perform fill.  CPU is not halted.\n");
				}
				else if (gbl_fp_single_step) {
					printf(" *** ERROR ***  Cant perform fill.  CPU is being single stepped.\n");
				}
				else {
					// -------- perform the fill command
					cpu_do_fill(datavalue);
				}
				break;

			// --------3: run
			case 3:
				cpu_do_run();
				break;

			// --------4: halt
			case 4:
				gbl_fp_runlight = false;
				break;

			// --------5: set switches value <switch value>
			case 5:
				cpu_set_switches(datavalue);
				printf(" Front panel switches 0x%04x\n", gbl_fp_switches);
				break;

			// --------6: set register display select value <reg display select value>
			// TODO: update register display select
			case 6:
				break;

			// --------set memory mode switch value <value> 
			//		0: virt / inst
			//		1 : virt / oper
			//		2 : act / inst
			//		3 : act / oper
			// TODO: update memory mode switch value
			case 7:
				break;

			// --------8: clear breakpoint halt
			// TODO: clear breakpoint halt.
			case 8:
				break;

			// --------9: console interrupt
			case 9:
				cpu_trigger_console_interrupt();
				break;

			// --------10: enter register <new value>, reg display select = register.
			// TODO: Enter register value
			case 10:
				break;

			// --------11: enter next
			// TODO: Enter next
			case 11:
				break;

			// --------12: enter mem
			// TODO: Enter mem
			case 12:
				break;

			// --------13: enter p/ma
			// TODO: Enter p/ma
			case 13:
				break;

			// --------14: step p/ma
			// TODO: step p/ma
			case 14:
				break;

			// --------15: single step
			case 15:
				cpu_do_step(1);
				break;

			// --------bad command value
			default:
				printf(" ------ bad command value %d \n", cmdvalue);
				break;
		}
	}
	else {
		printf(" ------ number of bytes recived is strange %d \n", recvCmdBytes);
	}

}


// =============================================================================================================
DWORD WINAPI frontpanel_writethread_proc(LPVOID lpParam) {

	bool status = false;

	// --------local data for sending to front panel
	SIMJ_U16 send_words[45] = { 0 };
#define BYTES_TO_SEND 90
#define MAX_COUNT 25

	// --------local temporary data 
	bool loc_vir_mode = false;
	bool loc_cpu_run = false;
	bool loc_cpu_power = false;
	bool loc_mem_error = false;
	bool loc_standby = false;
	bool loc_standby_power_fail = false;
	PSW loc_psw = { 0 };
	SIMJ_U16 loc_int_active = 0;
	SIMJ_U32 loc_loop_count = 0;
	int j = 0;
	bool running = 1;

#define BRIGHT_INC_HIGH 0x0400
#define BRIGHT_INC 4

// --------- buffer word indexes...
#define BRIGHT_ADDR_START 5
#define BRIGHT_DATA_START (BRIGHT_ADDR_START+8)
#define BRIGHT_SWITCH_START (BRIGHT_DATA_START+8)
#define BRIGHT_MISC3_START (BRIGHT_SWITCH_START+8)
#define BRIGHT_MISC4_START (BRIGHT_MISC3_START+8)

	// -------- raise the priority of this thread.
	status = SetThreadPriority((HANDLE)frontpanel_writethread_handle, THREAD_PRIORITY_TIME_CRITICAL);
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
	// IN_ADDR		loc_send_addr_struct;

	// -------- 1. Initialize Winsock library
	if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
			fprintf(stderr, " *** ERROR *** front_panel_writethread_proc - WSAStartup failed: %d\n", WSAGetLastError());
			loc_last_error = WSAGetLastError();
			printf(" Front Panel communications thread exiting.\n");
			// -------- indicate we are exiting.
			frontpanel_writethread_stop_request = 0;
			// --------exit thread.
			//  --  ExitThread(0);
			_endthreadex(0);
			return 0;
	}

	// -------- 2. Create a UDP socket
	loc_send_udp_socket = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
	if (loc_send_udp_socket == INVALID_SOCKET) {
			fprintf(stderr, " *** ERROR *** front_panel_writethread_proc - socket failed: %d\n", WSAGetLastError());
			loc_last_error = WSAGetLastError();
			WSACleanup();
			// -------- indicate we are exiting.
			frontpanel_writethread_stop_request = 0;
			// --------exit thread.
			//  --  ExitThread(0);
			_endthreadex(0);
			return 0;
	}

	// -------- 3. Set the send buffer size to 4096 (this should be much bigger than the 90 bytes needed each time
	int sndBufferSize = 4096;
	int iResult = setsockopt(loc_send_udp_socket, SOL_SOCKET, SO_SNDBUF, (char*)&sndBufferSize, sizeof(sndBufferSize));
	if (iResult == SOCKET_ERROR) {
		fprintf(stderr, " *** ERROR *** front_panel_writethread_proc setsockopt SO_SNDBUF failed: %ld\n", WSAGetLastError());
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
		fprintf(stderr, " *** ERROR *** front_panel_writethread_proc - socket non block set failed: %d\n", WSAGetLastError());
		loc_last_error = WSAGetLastError();
		closesocket(loc_send_udp_socket);
		WSACleanup();
		// -------- indicate we are exiting.
		frontpanel_writethread_stop_request = 0;
		// --------exit thread.
		//  --  ExitThread(0);
		_endthreadex(0);
		return 0;
	}


	// -------- 3. Set up the server address structure
	memset(&loc_udp_send_serverAddr, 0, sizeof(loc_udp_send_serverAddr));
	loc_udp_send_serverAddr.sin_family = AF_INET;						// IPv4
	loc_udp_send_serverAddr.sin_port = htons(loc_send_port);   // Port in network byte order
	inet_pton(AF_INET, "127.0.0.1", &loc_udp_send_serverAddr.sin_addr);

	// --------set running
	running = 1;

	// -------- loop until requested to stop
	while ( running ) {

		WaitForSingleObject(frontpanel_writetimer_handle, 10);

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
			// --------0100 exit request.
			if (frontpanel_writethread_stop_request != 0) {
				send_words[4] |= 0x0100;
				running = 0;
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
				//printf(" frontpanel_writethread_proc  Failed to send data. Error Code: %d\n", WSAGetLastError());
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
	if (frontpanel_writetimer_handle != NULL) {
		CloseHandle(frontpanel_writetimer_handle);
	}
	
	closesocket(loc_send_udp_socket);
	WSACleanup();

	printf(" Front Panel communications thread exiting.\n");

	// -------- indicate we are exiting.
	frontpanel_writethread_stop_request = 0;

	// --------exit thread.
	//  --  ExitThread(0);
	_endthreadex(0);

	return 0;

}


// =============================================================================================================
DWORD WINAPI frontpanel_readthread_proc(LPVOID lpParam) {

	bool status = false;


	// --------local temporary data 
	SIMJ_U32 loc_loop_count = 0;


	// --------- buffer word indexes...

	// -------- raise the priority of this thread.
	status = SetThreadPriority((HANDLE)frontpanel_readthread_handle, THREAD_PRIORITY_TIME_CRITICAL);
	if (!status) {
		printf(" *** ERROR *** Failed to raise priority of Front Panel read communications thread.  Status 0x%08x\n", GetLastError());
	}

	// --------Open the input UDP socket.
	WSADATA		wsaData;
	u_short		loc_recv_port = 57831;
	SOCKET		loc_recv_udp_socket = INVALID_SOCKET;
	DWORD		loc_last_error = 9999;
	struct sockaddr_in loc_udp_recv_serverAddr = { 0 };
	int			loc_send_addr_len = sizeof(loc_udp_recv_serverAddr);
	bool		running = true;

	// -------- 1. Initialize Winsock library
	if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) {
		fprintf(stderr, " *** ERROR *** front_panel_readthread_proc - WSAStartup failed: %d\n", WSAGetLastError());
		loc_last_error = WSAGetLastError();
		printf(" Front Panel read communications thread exiting.\n");
		// -------- indicate we are exiting.
		frontpanel_readthread_stop_request = 0;
		// --------exit thread.
		//  --  ExitThread(0);
		_endthreadex(0);
		return 0;
	}

	// -------- 2. Create a UDP socket
	loc_recv_udp_socket = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
	if (loc_recv_udp_socket == INVALID_SOCKET) {
		fprintf(stderr, " *** ERROR *** front_panel_readthread_proc - socket failed: %d\n", WSAGetLastError());
		loc_last_error = WSAGetLastError();
		WSACleanup();
		// -------- indicate we are exiting.
		frontpanel_readthread_stop_request = 0;
		// --------exit thread.
		//  --  ExitThread(0);
		_endthreadex(0);
		return 0;
	}

	// -------- 2a. bind to a port and address...
	memset(&loc_udp_recv_serverAddr, 0, sizeof(loc_udp_recv_serverAddr));
	loc_udp_recv_serverAddr.sin_family = AF_INET;						// IPv4
	loc_udp_recv_serverAddr.sin_port = htons(loc_recv_port);   // Port in network byte order
	inet_pton(AF_INET, "0.0.0.0", &loc_udp_recv_serverAddr.sin_addr);	// any adress..
	int iBindResult = bind(loc_recv_udp_socket, (struct sockaddr*)&loc_udp_recv_serverAddr, sizeof(loc_udp_recv_serverAddr));
	if (iBindResult == SOCKET_ERROR) {
		fprintf(stderr, " *** ERROR *** front_panel_readthread_proc bind failed: %ld\n", WSAGetLastError());
		closesocket(loc_recv_udp_socket);
		WSACleanup();
		_endthreadex(0);
		return 0;
	}

	// -------- 3. Set the recv buffer size to 4096 (this should be much bigger than the 4 bytes needed each time)
	int recvBufferSize = 4096;
	int iResult = setsockopt(loc_recv_udp_socket, SOL_SOCKET, SO_SNDBUF, (char*)&recvBufferSize, sizeof(recvBufferSize));
	if (iResult == SOCKET_ERROR) {
		fprintf(stderr, " *** ERROR *** front_panel_readthread_proc setsockopt SO_SNDBUF failed: %ld\n", WSAGetLastError());
		closesocket(loc_recv_udp_socket);
		WSACleanup();
		_endthreadex(0);
		return 0;
	}


	// -------- 4. Create Events
	// -------- hEvents[0] for Network, hEvents[1] for Program Exit
	WSAEVENT hEvents[2];
	hEvents[0] = WSACreateEvent(); // Network event
	hEvents[1] = CreateEvent(NULL, TRUE, FALSE, NULL); // Exit event
	// --------save event.
	frontpanel_readthread_stop_event = hEvents[1];

	// -------- 5. Associate socket with event for reading
	// -------- This call automatically sets the socket to non-blocking mode
	int iSelectResult = WSAEventSelect(loc_recv_udp_socket, hEvents[0], FD_READ | FD_CLOSE);
	if (iSelectResult == SOCKET_ERROR) {
		fprintf(stderr, " *** ERROR *** front_panel_readthread_proc WSAEventSelect error: %ld\n", WSAGetLastError());
	}


	// --------4. set the send socket to be non-blocking..
	// -- unsigned long nonBlocking = 1;
	// -- int ioctl_return = 0;
	// -- ioctl_return = ioctlsocket(loc_recv_udp_socket, FIONBIO, &nonBlocking);
	// -- if (ioctl_return == SOCKET_ERROR) {
	// -- 	fprintf(stderr, " *** ERROR *** front_panel_readthread_proc - socket non block set failed: %d\n", WSAGetLastError());
	// -- 	loc_last_error = WSAGetLastError();
	// -- 	closesocket(loc_recv_udp_socket);
	// -- 	WSACleanup();
	// -- 	// -------- indicate we are exiting.
	// -- 	frontpanel_readthread_stop_request = 0;
	// -- 	// --------exit thread.
	// -- 	//  --  ExitThread(0);
	// -- 	_endthreadex(0);
	// -- 	return 0;
	// -- }



	// --------set we are running
	running = true;

	// -debug- printf("starting console read command loop\n");

	// -------- loop until requested to stop
	while (running) {

		// --no-- WaitForSingleObject(frontpanel_readtimer_handle, 10);

		// --------wait for events...
		DWORD index = WSAWaitForMultipleEvents(2, hEvents, FALSE, WSA_INFINITE, FALSE);

		// --------increment loop count
		loc_loop_count++;

		// --------socket event triggered.
		if (index == WSA_WAIT_EVENT_0) {
			WSANETWORKEVENTS networkEvents;
			WSAEnumNetworkEvents(loc_recv_udp_socket, hEvents[0], &networkEvents);

			// --------did we get a new message
			if (networkEvents.lNetworkEvents & FD_READ) {
				// -debug- printf(" console command triggered -----\n");
				recvCmdBytes = recv(loc_recv_udp_socket, recvCmdBuffer, sizeof(recvCmdBuffer) - 1, 0);

				if (recvCmdBytes > 0) {
					// -debug- printf(" console command received %d -----\n", recvCmdBytes);
					proc_recv_cmd();
				}
			}
		}
		// --------exit event triggered...
		else if (index == WSA_WAIT_EVENT_0 + 1) {
			// -debug- printf("Exit signal received. Closing loop.\n");
			running = 0;
		}

	}	// -------- processing loop

	// --------close socket and perform WSACleanup...
	closesocket(loc_recv_udp_socket);
	WSACleanup();

	printf(" Front Panel read communications thread exiting.\n");

	// -------- indicate we are exiting.
	frontpanel_readthread_stop_request = 0;

	// --------exit thread.
	//  --  ExitThread(0);
	_endthreadex(0);

	return 0;

}


