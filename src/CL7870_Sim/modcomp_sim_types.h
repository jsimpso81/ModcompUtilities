#pragma once
#include <windows.h>

typedef struct {
	unsigned __int16 pc;		// program counter
	unsigned __int16 ps;		// status word
} PROC_STATUS_DOUBLEWORD;


// -------- IO procedures
typedef void (*DEVICE_OUTPUT_DATA)(unsigned __int16 device_address, unsigned __int16 data_word);
typedef void (*DEVICE_OUTPUT_CMD)(unsigned __int16 device_address, unsigned __int16 cmd_word);
typedef unsigned __int16 (*DEVICE_INPUT_STATUS)(unsigned __int16 device_address );
typedef unsigned __int16 (*DEVICE_INPUT_DATA)(unsigned __int16 device_address );
//typedef DWORD (*DEVICE_WORKER_THREAD)(LPVOID lpParam);
#define DEVICE_WORKER_THREAD  LPTHREAD_START_ROUTINE


// --------structure for unsigned word queues
typedef struct {
	volatile unsigned __int16	data[256];
	volatile unsigned __int8	next_in_index;
	volatile unsigned __int8	last_proc_index;
	volatile unsigned __int16	unproc_count;
	volatile unsigned __int16	proc_count;
} QUEUE_UWORD;


// -------- data block for console device
#define DEVICE_CONSOLE_MAX_BUFFER 8000
typedef struct {
	volatile unsigned __int16 ctrl_wake;
	volatile unsigned __int16 ctrl_status;
	QUEUE_UWORD ctrl_command_que;
	volatile unsigned __int16 ctrl_input_buffer_count;
	volatile unsigned int ctrl_input_buffer_index;
	volatile unsigned __int8 ctrl_input_buffer[DEVICE_CONSOLE_MAX_BUFFER];
	volatile unsigned __int16 ctrl_output_buffer_count;
	volatile unsigned __int16 ctrl_output_buffer_index;
	volatile unsigned __int8 ctrl_output_buffer[DEVICE_CONSOLE_MAX_BUFFER];
} DEVICE_CONSOLE_DATA;


// -------- data block for null device
#define DEVICE_NULL_MAX_BUFFER 8000
typedef struct {
	volatile unsigned __int16 ctrl_wake;
	volatile unsigned __int16 ctrl_status;
	QUEUE_UWORD ctrl_command_que;
	volatile unsigned __int16 ctrl_input_buffer_count;
	volatile unsigned int ctrl_input_buffer_index;
	volatile unsigned __int8 ctrl_input_buffer[DEVICE_NULL_MAX_BUFFER];
	volatile unsigned __int16 ctrl_output_buffer_count;
	volatile unsigned __int16 ctrl_output_buffer_index;
	volatile unsigned __int8 ctrl_output_buffer[DEVICE_NULL_MAX_BUFFER];
} DEVICE_NULL_DATA;
