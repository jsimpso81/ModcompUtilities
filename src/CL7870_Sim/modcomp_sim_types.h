#pragma once
#include <windows.h>
#include <stdbool.h>

// --------processor status word.
typedef struct {
	bool cc_c : 1;
	bool cc_o : 1;
	bool cc_z : 1;
	bool cc_n : 1;
	bool oh : 1;
	unsigned __int8 om : 3;
	unsigned _int8  grb : 4;
	bool prv : 1;
	unsigned __int8 im : 3;
}  PSW_BITS;

typedef union {
	unsigned __int16 all;
	PSW_BITS sep;
} PSW;


typedef union {
	unsigned __int16 uval;
	signed __int16 sval;
} VAL16;


typedef union {
	unsigned __int32 uval;
	signed __int32 sval;
} VAL32;



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


// -------- data block for generic device --- ALL DEVICES MUST START WITH THIS AND ADD VALUES TO END OF STRUCT.
typedef struct {
#include "generic_device_variables.h"
} DEVICE_GENERIC_DATA;


// -------- data block for console device
#define DEVICE_CONSOLE_MAX_BUFFER 8000
typedef struct {
#include "generic_device_variables.h"
	volatile unsigned __int16 ctrl_input_buffer_count;
	volatile unsigned int ctrl_input_buffer_index;
	volatile unsigned __int8 ctrl_input_buffer[DEVICE_CONSOLE_MAX_BUFFER];
	volatile unsigned __int16 ctrl_output_buffer_count;
	volatile unsigned __int16 ctrl_output_buffer_index;
	volatile unsigned __int8 ctrl_output_buffer[DEVICE_CONSOLE_MAX_BUFFER];
} DEVICE_CONSOLE_DATA;


// -------- data block for null device
#define DEVICE_NULL_MAX_BUFFER 12000
typedef struct {
#include "generic_device_variables.h"
	volatile unsigned __int16 ctrl_input_buffer_count;
	volatile unsigned int ctrl_input_buffer_index;
	volatile unsigned __int8 ctrl_input_buffer[DEVICE_NULL_MAX_BUFFER];
	volatile unsigned __int16 ctrl_output_buffer_count;
	volatile unsigned __int16 ctrl_output_buffer_index;
	volatile unsigned __int8 ctrl_output_buffer[DEVICE_NULL_MAX_BUFFER];
} DEVICE_NULL_DATA;
