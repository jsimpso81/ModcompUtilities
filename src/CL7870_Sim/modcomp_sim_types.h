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
	struct {
		unsigned __int8	src_reg : 4;
		unsigned __int8 dest_reg : 4;
		unsigned __int8 op_code : 8;
	} parts;
	unsigned __int16 all;
} INSTRUCTION;


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

// TODO: Make this a macro to support different sizes.
#define DEVICE_BUFFER_MAX_LEN 12000
typedef struct {
	unsigned int buf_len;
	unsigned int last_byte_writen_index;		// when next write index = last read index = buffer is full.
	unsigned int last_byte_read_index;			//  when index = nothing to read. 
	unsigned __int8 buffer[DEVICE_BUFFER_MAX_LEN];
} DEVICE_BUFFER;

// -------- data block for console device
typedef struct {
#include "generic_device_variables.h"
	volatile HANDLE com_handle;
	DEVICE_BUFFER in_buff;
	DEVICE_BUFFER out_buff;
} DEVICE_CONSOLE_DATA;


// -------- data block for null device
#define DEVICE_NULL_MAX_BUFFER 12000
typedef struct {
#include "generic_device_variables.h"
	DEVICE_BUFFER in_buff;
	DEVICE_BUFFER out_buff;
} DEVICE_NULL_DATA;
