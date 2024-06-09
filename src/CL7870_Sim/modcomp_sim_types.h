#pragma once

#include "simj_base.h"

#include <stdbool.h>


typedef union {
	SIMJ_U16 uval;
	SIMJ_S16 sval;
} VAL16;


typedef union {
	SIMJ_U32 uval;
	SIMJ_S32 sval;
	SIMJ_U16 zval[2];
} VAL32;

typedef union {
	SIMJ_U64 uval;
	SIMJ_S64 sval;
	SIMJ_U16 zval[4];
} VAL64;


// --------processor status word.
typedef struct {
	bool cc_c : 1;
	bool cc_o : 1;
	bool cc_z : 1;
	bool cc_n : 1;
	bool oh : 1;
	SIMJ_U8 om : 3;
	SIMJ_U8  grb : 4;
	bool prv : 1;
	SIMJ_U8 im : 3;
}  PSW_BITS;

typedef union {
	SIMJ_U16 all;
	PSW_BITS sep;
} PSW;

// ==================================================
// -------- MEMORY MAPS ARE A WORK IN PROCESS.
// -------- memory map access rights
typedef enum {
	no_access = 0,
	read_only = 1,
	read_execute = 2,
	read_exec_write = 3
} MEM_ACC_RIGHTS;

// --------Mem map entry
typedef union {
	struct {
		SIMJ_U16 mem_page : 13;
		bool shared : 1;
		MEM_ACC_RIGHTS : 2;
	} parts;
	SIMJ_U16 all;
}  MEM_MAP_BITS;

// -------- a complete memory map
// typedef struct {
//	MEM_MAP_BITS[256];
// } MEM_MAP;
// ==================================================

// -------- instruction parts
typedef union {
	struct {
		SIMJ_U8	src_reg : 4;
		SIMJ_U8 dest_reg : 4;
		SIMJ_U8 op_code : 8;
	} parts;
	SIMJ_U16 all;
} INSTRUCTION;


typedef struct {
	SIMJ_U16 pc;		// program counter
	SIMJ_U16 ps;		// status word
} PROC_STATUS_DOUBLEWORD;


typedef union {
	SIMJ_U16 reg16[16];
	SIMJ_U32 reg32[8];
	SIMJ_U64 reg64[4];
} REG_BLOCK;

// -------- IO procedures
typedef void (*DEVICE_OUTPUT_DATA)(SIMJ_U16 device_address, SIMJ_U16 data_word);
typedef void (*DEVICE_OUTPUT_CMD)(SIMJ_U16 device_address, SIMJ_U16 cmd_word);
typedef SIMJ_U16 (*DEVICE_INPUT_STATUS)(SIMJ_U16 device_address );
typedef SIMJ_U16 (*DEVICE_INPUT_DATA)(SIMJ_U16 device_address );
//typedef DWORD (*DEVICE_WORKER_THREAD)(LPVOID lpParam);
#define DEVICE_WORKER_THREAD  LPTHREAD_START_ROUTINE


// --------structure for unsigned word queues
typedef struct {
	volatile SIMJ_U16	data[256];
	volatile SIMJ_U8	next_in_index;
	volatile SIMJ_U8	last_proc_index;
	volatile SIMJ_U16	unproc_count;
	volatile SIMJ_U16	proc_count;
} QUEUE_UWORD;

// TODO: Make this a macro to support different sizes.
#define DEVICE_BUFFER_MAX_LEN 60000
typedef struct {
	unsigned int buf_len;
	volatile unsigned int last_byte_writen_index;		// when next write index = last read index = buffer is full.
	volatile unsigned int last_byte_read_index;			//  when index = nothing to read. 
	volatile SIMJ_U8 buffer[DEVICE_BUFFER_MAX_LEN];
} DEVICE_BUFFER;


// -------- data block for generic device --- ALL DEVICES MUST START WITH THIS AND ADD VALUES TO END OF STRUCT.
typedef struct {
#include "generic_device_variables.h"
} DEVICE_GENERIC_DATA;


// -------- data block for console device
typedef struct {
#include "generic_device_variables.h"
	volatile HANDLE com_handle;
	volatile bool break_detect_enabled;
	CRITICAL_SECTION CritSectStatusUpdate;
} DEVICE_CONSOLE_DATA;


// -------- data block for null device
// #define DEVICE_NULL_MAX_BUFFER 12000
typedef struct {
#include "generic_device_variables.h"
} DEVICE_NULL_DATA;
