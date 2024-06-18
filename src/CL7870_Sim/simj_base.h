// =====================================================================================================================
// 
//  simj_base.h
//
//		Header file for Jim Simpson based emulators and softer.
//		This contains mostly command and basic definitions.  It also contains
//		All platform specific items.
//
//		Copyright 2023, 2024  James A. Simpson.  
//
//
// =====================================================================================================================

// #pragma once

#define SIMJ_PLATFORM MSWINDOWS

#if SIMJ_PLATFORM == MSWINDOWS

#include <windows.h>
#include <synchapi.h>

#define SIMJ_U8	unsigned __int8
#define SIMJ_S8	signed __int8
#define SIMJ_U16 unsigned __int16
#define SIMJ_S16 signed __int16
#define SIMJ_U32 unsigned __int32
#define SIMJ_S32 signed __int32
#define SIMJ_U64 unsigned __int64
#define SIMJ_S64 signed __int64
#define SIMJ_UC unsigned char
#define SIMJ_SC signed char

#else
#error SIMJ  simj_base.h - Runtime platform not defined.   Compile aborted.
#endif


#include <stdbool.h>
#include <stdio.h>


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
typedef SIMJ_U16(*DEVICE_INPUT_STATUS)(SIMJ_U16 device_address);
typedef SIMJ_U16(*DEVICE_INPUT_DATA)(SIMJ_U16 device_address);
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


// -------- cpu
void cpu_init_data();
void cpu_do_fill(SIMJ_U16 new_switch_value);
void cpu_do_run();
void cpu_do_step(SIMJ_U16 step_count);
SIMJ_U16 cpu_get_program_counter();
PSW cpu_get_current_PSW();
SIMJ_U16 cpu_get_register_value(SIMJ_U16 reg_index);
bool cpu_get_power_on();
void cpu_set_register_value(SIMJ_U16 reg_index, SIMJ_U16 reg_value);
void cpu_set_power_on();
void cpu_set_program_counter(SIMJ_U16 pc);
void cpu_set_switches(SIMJ_U16 switch_value);
void cpu_classic_7860();
void cpu_start_thread();
void cpu_stop_thread();
void cpu_stop_data();
void cpu_trigger_clock_interrupt();
void cpu_trigger_console_interrupt();
SIMJ_U16 cpu_get_clock_trigger_count();
SIMJ_U32 cpu_get_instruction_count();
SIMJ_U16 cpu_read_internal_register(SIMJ_U16 int_reg_addr);
void cpu_request_DI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr);
void cpu_request_SI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr);
void cpu_get_interrupt(SIMJ_U16* act, SIMJ_U16* req, SIMJ_U16* ena,
	SIMJ_U32* di_req, SIMJ_U32* di_prc, SIMJ_U32* si_req, SIMJ_U32* si_prc);
void cpu_master_clear();


// -------- Real time clock
void rtclock_start_thread();
void rtclock_stop_thread();

// -------- user command
void process_user_commands(FILE* cmd_src);
void user_cmd_config_execute(char* input_file_name);
void user_cmd_print_help();
void cmd_process_print_prompt();
void cmd_process_parse(char* cmd_line, int max_len, char* cmd_line_parse[], int max_items, int* count_found);
void user_cmd_config_execute(char* input_file_name);
bool user_cmd_parse_u16(char* in_str, SIMJ_U16* out_val, SIMJ_U16 min_val, SIMJ_U16 max_val);
bool user_cmd_parse_device_type(char* in_device, SIMJ_U16* out_device_type);
void user_cmd_attach_device(SIMJ_U16 device_type, SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp, SIMJ_U16 extra_count, char* extra1, char* extra2);

// -------- iop
void iop_init_data();

// -------- device common
void* device_common_device_buffer_allocate(SIMJ_U16 device_address, size_t buffer_size);
void device_common_remove(SIMJ_U16 device_address);
uintptr_t device_common_start_thread(void* data_buffer, DEVICE_WORKER_THREAD thread_proc, unsigned* thread_id);
void device_common_stop_all();

int device_common_serial_close(HANDLE com_handle, DWORD* last_error);
int device_common_serial_open(char* com_port, HANDLE* com_handle, DWORD* last_error);
void device_common_serial_print_settings(DCB this_dcb);
int device_common_serial_set_params(HANDLE hCom, DWORD* last_error, bool USE_HDWR_OUTPUT_HANDSHAKE);

void device_common_buffer_init(volatile DEVICE_BUFFER* buff);
bool device_common_buffer_isempty(volatile DEVICE_BUFFER* buff);
bool device_common_buffer_isfull(volatile DEVICE_BUFFER* buff);
bool device_common_buffer_get(volatile DEVICE_BUFFER* buff, SIMJ_U8* to_get);
void device_common_buffer_put(volatile DEVICE_BUFFER* buff, SIMJ_U8 to_put);
void device_common_buffer_set_empty(volatile DEVICE_BUFFER* buff);

void device_common_thread_init(LPVOID data_buffer,
	DEVICE_WORKER_THREAD worker_proc,
	DEVICE_OUTPUT_DATA output_data_proc,
	DEVICE_OUTPUT_CMD output_cmd_proc,
	DEVICE_INPUT_DATA input_data_proc,
	DEVICE_INPUT_STATUS input_status_proc);

void device_common_capture_console(SIMJ_U8 in_data);
void device_common_capture_console_close();

// -------- specific devices
void device_null_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp);
void device_console_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp);

// -------- generic queue routines
void que_uword_init(volatile QUEUE_UWORD* que);
bool que_uword_recv(volatile QUEUE_UWORD* que, SIMJ_U16* cmd_word);
bool que_uword_send(volatile QUEUE_UWORD* queue, SIMJ_U16 value);

// -------- display routines
void disp_devices(FILE* io_unit);
void disp_cur_reg(FILE* io_unit);
void disp_interrupts(FILE* io_unit);
void disp_pc(FILE* io_unit, SIMJ_U16 loc_pc);
void disp_psw(FILE* io_unit, PSW loc_psw);
void disp_instruction_use(FILE* io_unit);

// -------- util
void util_get_opcode_disp(SIMJ_U16 instruction, char* op_buffer, size_t buf_size);
bool util_is_same_stream(FILE* one, FILE* two);

// --------templates for external interface
void rmi_request(SIMJ_U16 rmi_request);

// --------memory
SIMJ_U16 memory_plane_RMPS(SIMJ_U16 first_reg, SIMJ_U16 second_reg);
void memory_plane_init();


#include "modcomp_sim_globals.h"


// ================================================================================================
