// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			simj_base.h
//
//	Description:	Main definitions for simulator environment.  Defines base data types based on
//					host operating system and architecture.  Defines all other data types.
//					Defines callable procedures.  Defines global memory. 
//
//	Externally accessible routines:
// 
// Notes:
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================

// =====================================================================================================================
// 
//  simj_base.h
//
//		Header file for Jim Simpson based emulators and software.
//		This contains mostly command and basic definitions.  It also contains
//		All platform specific items.
//
//		Copyright 2023, 2024  James A. Simpson.  
//
//
// =====================================================================================================================

// =====================================================================================================================
// --------define simulation customizations ---- later these can be moved to the build routines
#define SIMJ_SIM_CPU 7830		// values are "7860" "7830" "II15" "3285"
#define SIMJ_SIM_HAS_EAU false	// values are true, false
#define SIMJ_SIM_CPU_1MEGMAX	// force memory to be one meg max.

// =====================================================================================================================
// --------define operating system and host architecture
#define SIMJ_PLATFORM MSWINDOWS
#define SIMJ_ARCH X86

// =====================================================================================================================
// --------define platform and architecture specific includes and data types.
#if SIMJ_PLATFORM == MSWINDOWS

#pragma once

#include <windows.h>
#include <synchapi.h>
#pragma comment(lib, "ws2_32.lib") // Link with ws2_32.lib

// ---------- simulator data types.
// -- 8 bit unsigned integer
#define SIMJ_U8	unsigned __int8
// -- 8 bit signed integer
#define SIMJ_S8	signed __int8
// -- 16 bit unsigned integer
#define SIMJ_U16 unsigned __int16
// -- 16 bit signed integer
#define SIMJ_S16 signed __int16
// -- 32 bit unsigned integer
#define SIMJ_U32 unsigned __int32
// -- 32 bit signed integer
#define SIMJ_S32 signed __int32
// -- 64 bit unsigned integer
#define SIMJ_U64 unsigned __int64
// -- 64 bit signed integer
#define SIMJ_S64 signed __int64
// -- 8 bit unsigned character
#define SIMJ_UC unsigned char
// -- 8 bit signed character
#define SIMJ_SC signed char
// -- 32 bit IEEE float
#define SIMJ_F32 float 
// -- 64 bit IEEE float
#define SIMJ_F64 double
// -- 32 bit Modcomp float storage
#define SIMJ_M32 SIMJ_U32
// -- 48 bit Modcomp float storage
#define SIMJ_M48 SIMJ_U64	// NEED TO USE 64 bits to store....
// -- 64 bit Modcomp float storage
#define SIMJ_M64 SIMJ_U64

#define SIMJ_TAPE_DPI __int64
#define SIMJ_TAPE_ERR DWORD

// -------- Resource 
#define DEFINE_RESOURCE( NAME )  CRITICAL_SECTION NAME
#define INIT_RESOURCE( NAME ) InitializeCriticalSectionAndSpinCount(&NAME, 0x00000400);
#define DELETE_RESOURCE( NAME ) DeleteCriticalSection(&NAME)
#define TAKE_RESOURCE( NAME ) 	EnterCriticalSection(&NAME)
#define GIVE_RESOURCE( NAME )  LeaveCriticalSection(&NAME)

#else
#error SIMJ  simj_base.h - Runtime platform not defined.   Compile aborted.
#endif

// =====================================================================================================================
// -------- includes always used.
#include <stdbool.h>
#include <stdio.h>

// =====================================================================================================================
// -------- simulator data types.
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

// -------- fields in PSW
#define PSW_MASK_IMAP	(SIMJ_U16)0xE000
#define PSW_MASK_PRV	(SIMJ_U16)0x1000
#define PSW_MASK_GRB	(SIMJ_U16)0x0F00
#define PSW_MASK_OMAP	(SIMJ_U16)0x00E0
#define PSW_MASK_OH		(SIMJ_U16)0x0010
#define PSW_MASK_CC_ALL (SIMJ_U16)0x000F
#define PSW_MASK_CC_N	(SIMJ_U16)0x0008
#define PSW_MASK_CC_Z	(SIMJ_U16)0x0004
#define PSW_MASK_CC_O	(SIMJ_U16)0x0002
#define PSW_MASK_CC_C	(SIMJ_U16)0x0001

#define PSW_SHIFT_IMAP 13
#define PSW_SHIFT_GRB  8
#define PSW_SHIFT_OMAP 5

// ==================================================
// -------- MEMORY MAPS ARE A WORK IN PROCESS.
// -------- memory map access rights
//typedef enum {
//	no_access = 0,
//	read_only = 1,
//	read_execute = 2,
//	read_exec_write = 3
//} MEM_ACC_RIGHTS;

// --------Mem map entry
// --------NOTE ACC and probably SHARED don't work.
typedef union {
	struct {
		SIMJ_U16 mem_page : 13;
		SIMJ_U8 shared : 1;
		SIMJ_U8 acc : 2;
	} parts;
	SIMJ_U16 all;
}  MEM_MAP_WORD;

#define MEM_MAP_WORD_PAGE_MASK		(SIMJ_U16)0x1fff
#define MEM_MAP_WORD_SHARED_MASK	(SIMJ_U16)0x2000
#define MEM_MAP_WORD_ACC_MASK		(SIMJ_U16)0xc000

#define MEM_MAP_WORD_SHARED_SHIFT	13
#define MEM_MAP_WORD_ACC_SHIFT		14

#define MEM_MAP_WORD_ACC_NONE		(SIMJ_U16)0x0000
#define MEM_MAP_WORD_ACC_READ		(SIMJ_U16)0x4000
#define MEM_MAP_WORD_ACC_EXEC		(SIMJ_U16)0x8000
#define MEM_MAP_WORD_ACC_WRITE		(SIMJ_U16)0xC000

// -------- a complete memory map
typedef struct {
	MEM_MAP_WORD entry[256];
} MEM_MAP;
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
typedef void (*DEVICE_MOUNT_UNIT)(SIMJ_U16 device_address, SIMJ_U16 unit, bool readonly, char* filename);
typedef void (*DEVICE_DISMOUNT_UNIT)(SIMJ_U16 device_address, SIMJ_U16 unit);


// --------number of supported controller types.
#define VALID_DEVICE_COUNT 11
#define DEVICE_TYPE_NULL			1
#define DEVICE_TYPE_CONSOLE			2
#define DEVICE_TYPE_CONSOLETCP		3
#define DEVICE_TYPE_TAPE			4
#define DEVICE_TYPE_DISC_MH			5
#define DEVICE_TYPE_DISC_LX			6
#define DEVICE_TYPE_DISC_IPS2		7
#define DEVICE_TYPE_A4811			8
#define DEVICE_TYPE_A4808			9
#define DEVICE_TYPE_MODACSIII		10
#define DEVICE_TYPE_MODACS1600		11


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
typedef volatile struct {
#include "generic_device_variables.h"
} DEVICE_GENERIC_DATA;


// -------- data block for console device, also used for consoletcp
typedef volatile struct {
#include "generic_device_variables.h"
	char filename[255];  // file name or device name on simulator (com1:, disc / tapes have separate names per unit.)
	SIMJ_U16 ipport;	 // tcp/udp port number for ip devices (console, async, etc.)
	volatile HANDLE com_handle;
	volatile bool break_detect_enabled;
	DEFINE_RESOURCE(ResourceStatusUpdate);
} DEVICE_CONSOLE_DATA;


// -------- data block for null device
// #define DEVICE_NULL_MAX_BUFFER 12000
typedef volatile struct {
#include "generic_device_variables.h"
} DEVICE_NULL_DATA;

// -------- data block for tape device
typedef volatile struct {
#include "generic_device_variables.h"
	DEFINE_RESOURCE(ResourceStatusUpdate);
	volatile SIMJ_S16 cur_sel_unit;
	volatile SIMJ_S16 io_cmd[4];	// command sent to io thread...
	volatile bool tape_mounted[4];
	volatile bool online[4];
	volatile bool bot[4];	// beginning of tape.
	volatile bool eof[4];	// end of file.
	volatile bool eot[4];	// end of tape -- may not need??
	volatile SIMJ_TAPE_DPI dpi[4];
	volatile SIMJ_U32 last_recsize[4];
	volatile bool tape_readonly[4];
	volatile FILE* tape_file_handle[4];
	volatile char tape_filename[4][255];  // filenames for mounted file.

} DEVICE_TAPE_DATA;


// -------- data block for disc device
typedef volatile struct {
#include "generic_device_variables.h"
	DEFINE_RESOURCE(ResourceStatusUpdate);
	volatile SIMJ_S16 cur_sel_unit;
	volatile SIMJ_S16 io_cmd[4];	// command sent to io thread...
	volatile SIMJ_U64 dpi[4];		// sent with cmd
	volatile SIMJ_U64 plat[4];		// saved platter
	volatile SIMJ_U64 head[4];		// saved head
	volatile SIMJ_U64 cyl[4];		// saved cyl
	volatile bool disc_mounted[4];
	volatile bool online[4];
	// volatile bool bot[4];	// beginning of disc.
	volatile bool eof[4];	// end of file.
	// volatile bool eot[4];	// end of disc
	volatile SIMJ_U32 last_recsize[4];
	volatile bool disc_readonly[4];
	volatile FILE* disc_file_handle[4];
	volatile char disc_filename[4][255];  // filenames for mounted file.

} DEVICE_DISC_DATA;


// -------- return status for floating point conversion routines.
#define SIMJ_FLTCVT_GOOD			1
#define SIMJ_FLTCVT_OVERFLOW		2
#define SIMJ_FLTCVT_ACCURACY_LOSS	3
#define SIMJ_FLTCVT_OTHER_ERR		4

// =====================================================================================================================
// --------callable procedures.
// 
// -------- cpu
void cpu_init_data();
void cpu_do_fill(SIMJ_U16 new_switch_value);
void cpu_do_run();
void cpu_do_step(SIMJ_U16 step_count);
SIMJ_U16 cpu_get_program_counter();
PSW cpu_get_current_PSW();
void cpu_get_instruction_trace(SIMJ_U16* inx,
	SIMJ_U16 pc_trace[1024], PSW psw_trace[1024], SIMJ_U16 actint_trace[1024],
	SIMJ_U32 trace[1024], SIMJ_U32 trace_w1[1024], SIMJ_U32 trace_w2[1024],
	SIMJ_U32 trace_w3[1024], SIMJ_U32 trace_w4[1024]);
bool cpu_get_virtual_mode();
void cpu_get_virtual_map(SIMJ_U16 map, MEM_MAP* copied_map);
SIMJ_U16 cpu_get_register_value(SIMJ_U16 reg_index);
SIMJ_U16 cpu_get_register_block_value(SIMJ_U16 reg_block_numb, SIMJ_U16 reg_numb);
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
void cpu_trigger_memory_parity_interrupt(SIMJ_U16 parity_interrupt_type);
void cpu_trigger_power_interrupt(SIMJ_U16 power_interrupt_type);
SIMJ_U16 cpu_get_clock_trigger_count();
SIMJ_U32 cpu_get_instruction_count();
SIMJ_U16 cpu_read_internal_register(SIMJ_U16 int_reg_addr);
void cpu_request_DI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr);
void cpu_request_SI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr);
void cpu_reset_DI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr);
void cpu_reset_SI(SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dev_addr);
void cpu_get_interrupt(SIMJ_U16* act, SIMJ_U16* req, SIMJ_U16* ena,
	SIMJ_U32* di_req, SIMJ_U32* di_prc, SIMJ_U32* si_req, SIMJ_U32* si_prc);
void cpu_master_clear();
SIMJ_U16 cpu_get_virtual_mem(SIMJ_U16 map_numb, SIMJ_U16 virt_addr);

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
void user_cmd_attach_device(SIMJ_U16 device_type, SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp,
	SIMJ_U16 extra_count, char* extra1, char* extra2, char* extra3);
void user_cmd_mount_device(SIMJ_U16 device_address, SIMJ_U16 unit, char* filename, bool readonly);
void user_cmd_dismount_device(SIMJ_U16 device_address, SIMJ_U16 unit);

// -------- iop
void iop_init_data();
int iop_finish_dmp_read(bool virt, SIMJ_S16 tc, SIMJ_U16 ta, SIMJ_U32 abs_tc_addr,
	SIMJ_U16 vdmp_miap_page, SIMJ_U16 vdmp_miap_length,
	SIMJ_U16* databuffer, int words_in_buffer);
int iop_get_dmp_word_count(bool virt, SIMJ_S16 tc, SIMJ_U16 ta, SIMJ_U32 tc_abs_addr,
	SIMJ_U16 vdmp_miap_page, SIMJ_U16 vdmp_miap_length,
	SIMJ_U16* dmp_words_requested);
int iop_get_dmp_parameters(int device_address, SIMJ_U16 dmp, SIMJ_S16* raw_tc, SIMJ_U16* raw_ta, bool* virt, SIMJ_U32* abs_tc_addr);
int iop_load_via_miap(SIMJ_U16 miap, SIMJ_U16 miap_len, SIMJ_U16 virt_addr, SIMJ_U16* word_loaded, SIMJ_U32* abs_addr);
int iop_store_via_miap(SIMJ_U16 miap, SIMJ_U16 miap_len, SIMJ_U16 virt_addr, SIMJ_U16 word_to_store);

// -------- device common
void* device_common_device_buffer_allocate(SIMJ_U16 device_address, size_t buffer_size);
void device_common_remove(SIMJ_U16 device_address);
uintptr_t device_common_start_thread(void* data_buffer, DEVICE_WORKER_THREAD thread_proc, unsigned* thread_id);
void device_common_stop_all();

int device_common_serial_close(HANDLE com_handle, DWORD* last_error);
int device_common_serial_open(char* com_port, HANDLE* com_handle, DWORD* last_error);
void device_common_serial_print_settings(DCB this_dcb);
int device_common_serial_set_params(HANDLE hCom, DWORD* last_error, bool USE_HDWR_OUTPUT_HANDSHAKE, SIMJ_U16 baud);

int device_common_raw_socket_open(SIMJ_U16 port, SOCKET* tcp_socket, DWORD* last_error);
int device_common_raw_socket_close(SOCKET tcp_socket, DWORD* last_error);
int device_common_raw_socket_read(SOCKET tcp_socket, DWORD desired_read_bytes,
	SIMJ_U8* loc_read_data, DWORD* actual_read_bytes, DWORD* last_error);
int device_common_raw_socket_write(SOCKET tcp_socket, DWORD desired_write_bytes,
	SIMJ_U8* loc_write_data, DWORD* actual_written_bytes, DWORD* last_error);

// --------tape image file processing routines...
int device_common_tape_close(FILE** tape_file_handle, SIMJ_TAPE_DPI* tape_dpi);
int device_common_tape_open(char* tape_filename, bool read_only, FILE** tape_file_handle,
	SIMJ_TAPE_DPI* tape_dpi, SIMJ_TAPE_ERR* last_error);
int device_common_tape_rewind(FILE** tape_file_handle, SIMJ_TAPE_DPI* tape_dpi);
int device_common_tape_read_record( FILE** tape_file_handle, SIMJ_TAPE_DPI* current_file_position,
	void* buf, SIMJ_U32 max_buf_bytes, SIMJ_U32* bytes_read, bool* end_of_file);

// --------disc image file processing routines.
int device_common_disc_open(char* disc_filename, bool read_only, FILE** disc_file_handle, DWORD* last_error);
int device_common_disc_read_sector(FILE* fp, unsigned __int64 sector, void* raw_sector_buf, unsigned __int16* flags);

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
	DEVICE_INPUT_STATUS input_status_proc,
	DEVICE_MOUNT_UNIT mount_unit_proc,
	DEVICE_DISMOUNT_UNIT dismount_unit_proc);

void device_common_capture_console(SIMJ_U8 in_data);
void device_common_capture_console_close();

// -------- specific devices
void device_null_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp);
void device_console_serial_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp, char* filename, SIMJ_U16 baud);
void device_console_tcp_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp, SIMJ_U16 port);
void device_tape_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp);
void device_disc_mh_init(SIMJ_U16 device_address, SIMJ_U16 bus, SIMJ_U16 prio, SIMJ_U16 dmp);

// -------- generic queue routines
void que_uword_init(volatile QUEUE_UWORD* que);
bool que_uword_recv(volatile QUEUE_UWORD* que, SIMJ_U16* cmd_word);
bool que_uword_send(volatile QUEUE_UWORD* queue, SIMJ_U16 value);

// -------- display routines
void disp_devices(FILE* io_unit);
void disp_cur_reg(FILE* io_unit);
void disp_reg_block(FILE* io_unit, SIMJ_U16 register_block);
void disp_interrupts(FILE* io_unit);
void disp_pc(FILE* io_unit, SIMJ_U16 loc_pc);
void disp_psw(FILE* io_unit, PSW loc_psw);
void disp_instruction_use(FILE* io_unit);
void disp_instruction_trace(FILE* io_unit);
void disp_virtual_map(FILE* io_unit, SIMJ_U16 map);

// -------- util
unsigned __int16  bswap16(unsigned __int16 a);
void util_get_opcode_disp(SIMJ_U16 instruction, char* op_buffer, size_t buf_size);
bool util_is_same_stream(FILE* one, FILE* two);
void util_high_res_spin_wait(SIMJ_U16 wait_time_100ns);
LARGE_INTEGER util_high_res_spin_wait_get_start();
void util_high_res_spin_wait_finish(SIMJ_U16 wait_time_100ns, LARGE_INTEGER StartingTime);

// -------- util floating point conversions
// -------- convert 32 bit signed integer to IEEE 64 bit float
SIMJ_U32 util_cvt_S32_IEEE64(SIMJ_S32 s32_in, SIMJ_F64* f64_out);
// -------- convert 64 bit signed integer to IEEE 64 bit float
SIMJ_U32 util_cvt_S64_IEEE64(SIMJ_S64 s64_in, SIMJ_F64* f64_out);
// -------- convert 32 bit signed integer to Modcomp 32 bit float
SIMJ_U32 util_cvt_S32_MCS32(SIMJ_S32 s32_in, SIMJ_M32* m32_out);
// -------- convert 32 bit signed integer to Modcomp 64 bit float
SIMJ_U32 util_cvt_S32_MCS64(SIMJ_S32 s32_in, SIMJ_M64* m64_out);
// -------- convert 64 bit signed integer to Modcomp 64 bit float
SIMJ_U32 util_cvt_S64_MCS64(SIMJ_S64 s64_in, SIMJ_M64* m64_out);
// -------- convert IEEE 64 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_IEEE64_S32(SIMJ_F64 f64_in, SIMJ_S32* s32_out);
// -------- convert IEEE 64 bit float to signed 64 bit integer
SIMJ_U32 util_cvt_IEEE64_S64(SIMJ_F64 f64_in, SIMJ_S64* s64_out);
// -------- convert Modcomp 32 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_MCS32_S32(SIMJ_M32 m32_in, SIMJ_S32* s32_out);
// -------- convert Modcomp 48 bit float (double float) to signed 32 bit integer
SIMJ_U32 util_cvt_MCS48_S32(SIMJ_M48 m48_in, SIMJ_S32* s32_out);
// -------- convert Modcomp 64 bit float to signed 32 bit integer
SIMJ_U32 util_cvt_MCS64_S32(SIMJ_M64 m64_in, SIMJ_S32* s32_out);
// -------- convert Modcomp 64 bit float to signed 64 bit integer
SIMJ_U32 util_cvt_MCS64_S64(SIMJ_M64 m64_in, SIMJ_S64* s64_out);
// -------- convert Modcomp 32 bit float to IEEE 64 bit float
SIMJ_U32 util_cvt_MCS32_IEEE64(SIMJ_M32 m32_in, SIMJ_F64* f64_out);
// -------- convert Modcomp 64 bit float to IEEE 64 bit float
SIMJ_U32 util_cvt_MCS64_IEEE64(SIMJ_M64 m64_in, SIMJ_F64* f64_out);
// -------- convert IEEE 64 bit float to Modcomp 32 bit float
SIMJ_U32 util_cvt_IEEE64_MCS32(SIMJ_F64 f64_in, SIMJ_M32* m32_out);
// -------- convert IEEE 64 bit float to Modcomp 64 bit float
SIMJ_U32 util_cvt_IEEE64_MCS64(SIMJ_F64 f64_in, SIMJ_M64* m64_out);
// -------- convert Modcomp 64 bit float to Modcomp 32 bit float
SIMJ_U32 util_cvt_MCS64_MCS32(SIMJ_M64 m64_in, SIMJ_M32* m32_out);
// -------- convert Modcomp 64 bit float to Modcomp 48 bit float
SIMJ_U32 util_cvt_MCS64_MCS48(SIMJ_M64 m64_in, SIMJ_M48* m48_out);
// -------- convert Modcomp 48 bit float to Modcomp 64 bit float
SIMJ_U32 util_cvt_MCS48_MCS64(SIMJ_M48 m48_in, SIMJ_M64* m64_out);
// -------- convert Modcomp 48 bit float to IEEE 64 bit float
SIMJ_U32 util_cvt_MCS48_IEEE64(SIMJ_M64 m64_in, SIMJ_F64* f64_out);
// -------- convert IEEE 64 bit float to Modcomp 48 bit float
SIMJ_U32 util_cvt_IEEE64_MCS48(SIMJ_F64 f64_in, SIMJ_M48* m48_out);

// --------templates for external interface
void rmi_request(SIMJ_U16 rmi_request);

// --------memory
SIMJ_U16 memory_plane_RMPS(SIMJ_U16 first_reg, SIMJ_U16 second_reg);
void memory_plane_WMPS(SIMJ_U16 first_reg, SIMJ_U16 second_reg, SIMJ_U16 data_register);
void memory_plane_init();

// =====================================================================================================================
// -------- global data.
#include "modcomp_sim_globals.h"


// =====================================================================================================================
