#pragma once
#include <windows.h>
#include <stdio.h>
#include <stdbool.h>

#include "modcomp_sim_types.h"

// -------- cpu
void cpu_init_data();
unsigned __int16 cpu_get_program_counter();
PSW cpu_get_current_PSW();
unsigned __int16 cpu_get_register_value(unsigned __int16 reg_index);
void cpu_set_register_value(unsigned __int16 reg_index, unsigned __int16 reg_value);
void cpu_set_program_counter(unsigned __int16 pc);
void cpu_set_switches(unsigned __int16 switch_value);
void cpu_classic_7860();
void cpu_start_thread();
void cpu_stop_thread();
void cpu_stop_data();
void cpu_trigger_clock_interrupt();
void cpu_trigger_console_interrupt();
unsigned __int16 cpu_get_clock_trigger_count();
unsigned __int32 cpu_get_instruction_count();
void cpu_request_DI(unsigned __int16 bus, unsigned __int16 prio, unsigned __int16 dev_addr);
void cpu_request_SI(unsigned __int16 bus, unsigned __int16 prio, unsigned __int16 dev_addr);
void cpu_get_interrupt(unsigned __int16* act, unsigned __int16* req, unsigned __int16* ena,
	unsigned __int32* di_req, unsigned __int32* di_prc, unsigned __int32* si_req, unsigned __int32* si_prc);
void cpu_master_clear();

// -------- Real time clock
void rtclock_start_thread();
void rtclock_stop_thread();

// -------- user command
void process_user_commands();
void user_cmd_print_help();
void cmd_process_print_prompt();
void cmd_process_parse(char* cmd_line, int max_len, char* cmd_line_parse[], int max_items, int* count_found);

// -------- iop
void iop_init_data();

// -------- device common
void* device_common_device_buffer_allocate(unsigned __int16 device_address, size_t buffer_size);
void device_common_remove(unsigned __int16 device_address);
uintptr_t device_common_start_thread(void* data_buffer, DEVICE_WORKER_THREAD thread_proc, unsigned* thread_id);
void device_common_stop_all();

int device_common_serial_close( HANDLE com_handle, DWORD* last_error);
int device_common_serial_open( char* com_port, HANDLE *com_handle, DWORD *last_error );
void device_common_serial_print_settings(DCB this_dcb);
int device_common_serial_set_params(HANDLE hCom, DWORD* last_error, bool USE_HDWR_OUTPUT_HANDSHAKE);

void device_common_buffer_init(volatile DEVICE_BUFFER* buff);
bool device_common_buffer_isempty(volatile DEVICE_BUFFER* buff);
bool device_common_buffer_isfull(volatile DEVICE_BUFFER* buff);
bool device_common_buffer_get(volatile DEVICE_BUFFER* buff, unsigned __int8* to_get);
void device_common_buffer_put(volatile DEVICE_BUFFER* buff, unsigned __int8 to_put);
void device_common_buffer_set_empty(volatile DEVICE_BUFFER* buff);

void device_common_thread_init(LPVOID data_buffer,
	DEVICE_WORKER_THREAD worker_proc,
	DEVICE_OUTPUT_DATA output_data_proc,
	DEVICE_OUTPUT_CMD output_cmd_proc,
	DEVICE_INPUT_DATA input_data_proc,
	DEVICE_INPUT_STATUS input_status_proc);

// -------- specific devices
void device_null_init(unsigned __int16 device_address, unsigned __int16 bus, unsigned __int16 prio, unsigned __int16 dmp);
void device_console_init(unsigned __int16 device_address, unsigned __int16 bus, unsigned __int16 prio, unsigned __int16 dmp);

// -------- generic queue routines
void que_uword_init(volatile QUEUE_UWORD* que);
bool que_uword_recv(volatile QUEUE_UWORD* que, __int16* cmd_word);
bool que_uword_send(volatile QUEUE_UWORD* queue, unsigned __int16 value);

// -------- display routines
void disp_devices( FILE* io_unit );
void disp_cur_reg( FILE* io_unit);
void disp_interrupts(FILE* io_unit);
void disp_pc( FILE* io_unit, unsigned __int16 loc_pc);
void disp_psw( FILE* io_unit, PSW loc_psw);
void disp_instruction_use( FILE* io_unit);

// -------- util
void util_get_opcode_disp(unsigned __int16 instruction, char* op_buffer, size_t buf_size);

// --------templates for external interface
void rmi_request(unsigned __int16 rmi_request);

// --------memory
unsigned __int16 memory_plane_RMPS(unsigned __int16 first_reg, unsigned __int16 second_reg);
void memory_plane_init();

