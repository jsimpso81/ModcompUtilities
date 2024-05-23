#pragma once
#include <windows.h>
#include <stdbool.h>

#include "modcomp_sim_types.h"

// -------- cpu
void cpu_init_data();
unsigned __int16 cpu_get_program_counter();
unsigned __int16 cpu_get_register_value(unsigned __int16 reg_index);
void cpu_set_register_value(unsigned __int16 reg_index, unsigned __int16 reg_value);
void cpu_set_program_counter(unsigned __int16 pc);
void classic_7860_cpu();
void start_cpu_thread();
void stop_cpu_thread();

// -------- user command
void process_user_commands();
void user_cmd_print_help();
void cmd_process_print_prompt();
void cmd_process_parse(char* cmd_line, int max_len, char* cmd_line_parse[], int max_items, int* count_found);

// -------- iop
void iop_init_data();

// -------- device common
void* device_common_buffer_allocate(unsigned __int16 device_address, size_t buffer_size);
void device_common_remove(unsigned __int16 device_address);
void device_common_stop_all();
int device_common_serial_close( HANDLE com_handle, DWORD* last_error);
int device_common_serial_open( char* com_port, HANDLE *com_handle, DWORD *last_error );
void device_common_serial_print_settings(DCB this_dcb);
int device_common_serial_set_params(HANDLE hCom, DWORD* last_error);
void device_common_thread_init(LPVOID data_buffer,
	DEVICE_WORKER_THREAD worker_proc,
	DEVICE_OUTPUT_DATA output_data_proc,
	DEVICE_OUTPUT_CMD output_cmd_proc,
	DEVICE_INPUT_DATA input_data_proc,
	DEVICE_INPUT_STATUS input_status_proc);

// -------- specific devices
void device_null_init(unsigned __int16 device_address);

// -------- generic queue routines
void que_uword_init(QUEUE_UWORD* que);
bool que_uword_recv(QUEUE_UWORD* que, __int16* cmd_word);
bool que_uword_send(QUEUE_UWORD* queue, unsigned __int16 value);






