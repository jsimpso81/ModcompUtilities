#pragma once

// -------- cpu
void init_cpu_data();
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
void init_iop_data();

// -------- device common
void device_common_remove(unsigned __int16 device_address);
void device_common_stop_all();
int device_common_serial_set_params();

// -------- specific devices
void device_null_init(unsigned __int16 device_address);






