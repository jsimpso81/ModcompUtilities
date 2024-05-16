#pragma once


void process_user_commands();
void cmd_process_print_prompt();
void classic_7860_cpu();
void cmd_process_parse(char* cmd_line, int max_len, char* cmd_line_parse[], int max_items, int* count_found);
void init_iop_data();
void init_cpu_data();
void start_cpu_thread();
void stop_cpu_thread();


void  device_null_output_data(unsigned __int16 data_value);
void  device_null_output_cmd(unsigned __int16 cmd_value);
unsigned __int16  device_null_input_data();
unsigned __int16  device_null_input_status();


void cpu_set_register_value(unsigned __int16 reg_index, unsigned __int16 reg_value);
void cpu_set_program_counter(unsigned __int16 pc);




