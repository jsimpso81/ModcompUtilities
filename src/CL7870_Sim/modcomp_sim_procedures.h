#pragma once


void process_user_commands();
void cmd_process_print_prompt();
void classic_7860_cpu();
void cmd_process_parse(char* cmd_line, int max_len, char* cmd_line_parse[], int max_items, int* count_found);
void start_cpu_thread();
void stop_cpu_thread();




