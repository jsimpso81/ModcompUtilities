#pragma once

typedef struct {
	unsigned __int16 pc;		// program counter
	unsigned __int16 ps;		// status word
} PROC_STATUS_DOUBLEWORD;


typedef void (*DEVICE_OUTPUT_DATA)(unsigned __int16 device_address, unsigned __int16 data_word);
typedef void (*DEVICE_OUTPUT_CMD)(unsigned __int16 device_address, unsigned __int16 cmd_word);
typedef unsigned __int16 (*DEVICE_INPUT_STATUS)(unsigned __int16 device_address );
typedef unsigned __int16 (*DEVICE_INPUT_DATA)(unsigned __int16 device_address );

typedef struct {
	unsigned __int16 ctrl_status;
	unsigned __int16 ctrl_command;
	unsigned __int16 ctrl_input_buffer_count;
	unsigned int ctrl_input_buffer_index;
	unsigned __int8 ctrl_input_buffer[8000];
	unsigned __int16 ctrl_output_buffer_count;
	unsigned __int16 ctrl_output_buffer_index;
	unsigned __int8 ctrl_output_buffer[8000];
} DEVICE_CONSOLE_DATA;


typedef struct {
	unsigned __int16 ctrl_status;
	unsigned __int16 ctrl_command;
} DEVICE_NULL_DATA;

