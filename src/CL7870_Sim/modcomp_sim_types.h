#pragma once

typedef
struct {
	unsigned __int16 pc;		// program counter
	unsigned __int16 ps;		// status word
} PROC_STATUS_DOUBLEWORD;


typedef void (*DEVICE_OUTPUT_DATA)(unsigned __int16 data_word);
typedef void (*DEVICE_OUTPUT_CMD)(unsigned __int16 cmd_word);
typedef unsigned __int16 (*DEVICE_INPUT_STATUS)(void);
typedef unsigned __int16 (*DEVICE_INPUT_DATA)(void);
