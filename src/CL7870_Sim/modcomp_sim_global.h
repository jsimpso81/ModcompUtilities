// =================================================================================
#pragma once

#include <windows.h>
#include <stdbool.h>

#include "modcomp_sim_types.h"

// -------- main memory
volatile unsigned __int16  gbl_mem[2097152];

// -------- items for front panel
volatile unsigned __int16 gbl_fp_switches = 0;
volatile unsigned __int16 gbl_fp_datalights = 0;
volatile unsigned __int16 gbl_fp_addrlights = 0;
volatile unsigned __int16 gbl_regselectswitches = 0;
volatile bool gbl_fp_powerlight = false;
volatile bool gbl_fp_standbylight = false;
volatile bool gbl_fp_backupfailure = false;
volatile bool gbl_fp_runlight = false;
volatile bool gbl_fp_cc_n_light = false;
volatile bool gbl_fp_cc_z_light = false;
volatile bool gbl_fp_cc_o_light = false;
volatile bool gbl_fp_cc_c_light = false;
volatile bool gbl_fp_io_light = false;
volatile bool gbl_fp_task_light = false;
volatile bool gbl_fp_memerror_light = false;
volatile bool gbl_fp_single_step = false;

// -------- not done more front panel info

// -------- CPU ITEMS (for debug)
volatile unsigned __int32 cpu_inst_used[256] = { 0 };

// -------- IO DEVICES
volatile unsigned __int16 iop_last_dev_status[64] = { 0 };

DEVICE_OUTPUT_DATA  iop_output_data_proc[64] = { 0 };
DEVICE_OUTPUT_CMD   iop_output_cmd_proc[64] = { 0 };
DEVICE_INPUT_DATA   iop_input_data_proc[64] = { 0 };
DEVICE_INPUT_STATUS iop_input_status_proc[64] = { 0 };

void* iop_device_buffer[64];
int iop_thread_stop_request[64];
int iop_thread_stop_request2[64];
HANDLE  iop_device_thread_handle[64];
HANDLE  iop_device_thread_handle2[64];
DWORD   iop_device_thread_id[64];
DWORD   iop_device_thread_id2[64];


// -------- execution options
bool gbl_verbose_debug = false;


// =================================================================================