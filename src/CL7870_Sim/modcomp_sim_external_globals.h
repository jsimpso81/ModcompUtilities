// =================================================================================
#pragma once

#include <windows.h>
#include <stdbool.h>

#include "modcomp_sim_types.h"

// -------- main memory
extern volatile unsigned __int16  gbl_mem[2097152];

// -------- items for front panel
extern volatile unsigned __int16 gbl_fp_switches;
extern volatile unsigned __int16 gbl_fp_datalights;
extern volatile unsigned __int16 gbl_fp_addrlights;
extern volatile unsigned __int16 gbl_regselectswitches;
extern volatile bool gbl_fp_powerlight;
extern volatile bool gbl_fp_standbylight;
extern volatile bool gbl_fp_backupfailure;
extern volatile bool gbl_fp_runlight;
extern volatile bool gbl_fp_cc_n_light;
extern volatile bool gbl_fp_cc_z_light ;
extern volatile bool gbl_fp_cc_o_light ;
extern volatile bool gbl_fp_cc_c_light ;
extern volatile bool gbl_fp_io_light ;
extern volatile bool gbl_fp_task_light ;
extern volatile bool gbl_fp_memerror_light ;
extern volatile bool gbl_fp_single_step ;

// -------- not done more front panel info


// -------- IO DEVICES
extern volatile unsigned __int16 iop_last_dev_status[64];

extern DEVICE_OUTPUT_DATA  iop_output_data_proc[64];
extern DEVICE_OUTPUT_CMD   iop_output_cmd_proc[64];
extern DEVICE_INPUT_DATA   iop_input_data_proc[64];
extern DEVICE_INPUT_STATUS iop_input_status_proc[64];

extern void* iop_device_buffer[64];
extern int iop_thread_stop_request[64];
extern int iop_thread_stop_request2[64];
extern HANDLE  iop_device_thread_handle[64];
extern HANDLE  iop_device_thread_handle2[64];
extern DWORD   iop_device_thread_id[64];
extern DWORD   iop_device_thread_id2[64];

extern bool gbl_verbose_debug;
// =================================================================================