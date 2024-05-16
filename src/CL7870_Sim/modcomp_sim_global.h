#pragma once

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

// PROC_STATUS_DOUBLEWORD proc_stat;


	

