#pragma once

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




