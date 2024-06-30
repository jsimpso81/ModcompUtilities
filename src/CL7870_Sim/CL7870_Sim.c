// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			CL7870_Sim.c
//
//	Description:	Main program for Modcomp Classic computer simulator.   This program
//                  can be compiled to simulate Classic 7860, 7830, II/15 cpus, with or
//                  without EAU floating point instructions.
//
//	Externally accessible routines:
//                  int main(int argc, char* argv[]);
// 
// Notes:			
//
// ================================================================================================
//	Revision history:
//		6/20/2024	JAS		Added header
// ================================================================================================
//

#define SIMJ_MAIN true

#include "simj_base.h"

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    bool done = false;
    int j;
    bool have_config_file = false;
    char config_file_name[500];


    /* -------- annouce our program  */
#if  SIMJ_SIM_CPU == 7860
    printf("\nCL7860_Sim - Modcomp classic 7860/7870 simulator.\n");
#elif  SIMJ_SIM_CPU == 7830
    printf("\nCL7830_Sim - Modcomp classic 7830 simulator.\n");
#elif  SIMJ_SIM_CPU == II15
    printf("\nCLII/15_Sim - Modcomp classic II/15 simulator.\n");
#else
#error SIMJ_SIM_CPU Had invalid cpu model.
#endif

    /* printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                user_cmd_print_help();
                exit(0);
            }

            /* -------- verbose debug messges */
            if (strcmp(argv[j], "-v") == 0 ) {
                gbl_verbose_debug = true;
            }

            /* -------- configuration file. */
            if (strcmp(argv[j], "-c") == 0) {

                // --------there should be another argument that is the file name.
                j++;
                if (j < argc) {
                    // --------store config file name, set flag indicating there is a config file.
                    strcpy_s(&config_file_name[0], sizeof(config_file_name), argv[j]);
                    have_config_file = true;
                }
                else {
                    printf(" *** ERROR ***  Configuration file command line parameer -c must be followed with the confuration filename.\n");
                }
 
            }

            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
    }

    // --------initialize data
    memory_plane_init();
    cpu_init_data();
    iop_init_data();
    //fp_init_data(); // front panel

    // -------- IOP start threads
    // TODO: Add IOP threads.

    // -------- start CPU thread.
    cpu_start_thread();

    // -------- start the clock
    rtclock_start_thread();

    // --------wait a little for things to start
    Sleep(500);

    // --------if an initial configuration file exists, read it and process the commands.
    if (have_config_file) {
        user_cmd_config_execute(&config_file_name[0]);
    }

    // -------- process user commands.   it returns when the exit command is given.
    process_user_commands(stdin);

    rtclock_stop_thread();
    cpu_stop_thread();
    device_common_stop_all();
    device_common_capture_console_close();

    printf("\nCL7860_sim -- terminating.\n");
    exit(0);
}

