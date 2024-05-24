// CL7870_Sim.cpp : This file contains the 'main' function. Program execution begins and ends there.
//



#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
// #include <pthread.h>

#include "modcomp_sim_global.h"
#include "modcomp_sim_procedures.h"

/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    bool done = false;
    int j;


    /* -------- annouce our program  */
    printf("\nCL7870_Sim - Modcomp classic 7870 simulator.\n");

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


            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
    }

    // --------initialize data
    cpu_init_data();
    iop_init_data();
    //fp_init_data();

    // -------- start CPU thread.
    cpu_start_thread();

    // --------wait a little for things to start
    Sleep(2000);

    // -------- process user commands.   it returns when the exit command is given.
    process_user_commands();

    cpu_stop_thread();
    device_common_stop_all();

    printf("\nCL7860_sim -- terminating.\n");
    exit(0);
}

