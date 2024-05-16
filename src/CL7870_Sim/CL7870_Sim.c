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
    // pthread_t cpu_thread_id;

    init_cpu_data();
    init_iop_data();

    /* -------- annouce our program  */
    printf("\nCL7870_Sim - Modcomp classic 7870 simulator.\n");

    // -------- commands
    //      mc - master clear
    //      help - get a help message, show command list
    //      set switches <value>
    //      set regdisplay <value>
    //      show switches
    //      show pc  - program counter
    //      show ps  - processor status
    //      fill - issue a fill command
    //      run - run cpu
    //      step - single step cpu
    //      halt = halt cpu
    //      device bus dev_addr pri dmp model/type file
    //                          types:  console  - cons <comx or tcp port>
    //                                  mag tape --  mt <unit> <tape file name>
    //                                  disk --  lx <unit> <disk file img>
    //                                  async -- as <unit> <comx or tcp port>
    //


    /* printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nCL7870_sim - Simulate a classic 7870 CPU \n\n");
                printf("        -h       print help message and exit\n");
                printf("        -?       print help message and exit\n");
                exit(0);
            }


            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
    }

    // -------- start CPU thread.
    start_cpu_thread();

    // -------- process user commands.   it returns when the exit command is given.
    process_user_commands();

    stop_cpu_thread();

    printf("\nCL7860_sim -- terminating.\n");
    exit(0);
}

