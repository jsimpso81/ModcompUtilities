// TapeImg_Dump.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    int j;
    bool swap_bytes = false;

    /* -------- annouce our program  */
    printf("\nDiskFile_Dump - Dump disk file in readable format.\n");

    /* -------- parse command line */
    /*      -s                     */
    /*      -n                     */
    /*      -f file                */
    /*      -h                     */
    /*      -?                     */
    /*                             */


    /* printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\nTakeImg_Dump - Dump records from a tape image.\n\n");
                printf("        -h       print help message and exit\n");
                printf("        -?       print help message and exit\n");
                printf("        -f file  name of tape image dump\n");
                // printf("        -s       swap  bytes\n");
                // printf("        -n       dont swap bytes\n");
                exit(0);
            }

            /* -------- set swap bytes */
            else if (strcmp(argv[j], "-s") == 0) {
                swap_bytes = true;
            }

            /* -------- unset swap bytes */
            else if (strcmp(argv[j], "-n") == 0) {
                swap_bytes = false;
            }

            /* -------- set file name to dump */
            else if (strcmp(argv[j], "-f") == 0) {
                j++;
                if (j < argc)
                    TapeImg_dump_records(argv[j], swap_bytes);
            }

            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
    }
    exit(0);
}


// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
