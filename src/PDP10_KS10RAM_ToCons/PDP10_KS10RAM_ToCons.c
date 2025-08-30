// PDP10_KS10RAM_ToCONS.c : This file contains the 'main' function. Program execution begins and ends there.
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
    bool have_file_name = false;
    char filename[1000] = { 0 };

    /* -------- annouce our program  */
    printf("\nPDP10_KS10RAM_ToCONS - Convert KS10 RAM Microcode file to console load commands.\n");

    /* printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\nPDP10_KS10RAM_ToCONS - Convert KS10 RAM Microcode file to console load commands.\n\n");
                printf("        -h       print help message and exit\n");
                printf("        -?       print help message and exit\n");
                printf("        -f file  filename of KS10 microcode RAM file\n");
                exit(0);
            }

            /* -------- set file name to dump */
            else if (strcmp(argv[j], "-f") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(filename, (rsize_t)1000, argv[j], 1000);
                    have_file_name = true;
                }
            }

            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
        // -------- if we have a file name, then dump it...
        if (have_file_name) {
            PDP10_ks10ram_to_cons(filename);
        }
    }
    exit(0);
}
