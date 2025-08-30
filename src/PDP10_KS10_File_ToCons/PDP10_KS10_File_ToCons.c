// PDP10_File_ToCons.c : This file contains the 'main' function. Program execution begins and ends there.
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
    char consport[1000] = { 'C', 'O', 'M', '1', ':', 0, 0};

    /* -------- annouce our program  */
    fprintf(stderr, "PDP10_File_ToCons - Copy contents of a file to console.\n");

    /* fprintf(stderr, "\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* fprintf(stderr, "     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                fprintf(stderr, "\nPDP10_File_ToCons - Copy contents of a file to console.\n");
                fprintf(stderr, "\n");
                fprintf(stderr, "\n Copy each line to console after KS10 prompt is displayed.");
                fprintf(stderr, "\n Lines starting with semi-colon are not copied.");
                fprintf(stderr, "\n");
                fprintf(stderr, "        -h       print help message and exit\n");
                fprintf(stderr, "        -?       print help message and exit\n");
                fprintf(stderr, "        -f file  filename of file to copy to console.\n");
                fprintf(stderr, "        -p port  console port device name <COM1>.\n");
                exit(0);
            }

            /* -------- set file name to use */
            else if (strcmp(argv[j], "-f") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(filename, (rsize_t)1000, argv[j], 1000);
                    have_file_name = true;
                }
            }


            /* -------- set com port */
            else if (strcmp(argv[j], "-p") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(consport, (rsize_t)1000, argv[j], 1000);
                }
            }


            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    fprintf(stderr, "\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
        // -------- if we have a file name, then dump it...
        if (have_file_name) {
            PDP10_ks10_file_to_cons(filename, consport);
        }
    }
    exit(0);
}
