// PDP10_File_ToText.c : This file contains the 'main' function. Program execution begins and ends there.
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
    fprintf(stderr, "PDP10_File_ToText - Convert a file to ascii text file.\n");

    /* fprintf(stderr, "\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* fprintf(stderr, "     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                fprintf(stderr, "\nPDP10_File_ToText - Convert a file to ascii text file.\n\n");
                fprintf(stderr, "        -h       print help message and exit\n");
                fprintf(stderr, "        -?       print help message and exit\n");
                fprintf(stderr, "        -f file  filename of 7 bit text file\n");
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
                    fprintf(stderr, "\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
        // -------- if we have a file name, then dump it...
        if (have_file_name) {
            PDP10_file_to_text(filename);
        }
    }
    exit(0);
}
