// DiskFile_ExtractCharString.c : This file contains the 'main' function. Program execution begins and ends there.
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
    int start_sector = 0;
    int last_sector = 0;

    /* -------- annouce our program  */
    printf("\nDiskFile_ExtractCharString - Dump part of disk file as char/byte variable definition.\n");

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
                printf("\n\nUSLPart_DirectoryDump - List all directory entries for a USL formatted disk partition\n\n");
                printf("        -h       print help message and exit\n");
                printf("        -?       print help message and exit\n");
                printf("        -f file  name of file to dump\n");
                printf("        -s       swap  bytes\n");
                printf("        -n       dont swap bytes\n");
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
                if (j < argc) {
                    start_sector = 3;
                    last_sector = 31;
                    dump_file_as_byte_variable(argv[j], swap_bytes, start_sector, last_sector);
                }
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

