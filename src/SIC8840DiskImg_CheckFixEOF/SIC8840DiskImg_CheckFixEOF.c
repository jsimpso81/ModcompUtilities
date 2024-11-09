// SIC8840DiskImg_CheckFixEOF.c : This file contains the 'main' function. Program execution begins and ends there.
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    int j;
    char  image_name[1000];
    bool have_image_name = false;
    __int64 unit = 0;
    bool fix = false;
    bool list = false;

    /* -------- annouce our program  */
    printf("\nSIC8840DiskImg_CheckFixEOF - Check and optionally fix EOF buffer for disk unit.\n");


    /* -------- printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nSIC8840DiskImg_CheckFixEOF - Check and optionally fix EOF buffer for disk unit.\n");
                printf("        -h       print help message nad exit\n");
                printf("        -?       print help message nad exit\n");
                printf("        -i disk_image  modcomp emulator disk image file\n");
                printf("        -u unit  disc unit number (0)\n");
                printf("        -f       fix EOF buffer\n");
                printf("        -l       list EOF sectors\n");
                exit(0);
            }

            /* -------- disk image file name */
            else if (strcmp(argv[j], "-i") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(image_name, 1000, argv[j], strlen(argv[j]));
                    have_image_name = true;
                }
            }

            /* -------- unit number */
            else if (strcmp(argv[j], "-u") == 0) {
                j++;
                if (j < argc)
                    sscanf_s(argv[j], "%lld", &unit);
            }

            /* -------- fix */
            else if (strcmp(argv[j], "-f") == 0) {
                fix = true;
            }

            /* -------- list */
            else if (strcmp(argv[j], "-l") == 0) {
                list = true;
            }

            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }

        }
    }

    if (have_image_name ) {
        // -------- do EOF buffer dump
        if (unit == 4) {
            printf("\nCheck disk image %s EOF Buffer .\n", image_name);
            sic_884x_disk_image_dump_eof_buffer(image_name);
            printf("\n\nDone\n");
        }
        // -------- do regular dump for units 0 - 3
        else {
            printf("\nCheck disk image %s disk unit %lld  %s.\n",
                image_name, unit, (fix ? " - check and fix" : " - check only"));

            sic_884x_disk_image_check_fix_eof(image_name, unit, fix, list);
            printf("\n\nDone\n");
        }
    }
    else {
        printf("\n *** ERROR **** Not all required parameters provided.\n");
    }
    exit(0);
}
