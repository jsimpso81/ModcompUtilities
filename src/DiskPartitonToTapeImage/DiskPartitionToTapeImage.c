// DiskPartitionToTapeImage.c : This file contains the 'main' function. Program execution begins and ends there.
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h> 

#include "../modcomp_utility_library/modcomp_utility_library.h"

/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    int j;
    char  tape_image_name[1000];
    char  partition_name[1000];
    bool swap_bytes = false;
    __int64 starting_sector = 0;
    __int64 starting_track = 0;
    __int64 sector_count = 1;
    __int64 unit = 0;
    bool have_tape_image_name = false;
    bool have_partition_name = false;
    bool have_starting_sector = false;


    /* -------- annouce our program  */
    printf("\nDiskPartitionToTapeImage - Copy the contents of a disk partition image to tape image\n");


    /* -------- printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nDiskPartitionToTapeImage - Copy the contents of a disk partition image to tape image\n\n");
                printf("        -h       print help message nad exit\n");
                printf("        -?       print help message nad exit\n");
                printf("        -p partition_file  partition file\n");
                printf("        -s swap bytes\n");
                printf("        -t tape_image  standard emulator tape file\n");
                exit(0);
            }

            // -------- swap bytes
            else if (strcmp(argv[j], "-s") == 0 ) {
                swap_bytes = true;
            }
            /* -------- tape image file name */
            else if (strcmp(argv[j], "-t") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(tape_image_name, 1000, argv[j], strlen(argv[j]));
                    have_tape_image_name = true;
                }
            }

            /* -------- partition file name */
            else if (strcmp(argv[j], "-p") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(partition_name, 1000, argv[j], strlen(argv[j]));
                    have_partition_name = true;
                }
            }

            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }

        }
    }

    if (have_tape_image_name && have_partition_name ) {
        printf("\nCopying partition %s to tape image %s \n", partition_name, tape_image_name);

        copy_disk_partition_to_tape_image(partition_name, tape_image_name, swap_bytes );
    }
    else {
        printf("\n *** ERROR **** Not all required parameters provided.\n");
    }
    exit(0);
}
