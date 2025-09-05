// ModcompEmulDiskImg_UpdatePartition.c : This file contains the 'main' function. Program execution begins and ends there.
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
    char  partition_name[1000];
    __int64 starting_sector = 0;
    __int64 sector_count = 1;
    bool have_image_name = false;
    bool have_partition_name = false;
    bool update_raw = false;

    /* -------- annouce our program  */
    printf("\nModcompEmulDiskImg_UpdatePartition - Update partition file on a Modcomp Emulator disk image\n");


    /* -------- printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nModcompEmulDiskImg_UpdatePartition - Update partition file on a Modcomp Emulator disk image\n\n");
                printf("        -h       print help message nad exit\n");
                printf("        -?       print help message nad exit\n");
                printf("        -i disk_image  modcomp emulator disk image file\n");
                printf("        -p partition_file  partition file\n");
                printf("        -s start_sector  starting sector 0 relative\n");
                printf("        -c sector_count  number of sectors\n");
                printf("        -r       raw (partition includes flags)\n");
                exit(0);
            }

            /* -------- update raw */
            else if (strcmp(argv[j], "-r") == 0) {
                update_raw = true;
            }

            /* -------- disk image file name */
            else if (strcmp(argv[j], "-i") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(image_name, 1000, argv[j], strlen(argv[j]));
                    have_image_name = true;
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

            /* -------- starting sector */
            else if (strcmp(argv[j], "-s") == 0) {
                j++;
                if (j < argc)
                    sscanf_s(argv[j], "%lld", &starting_sector);
            }

            /* -------- sector count */
            else if (strcmp(argv[j], "-c") == 0) {
                j++;
                if (j < argc)
                    sscanf_s(argv[j], "%lld", &sector_count);
            }

            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }

        }
    }

    if (have_image_name && have_partition_name) {
        printf("\nUpdating partition %s to disk %s starting at %lld length %lld sectors.\n", partition_name, image_name, starting_sector, sector_count);

        update_modcomp_emul_disk_partition(image_name, partition_name, 
            starting_sector, sector_count, update_raw);
    }
    else {
        printf("\n *** ERROR **** Not all required parameters provided.\n");
    }
    exit(0);
}
