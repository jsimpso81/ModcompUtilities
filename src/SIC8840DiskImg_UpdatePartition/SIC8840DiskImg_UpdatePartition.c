// SIC8840DiskImg_UpdatePartition.c : This file contains the 'main' function. Program execution begins and ends there.
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
    __int64 starting_track = 0;
    __int64 sector_count = 1;
    __int64 unit = 0;
    bool have_image_name = false;
    bool have_partition_name = false;
    bool have_starting_sector = false;


    /* -------- annouce our program  */
    printf("\nSIC8840DiskImg_UpdatePartition - Update partition file on a SIC8840 disk image\n");


    /* -------- printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nSIC8840DiskImg_UpdatePartition - Update partition file on a SIC8840 disk image\n\n");
                printf("        -h       print help message nad exit\n");
                printf("        -?       print help message nad exit\n");
                printf("        -i disk_image  modcomp emulator disk image file\n");
                printf("        -p partition_file  partition file\n");
                printf("        -s start_sector  starting sector 0 relative\n");
                printf("        -t start_track   starting track 0 relative\n");
                printf("        -c sector_count  number of sectors\n");
                printf("        -u drive_unit    drive unit 0-3 (0)\n");
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
                if (j < argc) {
                    sscanf_s(argv[j], "%lld", &starting_sector);
                    have_starting_sector = true;
                }
            }

            /* -------- starting track */
            else if (strcmp(argv[j], "-t") == 0) {
                j++;
                if (j < argc) {
                    sscanf_s(argv[j], "%lld", &starting_track);
                    starting_sector = starting_track * SIC_8840_SEC_PER_TRK;
                    have_starting_sector = true;
                }
            }

            /* -------- unit number */
            else if (strcmp(argv[j], "-u") == 0) {
                j++;
                if (j < argc) {
                    sscanf_s(argv[j], "%lld", &unit);
                }
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

    if (have_image_name && have_partition_name && have_starting_sector) {
        printf("\nUpdating partition %s to disk %s starting at %lld length %lld sectors.\n", partition_name, image_name, starting_sector, sector_count);

        update_sic_884x_disk_partition(image_name, partition_name, starting_sector, sector_count, unit );
    }
    else {
        printf("\n *** ERROR **** Not all required parameters provided.\n");
    }
    exit(0);
}
