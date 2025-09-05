// ModcompEmulDiskImg_ExtractPartition.c : This file contains the 'main' function. Program execution begins and ends there.
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
    __int64 sector_per_track = 96;
    __int64 geom = 28;
    bool dump_raw = false;

    /* -------- annouce our program  */
    printf("\nModcompEmulDiskImg_ExtractPartition - Extract partition file from a Modcomp Emulator disk image\n");


    /* -------- printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nModcompEmulDiskImg_ExtractPartition - Extract partition file from a Modcomp Emulator disk image\n\n");
                printf("        -h       print help message nad exit\n");
                printf("        -?       print help message nad exit\n");
                printf("        -i disk_image  dump the directory of this USL partition file\n");
                printf("        -p partition_file  dump the directory of this USL partition file\n");
                printf("        -s start_sector  starting sector 0 relative\n");
                printf("        -c sector_count  number of sectors\n");
                printf("        -t sector/track  sectors per track (96)\n");
                printf("        -g geom          geometry (28)\n");
                printf("        -r               raw (inc flag eof)\n");
                exit(0);
            }

            /* -------- dump as raw partition */
            else if (strcmp(argv[j], "-r") == 0) {
                dump_raw = true;
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
                    strncpy_s(partition_name, 1000, argv[j],strlen(argv[j]));
                    have_partition_name = true;
                }
            }

            /* -------- starting sector */
            else if (strcmp(argv[j], "-s") == 0) {
                j++;
                if (j < argc)
                    sscanf_s(argv[j], "%lld" , &starting_sector);
            }

            /* -------- sector count */
            else if (strcmp(argv[j], "-c") == 0) {
                j++;
                if (j < argc)
                    sscanf_s(argv[j], "%lld", &sector_count);
            }

            /* -------- sectors per track */
            else if (strcmp(argv[j], "-t") == 0) {
                j++;
                if (j < argc)
                    sscanf_s(argv[j], "%lld", &sector_per_track);
            }

            /* -------- geometry */
            else if (strcmp(argv[j], "-g") == 0) {
                j++;
                if (j < argc)
                    sscanf_s(argv[j], "%lld", &geom);
            }


            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }

        }
    }

    if (have_image_name && have_partition_name) {
        printf("\nExtracting partition %s from disk %s starting at %lld length %lld sectors.\n", partition_name, image_name, starting_sector, sector_count);

        extract_modcomp_emul_disk_partition(image_name, partition_name, starting_sector,
               sector_count, sector_per_track, geom, dump_raw );
    }
    else {
        printf("\n *** ERROR **** Not all required parameters provided.\n");
    }
    exit(0);
}
