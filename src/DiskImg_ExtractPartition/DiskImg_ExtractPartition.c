// DiskImg_ExtractPartition.c : This file contains the 'main' function. Program execution begins and ends there.
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

    /* -------- annouce our program  */
    printf("\nDiskImg_ExtractPartition - Extract partition file from a Modcomp disk image\n");

    /* -------- parse command line */
    /*      -i file                */
    /*      -p file                */
    /*      -s starting sector     */
    /*      -c number of sectors   */
    /*      -h                     */
    /*      -?                     */
    /*                             */


    /* -------- printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nUSLPart_DirectoryDump - List all directory entries for a USL formatted disk partition\n\n");
                printf("        -h       print help message nad exit\n");
                printf("        -?       print help message nad exit\n");
                printf("        -i disk_image  dump the directory of this USL partition file\n");
                printf("        -p partition_file  dump the directory of this USL partition file\n");
                printf("        -s start_sector  starting sector 0 relative\n");
                printf("        -c sector_count  number of sectors\n");
                exit(0);
            }

            /* -------- disk image file name */
            else if (strcmp(argv[j], "-i") == 0) {
                j++;
                if (j < argc) 
                    strncpy_s( image_name, 1000, argv[j], strlen(argv[j]));
            }

            /* -------- partition file name */
            else if (strcmp(argv[j], "-p") == 0) {
                j++;
                if (j < argc)
                    strncpy_s(partition_name, 1000, argv[j],strlen(argv[j]));
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
        }
    }

    if (have_image_name && have_partition_name) {
        printf("\nExtracting partition %s from disk %s starting at %lld length %lld sectors.\n", partition_name, image_name, starting_sector, sector_count);

        extract_partition(image_name, partition_name, starting_sector, sector_count);
    }
    else {
        printf("\n *** ERROR **** Not all required parameters provided.\n");
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
