// DiskPart_MakeEmpty.c : This file contains the 'main' function. Program execution begins and ends there.
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    __int64 j;
    char  partition_name[1000];
    __int64 sector_count = 1;
    bool have_partition_name = false;

    FILE* outpart;
    unsigned _int16 raw_sector_buffer[128] = { 0 };
    errno_t status;




    /* -------- annouce our program  */
    printf("\nDiskPart_MakeEmpty -- Create empty disk partition\n");


    /* -------- printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nDiskPart_MakeEmpty -- Create empty disk partition\n\n");
                printf("        -h       print help message nad exit\n");
                printf("        -?       print help message nad exit\n");
                printf("        -p partition_file  dump the directory of this USL partition file\n");
                printf("        -c sector_count  number of sectors\n");
                exit(0);
            }

            /* -------- partition file name */
            else if (strcmp(argv[j], "-p") == 0) {
                j++;
                if (j < argc)
                    strncpy_s(partition_name, 1000, argv[j], strlen(argv[j]));
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

    if (have_partition_name) {
        printf("\nCreating empty partition %s length %lld sectors.\n", partition_name, sector_count);

        /* -------- open output partition image */
        status = fopen_s(&outpart, partition_name, "wb");

        if (status == 0) {

            /* -------- zero buffer */
            for (j = 0; j < 128; j++) {
                raw_sector_buffer[j] = 0;
            }


            /* -------- loop over all sectors, reading and writing */
            for (j = 0; j < sector_count; j++) {
                fwrite(raw_sector_buffer, (size_t)256, (size_t)1, outpart);
            }

            fclose(outpart);

            printf("\n Done.\n");
        }
    }
    else {
        printf("\n *** ERROR **** Not all required parameters provided.\n");
    }
    exit(0);
}