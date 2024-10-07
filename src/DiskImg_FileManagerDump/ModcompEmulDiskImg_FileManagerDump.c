// ModcompEmulDiskImg_FileManagerDump.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    int j;
    char  image_name[1000] = "";

    FILE* inimg;
    __int64 sector;
    unsigned __int16 sector_buffer[128];
    int stat;
    errno_t status;
    unsigned __int64 next_dpi_1;
    unsigned __int64 next_dpi_2;
    unsigned __int64 next_dpi_3;
    unsigned __int64 next_dpi;

    unsigned __int64 voldir_dpi_1;
    unsigned __int64 voldir_dpi_2;
    unsigned __int64 voldir_dpi;



    /* -------- annouce our program  */
    printf("\nModcompEmulDiskImg_FileManagerDump - Dump File Manager definition file from a Modcomp Emulator disk image\n");

    /* -------- printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\n\nModcompEmulDiskImg_FileManagerDump - Dump File Manager definition file from a Modcomp Emulator disk image\n\n");
                printf("        -h       print help message nad exit\n");
                printf("        -?       print help message nad exit\n");
                printf("        -i disk_image  Modcomp Emulator disk image file \n");
                exit(0);
            }

            /* -------- disk image file name */
            else if (strcmp(argv[j], "-i") == 0) {
                j++;
                if (j < argc)
                    strncpy_s(image_name, 1000, argv[j], strlen(argv[j]));
            }

            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }

        }
    }


    /* -------- open disk image file */
    status = fopen_s(&inimg, image_name, "rb");

    if (status != 0) {
        printf("\n *** ERROR *** - Trouble opening image file - %s  Error status: %d\n\n", image_name, status);
        exit(0);
    }

    /* -------- read volume header pointer.  */
    sector = 0;
    stat = read_modcomp_emul_disk_sector_lba(inimg, sector, sector_buffer);

    printf("\n --------volume header pointer--------\n");

    next_dpi_1 = (sector_buffer[9] >> 8) & 0x00ff;
    next_dpi_2 = (sector_buffer[10] >> 8) & 0x00ff;
    next_dpi_3 = (sector_buffer[11] >> 8) & 0x00ff;
    next_dpi = (next_dpi_1 << 16) | (next_dpi_2 << 8) | next_dpi_3;
    printf("    volume header label dpi %lld\n", next_dpi);

    dump_raw_disk_sector(sector_buffer);

    /* -------- read volume header */
    sector = next_dpi;
    stat = read_modcomp_emul_disk_sector_lba(inimg, sector, sector_buffer);

    printf("\n --------volume header label--------\n");

    voldir_dpi_1 = sector_buffer[1] & 0x00ff;
    voldir_dpi_2 = sector_buffer[2];
    voldir_dpi = (voldir_dpi_1 << 16) | voldir_dpi_2;

    printf("    volume directory dpi %lld\n", voldir_dpi);

    dump_raw_disk_sector(sector_buffer);

    while (voldir_dpi != 0) {

        /* -------- read volume directory segment */
        sector = voldir_dpi;
        stat = read_modcomp_emul_disk_sector_lba(inimg, sector, sector_buffer);

        printf("\n --------volume directory segment--------\n");

        voldir_dpi_1 = sector_buffer[1] & 0x00ff;
        voldir_dpi_2 = sector_buffer[2];
        voldir_dpi = (voldir_dpi_1 << 16) | voldir_dpi_2;

        printf("    next volume directory dpi %lld\n", voldir_dpi);

        dump_raw_disk_sector(sector_buffer);

    }


    /* -------- close volume */
    fclose(inimg);


    exit(0);
}
