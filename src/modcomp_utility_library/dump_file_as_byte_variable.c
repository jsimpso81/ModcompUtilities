#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

#define MAX(i, j) (((i) > (j)) ? (i) : (j))
#define MIN(i, j) (((i) < (j)) ? (i) : (j))

/* ========================================================================================================================*/
void dump_file_as_byte_variable(char* filename, bool swap_bytes, int start_sector, int end_sector) {

    FILE* inpart;
    __int64 sector;

    unsigned _int16 sector_buffer[128];

    int stat;
    errno_t status;
    bool not_done = true;
    int j;
    __int64 word_index;
    unsigned char chars[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    unsigned char cancode1[4] = { 0, 0, 0, 0 };
    unsigned char cancode2[4] = { 0, 0, 0, 0 };
    unsigned char cancode3[4] = { 0, 0, 0, 0 };
    unsigned char cancode4[4] = { 0, 0, 0, 0 };


    /* -------- open input disk image */
    status = fopen_s(&inpart, filename, "rb");

    if (status != 0) {
        printf("\nError - could not open file -- %s\n\n", filename);
    }

    else {

        printf("Dumping file %s\n", filename);

        /* -------- set starting sector and number of sectors */
        sector = 0;
        not_done = true;
        word_index = 0;
        size_t return_count;
        int end_of_file;
        char temp_string[10] = { 0 };

        printf(" // -------- start of byte data \n");

        while (not_done) {

            /* -------- read next directory sector, parse and print */
            stat = read_sector_lba(inpart, sector, 1, &sector_buffer, &return_count, &end_of_file);

            printf(" // -- read sector %lld status %d count read %zd end of file %d\n", sector, stat, return_count, end_of_file);

            if (return_count > 0) {


                if (sector >= start_sector && sector <= end_sector) {

                    if (swap_bytes) {
                        for (j = 0; j < 128; j++) {
                            sector_buffer[j] = bswap16(sector_buffer[j]);
                        }
                    }

                    dump_sector_as_byte_variable(sector_buffer);
                }
                sector++;
            }

            else {
                not_done = false;
            }

            if (stat != 0)
                not_done = false;
    }

        fclose(inpart);

        printf("\nDone.\n");

    }
}
