#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

#define MAX(i, j) (((i) > (j)) ? (i) : (j))
#define MIN(i, j) (((i) < (j)) ? (i) : (j))

/* ========================================================================================================================*/
void dump_file(char* filename, bool swap_bytes) {

    FILE* inpart;
    __int64 sector;

    unsigned _int16 sector_buffer[128];

    int stat;
    errno_t status;
    bool not_done = true;
    int start_word;
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

        while (not_done) {

            /* -------- read next directory sector, parse and print */
            stat = read_sector_lba(inpart, sector, 1, &sector_buffer, &return_count, &end_of_file);

            printf(" read sector %lld status %d count read %zd end of file %d\n", sector, stat, return_count, end_of_file);

            if ( return_count > 0 ) {

                if (swap_bytes) {
                    for (j = 0; j < 128; j++) {
                        sector_buffer[j] = bswap16(sector_buffer[j]);
                    }
                }



                for (start_word = 0; start_word < 128; start_word += 4) {

                    /* -------- ascii xx, xx, xx, xx, xx, xx, xx, xx | xxxxxx, xxxxxx, xxxxxx, xxxxxx | 0xxxxx 0xxxxx 0xxxxx 0xxxxx | can can can can */
                    /* -------- allow only printable characters */
                    chars[1] =  MAX(sector_buffer[start_word] & 0x007f, 32);
                    chars[0] =  MAX((sector_buffer[start_word] >> 8) & 0x007f, 32);
                    chars[3] =  MAX(sector_buffer[start_word + 1] & 0x007f, 32);
                    chars[2] =  MAX((sector_buffer[start_word + 1] >> 8) & 0x007f, 32);
                    chars[5] =  MAX(sector_buffer[start_word + 2] & 0x007f, 32);
                    chars[4] =  MAX((sector_buffer[start_word + 2] >> 8) & 0x007f, 32);
                    chars[7] =  MAX(sector_buffer[start_word + 3] & 0x007f, 32);
                    chars[6] =  MAX((sector_buffer[start_word + 3] >> 8) & 0x007f, 32);

                    strcpy_s(cancode1, 4, from_can_code(sector_buffer[start_word], temp_string));
                    strcpy_s(cancode2, 4, from_can_code(sector_buffer[start_word + 1], temp_string));
                    strcpy_s(cancode3, 4, from_can_code(sector_buffer[start_word + 2], temp_string));
                    strcpy_s(cancode4, 4, from_can_code(sector_buffer[start_word + 3], temp_string));

                    printf(" %8lld  | %08s | %6d %6d %6d %6d | 0x%04X 0x%04X 0x%04X 0x%04X | %3s %3s %3s %3s | \n",
                        word_index, chars,
                        sector_buffer[start_word], sector_buffer[start_word + 1], sector_buffer[start_word + 2], sector_buffer[start_word + 3],
                        sector_buffer[start_word], sector_buffer[start_word + 1], sector_buffer[start_word + 2], sector_buffer[start_word + 3],
                        cancode1, cancode2, cancode3, cancode4
                    );
                    word_index += 4;

                    if (stat != 0)
                        not_done = false;

                }
                sector++;
            }

            else
            {
                not_done = false;
            }
        }

        fclose(inpart);

        printf("\nDone.\n");

    }
}
