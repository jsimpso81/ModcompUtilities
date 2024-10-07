
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

#define MAX(i, j) (((i) > (j)) ? (i) : (j))
#define MIN(i, j) (((i) < (j)) ? (i) : (j))

/* ========================================================================================================================*/
void dump_raw_disk_sector_as_byte_variable(unsigned __int16 sector_buffer[]) {

    int start_word;

    unsigned __int8 our_byte[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };


    for (start_word = 0; start_word < 128; start_word += 4) {

        /* -------- ascii xx, xx, xx, xx, xx, xx, xx, xx | xxxxxx, xxxxxx, xxxxxx, xxxxxx | 0xxxxx 0xxxxx 0xxxxx 0xxxxx | can can can can */
        /* -------- allow only printable characters */
        our_byte[1] = sector_buffer[start_word] & 0x00ff;
        our_byte[0] = (sector_buffer[start_word] >> 8) & 0x00ff;
        our_byte[3] = sector_buffer[start_word + 1] & 0x00ff;
        our_byte[2] = (sector_buffer[start_word + 1] >> 8) & 0x00ff;
        our_byte[5] = sector_buffer[start_word + 2] & 0x00ff;
        our_byte[4] = (sector_buffer[start_word + 2] >> 8) & 0x00ff;
        our_byte[7] = sector_buffer[start_word + 3] & 0x00ff;
        our_byte[6] = (sector_buffer[start_word + 3] >> 8) & 0x00ff;


        printf("   0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X, 0x%02X,   \n",
            our_byte[0], our_byte[1], our_byte[2], our_byte[3], our_byte[4], our_byte[5], our_byte[6], our_byte[7]
        );
    }
}
