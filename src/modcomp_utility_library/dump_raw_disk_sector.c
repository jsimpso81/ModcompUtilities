
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

#define MAX(i, j) (((i) > (j)) ? (i) : (j))
#define MIN(i, j) (((i) < (j)) ? (i) : (j))

/* ========================================================================================================================*/
void dump_raw_disk_sector(unsigned __int16* sector_buffer) {

    int start_word;

    unsigned char chars[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    unsigned char cancode1[4] = { 0, 0, 0, 0 };
    unsigned char cancode2[4] = { 0, 0, 0, 0 };
    unsigned char cancode3[4] = { 0, 0, 0, 0 };
    unsigned char cancode4[4] = { 0, 0, 0, 0 };
    unsigned char temp_string[10] = { 0 };


    for (start_word = 0; start_word < 128; start_word += 4) {

        /* -------- ascii xx, xx, xx, xx, xx, xx, xx, xx | xxxxxx, xxxxxx, xxxxxx, xxxxxx | 0xxxxx 0xxxxx 0xxxxx 0xxxxx | can can can can */
        /* -------- allow only printable characters */
        chars[1] = MAX(sector_buffer[start_word] & 0x007f, 32);
        chars[0] = MAX((sector_buffer[start_word] >> 8) & 0x007f, 32);
        chars[3] = MAX(sector_buffer[start_word + 1] & 0x007f, 32);
        chars[2] = MAX((sector_buffer[start_word + 1] >> 8) & 0x007f, 32);
        chars[5] = MAX(sector_buffer[start_word + 2] & 0x007f, 32);
        chars[4] = MAX((sector_buffer[start_word + 2] >> 8) & 0x007f, 32);
        chars[7] = MAX(sector_buffer[start_word + 3] & 0x007f, 32);
        chars[6] = MAX((sector_buffer[start_word + 3] >> 8) & 0x007f, 32);

        from_can_code(sector_buffer[start_word], temp_string);
        strcpy_s(cancode1, 4, temp_string);
        from_can_code(sector_buffer[start_word + 1], temp_string);
        strcpy_s(cancode2, 4, temp_string);
        from_can_code(sector_buffer[start_word + 2], temp_string);
        strcpy_s(cancode3, 4, temp_string);
        from_can_code(sector_buffer[start_word + 3], temp_string);
        strcpy_s(cancode4, 4, temp_string);

        printf("   %6d  | %08s | %6d %6d %6d %6d | 0x%04X 0x%04X 0x%04X 0x%04X | %3s %3s %3s %3s | \n",
                        start_word, chars,
                        sector_buffer[start_word], sector_buffer[start_word + 1], sector_buffer[start_word + 2], sector_buffer[start_word + 3],
                        sector_buffer[start_word], sector_buffer[start_word + 1], sector_buffer[start_word + 2], sector_buffer[start_word + 3],
                        cancode1, cancode2, cancode3, cancode4
        );
    }
}
