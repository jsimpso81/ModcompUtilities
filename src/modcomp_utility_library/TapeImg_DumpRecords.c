#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

#define MAX(i, j) (((i) > (j)) ? (i) : (j))
#define MIN(i, j) (((i) < (j)) ? (i) : (j))

/* ========================================================================================================================*/
void TapeImg_dump_records(char* filename, bool swap_bytes) {

    FILE* inpart;
    __int64 current_file_pos = 0;
    size_t bytes_read = 0;
    size_t words_read = 0;

    union {
        unsigned __int8 ubytes[65536];
        unsigned __int16 words[32768];
    } tape_buffer;

    int stat;
    errno_t status;
    bool not_done = true;
    size_t start_word;
    size_t j;
    __int64 word_index;

    unsigned char chars[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    unsigned char cancode1[4] = { 0, 0, 0, 0 };
    unsigned char cancode2[4] = { 0, 0, 0, 0 };
    unsigned char cancode3[4] = { 0, 0, 0, 0 };
    unsigned char cancode4[4] = { 0, 0, 0, 0 };


    /* -------- open input disk image */
    status = fopen_s(&inpart, filename, "rb");

    if (status != 0) {
        printf("\n *** ERROR *** - could not open file -- %s\n\n", filename);
    }

    else {

        printf("Dumping file %s\n", filename);

        /* -------- set starting sector and number of sectors */
        not_done = true;
        current_file_pos = 0;
        word_index = 0;
        int end_of_file;
        char temp_string[10] = { 0 };

        while (not_done) {

            /* -------- read next directory sector, parse and print */
            bytes_read = 0;
            end_of_file = -1;
            stat = TapeImg_read_next_record(inpart, &current_file_pos, &tape_buffer, 65536, &bytes_read, &end_of_file);

            printf(" read tape record -- status %d bytes read %zd end of file %d\n", stat, bytes_read, end_of_file);

            if (bytes_read > 0) {

                size_t temp_size = bytes_read;
                if (temp_size % 2 != 0)
                    temp_size++;
                temp_size /= 2;

                if (swap_bytes) {
                    for (j = 0; j < temp_size; j++) {
                        tape_buffer.words[j] = bswap16(tape_buffer.words[j]);
                    }
                }

                // words_read = bytes_read / 2;
                words_read = temp_size;

                for (start_word = 0; start_word < words_read; start_word += 4) {

                    /* -------- ascii xx, xx, xx, xx, xx, xx, xx, xx | xxxxxx, xxxxxx, xxxxxx, xxxxxx | 0xxxxx 0xxxxx 0xxxxx 0xxxxx | can can can can */
                    /* -------- allow only printable characters */
                    chars[1] = MAX(tape_buffer.words[start_word] & 0x007f, 32);
                    chars[0] = MAX((tape_buffer.words[start_word] >> 8) & 0x007f, 32);
                    chars[3] = MAX(tape_buffer.words[start_word + 1] & 0x007f, 32);
                    chars[2] = MAX((tape_buffer.words[start_word + 1] >> 8) & 0x007f, 32);
                    chars[5] = MAX(tape_buffer.words[start_word + 2] & 0x007f, 32);
                    chars[4] = MAX((tape_buffer.words[start_word + 2] >> 8) & 0x007f, 32);
                    chars[7] = MAX(tape_buffer.words[start_word + 3] & 0x007f, 32);
                    chars[6] = MAX((tape_buffer.words[start_word + 3] >> 8) & 0x007f, 32);

                    strcpy_s(cancode1, 4, from_can_code(tape_buffer.words[start_word], temp_string));
                    strcpy_s(cancode2, 4, from_can_code(tape_buffer.words[start_word + 1], temp_string));
                    strcpy_s(cancode3, 4, from_can_code(tape_buffer.words[start_word + 2], temp_string));
                    strcpy_s(cancode4, 4, from_can_code(tape_buffer.words[start_word + 3], temp_string));

                    printf(" %8lld  | %08s | %6d %6d %6d %6d | 0x%04X 0x%04X 0x%04X 0x%04X | %3s %3s %3s %3s | \n",
                        word_index, chars,
                        tape_buffer.words[start_word], tape_buffer.words[start_word + 1], tape_buffer.words[start_word + 2], tape_buffer.words[start_word + 3],
                        tape_buffer.words[start_word], tape_buffer.words[start_word + 1], tape_buffer.words[start_word + 2], tape_buffer.words[start_word + 3],
                        cancode1, cancode2, cancode3, cancode4
                    );
                    word_index += 4;

                    if (stat != 0)
                        not_done = false;

                }
            }
            else if (end_of_file != 0) {
                printf("\n ============= end of file ====================\n");
                not_done = false;
            }

            else
            {
                printf("\n ====================== file mark ===============\n");
            }
        }

        fclose(inpart);

        printf("\nDone.\n");

    }
}
