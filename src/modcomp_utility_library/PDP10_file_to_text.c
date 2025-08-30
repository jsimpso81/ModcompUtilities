#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

// #define MAX(i, j) (((i) > (j)) ? (i) : (j))
// #define MIN(i, j) (((i) < (j)) ? (i) : (j))



/* ========================================================================================================================*/
void PDP10_file_to_text(char* filename) {

    FILE* inpart = NULL;
    unsigned int word_index = 0;
    unsigned __int64 word36 = 0;
    errno_t status;
    int j = 0;
    unsigned int cr_lf_state = 0;


    /* -------- open input disk image */
    status = fopen_s(&inpart, filename, "rb");

    if (status != 0) {
        fprintf(stderr, "\n *** ERROR *** - could not open file -- %s\n\n", filename);
        return;
    }

    bool not_done = true;
    unsigned char chars6[6] = { 0,0,0,0,0,0 };
    cr_lf_state = 0;
    // --------do until done....
    while (not_done) {
        // --------first word is low order word.
        word36 = PDP10_read_extracted_word(inpart);

        // --------assume if null then end of file.....
        if (word36 == 0) {
            not_done = false;
        }
        else {
            PDP10_parse_7bit_text(word36, chars6);
            // printf("%5s", chars6);
            for (j = 0; j < 5; j++) {
                if (chars6[j] == 10) {
                    // fprintf(stderr, " <LF> found\n");
                    if (cr_lf_state == 1) {
                        printf("\n");
                    }
                    else {
                        printf("%1c", chars6[j]);
                    }
                    cr_lf_state = 0;
                }
                else if (chars6[j] == 13) {
                    // fprintf(stderr, " <CR> found\n");
                    if (cr_lf_state == 1) {
                        printf("%1c", 13);      // orphan cr, print it...
                    }
                    cr_lf_state = 1;
                }
                else {
                    if (cr_lf_state == 1) {
                        printf("%1c", 13);      // orphan cr, print it...
                    }
                    cr_lf_state = 0;
                    printf("%1c", chars6[j]);
                }
            }
        }
    }
    if (cr_lf_state == 1) {
        printf("%1c", 13);      // orphan cr, print it...
    }
    cr_lf_state = 0;


    fclose(inpart);

    return;

}
