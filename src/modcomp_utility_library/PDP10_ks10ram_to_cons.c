#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

// #define MAX(i, j) (((i) > (j)) ? (i) : (j))
// #define MIN(i, j) (((i) < (j)) ? (i) : (j))



/* ========================================================================================================================*/
void PDP10_ks10ram_to_cons(char* filename) {

    FILE* inpart = NULL;
    unsigned int word_index = 0;
    unsigned __int64 mc_word1 = 0;
    unsigned __int64 mc_word2 = 0;
    unsigned __int64 mc_word3 = 0;
    errno_t status;


    /* -------- open input disk image */
    status = fopen_s(&inpart, filename, "rb");

    if (status != 0) {
        printf("\n *** ERROR *** - could not open file -- %s\n\n", filename);
        return;
    }

    // --------loop over all the microcode ram words 2k..
    for (word_index = 0; word_index < 2048; word_index++) {
    
        // --------first word is low order word.
        mc_word1 = PDP10_read_extracted_word(inpart);
        mc_word2 = PDP10_read_extracted_word(inpart);
        mc_word3 = PDP10_read_extracted_word(inpart);

        // ----------36 bit words must only be 12 octal digits long...
        if (mc_word1 > 0777777777777) {
            printf(" *** ERROR *** 36 bit word 1 exceeds 12 digits %022llo\n", mc_word1);
        }
        // ----------36 bit words must only be 12 octal digits long...
        if (mc_word2 > 0777777777777) {
            printf(" *** ERROR *** 36 bit word 2 exceeds 12 digits %022llo\n", mc_word2);
        }
        // ----------last word should only be 8 octal digits long...
        if (mc_word3 > 077777777) {
            printf(" *** ERROR *** high order word is greater than 8 digits %022llo\n", mc_word3);
        }

        printf("LC %04o\n",word_index);
        printf("DC %08llo%012llo%012llo\n",mc_word3,mc_word2,mc_word1);
    }

    // --------this should give an error !
    mc_word1 = PDP10_read_extracted_word(inpart);


    fclose(inpart);

    return;

}
