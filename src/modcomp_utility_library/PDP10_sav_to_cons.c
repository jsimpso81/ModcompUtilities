#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

// #define MAX(i, j) (((i) > (j)) ? (i) : (j))
// #define MIN(i, j) (((i) < (j)) ? (i) : (j))



/* ========================================================================================================================*/
// 
//  DEC nonsharable save format:
//  
//          <data blocks>
//          <entry vector pointer>
//  
//      <data block>:
//          -<# wds>,,<addr-1>
//          <block of that many words>
//  
//      <entry vector pointer>:
//          <length of vector>,,<addr of vector>
//  
//          Vector word 0 is instr to execute to start program.
//          Vector word 1 is instr to execute to reenter program.
//          Vector word 2 contains program version # info.
//  
//          BUT if LH is 254000 then:
//                  start addr = RH(120)
//                  reenter addr = RH(124)
//                  version info in 137
//          Actually it appears that the RH may be the start address.
//                  If it's non-zero, let's use that.
//  
void PDP10_sav_to_cons(char* filename) {

    FILE* inpart = NULL;
    unsigned int word_index = 0;
    unsigned __int64 blk_word1 = 0;
    unsigned __int64 dat_word = 0;
    __int32 blk_len = 0;
    unsigned __int32 blk_cmd = 0;
    unsigned __int32 blk_addr = 0;
    __int32 j = 0;

    errno_t status;
    bool notdone = true;


    /* -------- open input disk image */
    status = fopen_s(&inpart, filename, "rb");

    if (status != 0) {
        printf("\n *** ERROR *** - could not open file -- %s\n\n", filename);
        return;
    }
    printf(";--------dump of file: %s\n", filename);


    // printf("\n Sav to cons -- starting\n\n");

    // --------loop over all data blocks.  all data blocks must start with a negative value....
    notdone = true;
    while ( notdone ) {

        // --------first word is low order word.
        blk_word1 = PDP10_read_extracted_word(inpart);

        // -------- is block length negative.  if not then this is not a data block,
        // -------- but rather the entry vector
        if ((blk_word1 & 0400000000000) == 0) {
            notdone = false;
            // printf("; ----- not data block, value %012llo\n", blk_word1);
        }
        else {
            blk_len = ((blk_word1 >> 18) & 0777777) | 0xfffc0000;
            blk_len *= -1;
            blk_addr = (blk_word1 & 0777777) + 1;
            // printf("; ----- data block, value %012llo, length %d, addr %012o\n", blk_word1,blk_len,blk_addr);


            printf("LA %07o\n", blk_addr);
            for (j = 0; j < blk_len; j++) {
                dat_word = PDP10_read_extracted_word(inpart);
                if (j == 0) {
                    printf("DM %012llo\n", dat_word);
                }
                else {
                    printf("DN %012llo\n", dat_word);
                }

            }
        }
    }

    // -------- process vector....
    blk_cmd = ( (blk_word1 >> 18 ) & 0777777);
    blk_addr = (blk_word1 & 0777777);
    printf(";---- high half %06o \n", blk_cmd);
    printf("ST %06o\n", blk_addr);

    // --------ensure we got all data... This should fail..
    blk_word1 = PDP10_read_extracted_word(inpart);


    fclose(inpart);

    return;

}
