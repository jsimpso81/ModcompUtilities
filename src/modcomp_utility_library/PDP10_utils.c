#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"


/* ========================================================================================================================*/
void PDP10_dump_tape_header(unsigned __int64* tape_words_36) {

    const char* record_types[9] = { "--Unknown--          ",
                          "Tape Label           ",
                          "Saveset Header       ",
                          "Saveset Trailer      ",
                          "Data Record          ",
                          "Directory Information",
                          "End of Volume        ",
                          "Comment Record       ",
                          "Saveset Continuation " };

    // -------- TAPE RECORD HEADER.
    printf(" Tape Record Header.\n");
    printf("       Record type.........................: %lld - %s \n", tape_words_36[0],
        (tape_words_36[0] <= 8 ? record_types[tape_words_36[0]] : record_types[0]));
    printf("       Sequence number.....................: %lld \n", tape_words_36[1]);
    printf("       Relative tape number ...............: %lld \n", tape_words_36[2]);
    printf("       Flags     ..........................: 0x%09llX \n", tape_words_36[3]);
    printf("       Checksum ...........................: %lld \n", tape_words_36[4]);
    printf("       Number of data words ...............: %lld \n", tape_words_36[5]);
    printf("       Number of works to skip before data : %lld \n", tape_words_36[6]);
    printf("       Customer word ..................... : %lld \n", tape_words_36[13]);
}

/* ========================================================================================================================*/
void PDP10_dump_2word(unsigned __int64 inx, unsigned __int64 word1, unsigned __int64 word2) {


    unsigned char chars[13] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    unsigned char chars10[11] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

    //--------convert a single word to 6 ascii characters.
    PDP10_parse_6bit_text(word1, &(chars[0]));
    PDP10_parse_6bit_text(word2, &(chars[6]));

    // --------tape 7 bit...
    PDP10_parse_7bit_text(word1, &chars10[0]);
    // --------make certain it is printable....
    chars10[0] = (chars10[0] < 32 || chars10[0]>126 ? 32 : chars10[0]);
    chars10[1] = (chars10[1] < 32 || chars10[1]>126 ? 32 : chars10[1]);
    chars10[2] = (chars10[2] < 32 || chars10[2]>126 ? 32 : chars10[2]);
    chars10[3] = (chars10[3] < 32 || chars10[3]>126 ? 32 : chars10[3]);
    chars10[4] = (chars10[4] < 32 || chars10[4]>126 ? 32 : chars10[4]);

    PDP10_parse_7bit_text(word2, &chars10[5]);
    // --------make certain it is printable....
    chars10[5] = (chars10[5] < 32 || chars10[5]>126 ? 32 : chars10[5]);
    chars10[6] = (chars10[6] < 32 || chars10[6]>126 ? 32 : chars10[6]);
    chars10[7] = (chars10[7] < 32 || chars10[7]>126 ? 32 : chars10[7]);
    chars10[8] = (chars10[8] < 32 || chars10[8]>126 ? 32 : chars10[8]);
    chars10[9] = (chars10[9] < 32 || chars10[9]>126 ? 32 : chars10[9]);

    printf(" %8lld | %012s | %012llo %012llo | 0x%09llX 0x%09llX | %10s | \n",
        inx, chars, word1, word2, word1, word2, chars10);
}

/* ========================================================================================================================*/
void PDP10_dump_2word_header() {
    printf(" ----Header------------------------------------------------------------------------------------\n");
    printf(" -Offset ---Text (6 bit)------Value (octal)---------------Value (hex)-------------Text (7 bit)-\n");
}

/* ========================================================================================================================*/
void PDP10_dump_3word_header() {
    printf(" ----Header----------------------------------------------------------------------------------------------------------------------\n");
    printf(" -Offset ---Text (6 bit)------------Value (octal)----------------------------Value (hex)-------------------------Text (7 bit)----\n");
}

/* ========================================================================================================================*/
void PDP10_dump_3word(unsigned __int64 inx, unsigned __int64 word1, unsigned __int64 word2, unsigned __int64 word3) {


    unsigned char chars[19] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
    unsigned char chars10[16] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

    //--------convert a single word to 6 ascii characters.
    PDP10_parse_6bit_text(word1, &(chars[0]));
    PDP10_parse_6bit_text(word2, &(chars[6]));
    PDP10_parse_6bit_text(word3, &(chars[12]));

    // --------tape 7 bit...
    PDP10_parse_7bit_text(word1, &chars10[0]);
    // --------make certain it is printable....
    chars10[0] = (chars10[0] < 32 || chars10[0]>126 ? 32 : chars10[0]);
    chars10[1] = (chars10[1] < 32 || chars10[1]>126 ? 32 : chars10[1]);
    chars10[2] = (chars10[2] < 32 || chars10[2]>126 ? 32 : chars10[2]);
    chars10[3] = (chars10[3] < 32 || chars10[3]>126 ? 32 : chars10[3]);
    chars10[4] = (chars10[4] < 32 || chars10[4]>126 ? 32 : chars10[4]);

    PDP10_parse_7bit_text(word2, &chars10[5]);
    // --------make certain it is printable....
    chars10[5] = (chars10[5] < 32 || chars10[5]>126 ? 32 : chars10[5]);
    chars10[6] = (chars10[6] < 32 || chars10[6]>126 ? 32 : chars10[6]);
    chars10[7] = (chars10[7] < 32 || chars10[7]>126 ? 32 : chars10[7]);
    chars10[8] = (chars10[8] < 32 || chars10[8]>126 ? 32 : chars10[8]);
    chars10[9] = (chars10[9] < 32 || chars10[9]>126 ? 32 : chars10[9]);

    PDP10_parse_7bit_text(word3, &chars10[10]);
    // --------make certain it is printable....
    chars10[10] = (chars10[10] < 32 || chars10[10]>126 ? 32 : chars10[10]);
    chars10[11] = (chars10[11] < 32 || chars10[11]>126 ? 32 : chars10[11]);
    chars10[12] = (chars10[12] < 32 || chars10[12]>126 ? 32 : chars10[12]);
    chars10[13] = (chars10[13] < 32 || chars10[13]>126 ? 32 : chars10[13]);
    chars10[14] = (chars10[14] < 32 || chars10[14]>126 ? 32 : chars10[14]);


    printf(" %8lld | %018s | %012llo %012llo %012llo | 0x%09llX 0x%09llX 0x%09llX | %15s | \n",
        inx, chars, word1, word2, word3, word1, word2, word3, chars10);
}

/* ========================================================================================================================*/
void PDP10_parse_7bit_text(unsigned __int64 word36, unsigned char* chars6) {

    //--------convert a single word to 5 ascii characters.
    chars6[0] = (word36 >> 29) & 0x7f;
    chars6[1] = (word36 >> 22) & 0x7f;
    chars6[2] = (word36 >> 15) & 0x7f;
    chars6[3] = (word36 >> 8) & 0x7f;
    chars6[4] = (word36 >> 1) & 0x7f;
    chars6[5] = 0;

}

/* ========================================================================================================================*/
void PDP10_parse_6bit_text(unsigned __int64 word36, unsigned char* chars7) {

    //--------convert a single word to 6 ascii characters.
    //--------convert a single word to 6 ascii characters.
    chars7[0] = ((word36 >> 30) & 0x000000000000003f) + 32;
    chars7[1] = ((word36 >> 24) & 0x000000000000003f) + 32;
    chars7[2] = ((word36 >> 18) & 0x000000000000003f) + 32;
    chars7[3] = ((word36 >> 12) & 0x000000000000003f) + 32;
    chars7[4] = ((word36 >> 6) & 0x000000000000003f) + 32;
    chars7[5] = (word36 & 0x000000000000003f) + 32;
}

/* ========================================================================================================================*/
unsigned __int64 PDP10_tape_decode_word36(unsigned char* tapebytes) {

    unsigned __int64 word36_temp1;
    unsigned __int64 word36_temp2;
    unsigned __int64 word36_temp3;
    unsigned __int64 word36_temp4;
    unsigned __int64 word36_temp5;
    unsigned __int64 word36_temp;
    unsigned __int64 test1;
    unsigned __int64 test2;

    word36_temp1 = tapebytes[0];
    word36_temp2 = tapebytes[1];
    word36_temp3 = tapebytes[2];
    word36_temp4 = tapebytes[3];
    word36_temp5 = tapebytes[4];

    word36_temp = (word36_temp1 << 28) | (word36_temp2 << 20) | (word36_temp3 << 12) |
        (word36_temp4 << 4) | (word36_temp5 & 0x000000000000000f);
    // --------check last byte... high order 2 bits should be zero.
    if ((word36_temp5 & 0x00000000000000c0) != 0) {
        fprintf(stderr, "\n *** ERROR ***  High order 2 bits of byte 5 are not zero.\n");
    }
    // --------check last byte -- next 2 highest bits should match low order bits of byte 4
    test1 = (word36_temp4 & 0x0000000000000003);
    test2 = ((word36_temp5 >> 4) & 0x0000000000000003);
    //if ( test1 != test2 ) {
    //    printf("\n *** ERROR ***  Dup bit in bytes 4 and 5 dont match %lld %lld.\n",test1,test2);
    //}
    if (test2 != 0) {
        fprintf(stderr, "\n *** ERROR ***  Dup bit in bytes 4 and 5 not zero %lld.\n", test2);
    }

    return word36_temp;
}


/* ========================================================================================================================*/
void PDP10_write_extracted_word(FILE* extpart, unsigned __int64* word36) {

    size_t items_written = 0;

    items_written = fwrite(word36, 8, 1, extpart);
    if (items_written != 1) {
        fprintf(stderr, " Items written mismatch %lld not 1\n", items_written);
    }
}


/* ========================================================================================================================*/
unsigned __int64 PDP10_read_extracted_word(FILE* readfile) {

    size_t items_read = 0;
    unsigned __int64 read_word_36 = 0;

    items_read = fread(&read_word_36, 8, 1, readfile);
    if (items_read != 1) {
        fprintf(stderr, " Items read mismatch %lld not 1\n", items_read);
    }
    return read_word_36;
}
