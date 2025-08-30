#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

// #define MAX(i, j) (((i) > (j)) ? (i) : (j))
// #define MIN(i, j) (((i) < (j)) ? (i) : (j))

static union {
    unsigned __int8 ubytes[65536];
    unsigned __int16 words[32768];
} tape_buffer10;

static unsigned __int64 tape_words_36[16384] = { 0 };

// --------non-data data block information....
static unsigned char file_device[40] = { 0 };
static unsigned char file_directory[40] = { 0 };
static unsigned char file_name[40] = { 0 };
static unsigned char file_ext[40] = { 0 };
static bool file_device_found = false;
static bool file_directory_found = false;
static bool file_name_found = false;
static bool file_ext_found = false;


void PDP10_tape_parse_non_data_block(unsigned __int64* data_words);
void PDP10_tape_parse_O$NAME_block(unsigned __int64* data_words);


/* ========================================================================================================================*/
void PDP10_TapeImg_dump10_records(char* filename, bool swap_bytes) {

    FILE* inpart;
    FILE* extpart = NULL;
    bool ext_file_open = false;
    char extfilename[160] = { 0 };
    __int64 current_file_pos = 0;
    size_t bytes_read = 0;
    size_t words16_read = 0;
    size_t words_36_read = 0;
    unsigned __int64 word_36_inx = 0;
    unsigned __int64 byte_inx = 0;
    unsigned __int64 ext_words = 0;
    unsigned __int64 wrt_inx = 0;

    int stat;
    errno_t status;
    bool not_done = true;
    unsigned __int64  start_word;
    size_t j;
    __int64 word_index;

    int end_of_file;
    size_t temp_size = 0;

    unsigned __int64 curr_file_no = 0;
    bool has_header = false;


#define flags_non_data_record  (unsigned __int64)0x100000000
#define flags_end_of_file    (unsigned __int64)0x800000000
// #define flags_data_record    (unsigned __int64)0x200000000


    /* -------- open input disk image */
    status = fopen_s(&inpart, filename, "rb");

    if (status != 0) {
        printf("\n *** ERROR *** - could not open file -- %s\n\n", filename);
        return;
    }


    printf("Dumping file %s\n", filename);

    /* -------- set starting sector and number of sectors */
    not_done = true;
    current_file_pos = 0;
    word_index = 0;

    while (not_done) {

        /* -------- read next directory sector, parse and print */
        bytes_read = 0;
        end_of_file = -1;
        stat = TapeImg_read_next_record(inpart, &current_file_pos, &tape_buffer10, 65536, &bytes_read, &end_of_file);

        printf(" read tape record -- status %d bytes read %zd end of file %d\n", stat, bytes_read, end_of_file);


        // --------we have some data.
        if (bytes_read > 0) {

            // -------- get number of 16 bit words read...
            temp_size = bytes_read;
            if (temp_size % 2 != 0) {
                temp_size++;
            }
            temp_size /= 2;

            // -------- swap bytes --- really shouldn't do for PDP10 tapes !!!
            if (swap_bytes) {
                for (j = 0; j < temp_size; j++) {
                    tape_buffer10.words[j] = bswap16(tape_buffer10.words[j]);
                }
            }

            // -------- get number of 16 bit words read...
            words16_read = temp_size;

            // -------- get number of 36 bit words read.
            // -------- each 36 bit word 5 bytes..
            words_36_read = bytes_read;
            words_36_read /= 5;
            if (bytes_read % 5 != 0) {
                printf("\n *** ERROR ***  Bytes read is not divisible by 5\n");
            }

            // --------parse out the 36 bit words -- FROM THE DATA RECORD....
            word_36_inx =0;
            for (byte_inx = 0; byte_inx < bytes_read; byte_inx += 5) {
                tape_words_36[word_36_inx] = PDP10_tape_decode_word36(&tape_buffer10.ubytes[byte_inx]);
                word_36_inx++;
            }

            // -------- see if tape record has header...
            if (word_36_inx == 544) {
                has_header = true;
            }
            else if (word_36_inx = 512) {
                has_header = false;
            }
            else {
                has_header = false;
                printf(" -------- Unexpected record word length %lld\n", word_36_inx);
            }

            // -------- if there is a header, dump it and process it.
            if (has_header) {

                // --------dump tape header record...
                PDP10_dump_tape_header(&(tape_words_36[0]));

                // --------see if we found an beginning of file -- as indicated by a non data record.
                if (tape_words_36[0] == 4 && ((tape_words_36[3] & flags_non_data_record) != 0)) {

                    // -------- parse non data block....
                    PDP10_tape_parse_non_data_block(&(tape_words_36[32]));

                    // -------- create extract file name.....
                    extfilename[0] = 0; // zero out any existing file name.
                    extfilename[1] = 0; // just in case.
                    strcat_s(extfilename, 150, "extract/");
                    if (file_directory_found) {
                        strcat_s(extfilename, 150, file_directory);
                        strcat_s(extfilename, 150, "__");
                    }
                    if (file_name_found) {
                        strcat_s(extfilename, 150, file_name);
                    }
                    if (file_ext_found) {
                        strcat_s(extfilename, 150, ".");
                        strcat_s(extfilename, 150, file_ext);
                    }
                    // sprintf_s(extfilename, (size_t)100, "extract/file%04lld.data", curr_file_no);
                    // -------- open file and check status.
                    status = fopen_s(&extpart, extfilename, "wb");
                    ext_words = 0;
                    if (status != 0) {
                        printf("\n *** ERROR *** - could not open file -- %s\n\n", filename);
                        ext_file_open = false;
                    }
                    else {
                        printf("\n Extract file %s opened\n", extfilename);
                        ext_file_open = true;
                    }
                }

                // -------- print out header -- octal, hex, ascii - 2 words per line
                PDP10_dump_2word_header();
                for (start_word = 0; start_word < 32; start_word += 2) {
                    PDP10_dump_2word(start_word, tape_words_36[start_word], tape_words_36[start_word + 1]);
                }

                // -------- print out data -- octal, hex, ascii - 2 words per line
                // -------- skip the 32 header words.               
                PDP10_dump_2word_header();
                for (start_word = 32; start_word < word_36_inx; start_word += 2) {
                    PDP10_dump_2word(start_word - 32, tape_words_36[start_word], tape_words_36[start_word + 1]);
                }
            }

            // --------no header, just dump data.
            else {

                // -------- print out data -- octal, hex, ascii - 2 words per line
                // -------- skip the 32 header words.               
                PDP10_dump_2word_header();
                for (start_word = 0; start_word < word_36_inx; start_word += 2) { 
                    PDP10_dump_2word(start_word, tape_words_36[start_word], tape_words_36[start_word + 1]);
                }
            }

            // ------- only process data record extraction if there is a header...
            if (has_header) {

                // --------see if we found a data record.
                if (tape_words_36[0] == 4 && ((tape_words_36[3] & flags_non_data_record) == 0)) {

                    // --------are there data words to write and is a file open...
                    if (ext_file_open && (tape_words_36[5] > 0)) {
                        // --------write some data words.
                        for (wrt_inx = 0; wrt_inx < tape_words_36[5]; wrt_inx++) {
                            PDP10_write_extracted_word(extpart, &tape_words_36[32 + wrt_inx]);
                        }
                        ext_words += tape_words_36[5];
                    }
                }

                // --------see if we found an end of file.
                if (tape_words_36[0] == 4 && ((tape_words_36[3] & flags_end_of_file) != 0)) {
                    // -------- if file is open, close it.
                    if (ext_file_open) {
                        ext_file_open = false;
                        fclose(extpart);
                        printf(" Extract file %s closed\n", extfilename);
                        printf(" Words written %lld, expected words written %lld\n", ext_words, tape_words_36[13] + tape_words_36[5]);
                    }
                    curr_file_no++;
                }
            }
        }

        // -------- bytes read == 0 and eof_mark
        else if (end_of_file != 0) {
            printf("\n ============= end of file ====================\n");
            not_done = false;
        }

        // -------- bytes read == 0
        else
        {
            printf("\n ====================== file mark ===============\n");
        }
    }

    printf("\n FOUND FILES = %lld\n", curr_file_no);

    fclose(inpart);

    printf("\nDone.\n");

    return;

}

/* ========================================================================================================================*/
void PDP10_tape_parse_non_data_block(unsigned __int64* data_words) {

    bool notdone;
    unsigned int index;
    unsigned int info_type;
    unsigned int info_len;

    // -------- set items not found...
    file_device_found = false;
    file_directory_found = false;
    file_name_found = false;
    file_ext_found = false;

    notdone = true;
    index = 0;

    while (notdone) {

        if (index >= 512) {
            notdone = false;
        }
        else {

            info_type = (data_words[index] >> 18) & 0x0003ffff;
            info_len = data_words[index] & 0x0003ffff;

            printf(" Info type %d, info length %d\n", info_type, info_len);

            // -------
            switch (info_type) {

                // -------- file name
            case 1:     // O$NAME
                PDP10_tape_parse_O$NAME_block(&(data_words[index+1]));
                index += info_len;
                break;

                // -------- file attributes
            case 2:     // O$FILE
                index += info_len;
                break;

                // -------- directory information
            case 3:     // O$DIRT
                index += info_len;
                break;

                // -------- system header
            case 4:     // O$SYSN
                index += info_len;
                break;

                // -------- saveset name
            case 5:     // O$SSNM
                index += info_len;
                break;

                // --------unknown or done....
            default:
                notdone = false;
                break;

            }
        }


    }

}


/* ========================================================================================================================*/
void PDP10_tape_parse_O$NAME_block(unsigned __int64* data_words) {

    bool notdone;
    unsigned int index;
    unsigned int info_type;
    unsigned int info_len;
    unsigned int j;

    notdone = true;
    index = 0;

    while (notdone) {

        if (index >= 512) {
            notdone = false;
        }
        else {

            info_type = (data_words[index] >> 18) & 0x0003ffff;
            info_len = data_words[index] & 0x0003ffff;

            printf(" Filename Info type %d, info length %d\n", info_type, info_len);

            // -------
            switch (info_type) {

                // -------- 
            case 1:     // ??
                index += info_len;
                break;

                // -------- FILE NAME
            case 2:     // file name
                if (info_len > 9) {
                    printf(" *** ERROR *** Filename length is too long %d\n", info_len);
                }
                for (j = 0; j < 40; j++) {
                    file_name[j] = 0;
                }
                // --------grab name stored as 7 bit text.
                for (j = 0; (j < (info_len - 1)) && ( j < 9 ); j++) {
                    PDP10_parse_7bit_text(data_words[index + j + 1], &(file_name[j * 5]));
                }
                file_name_found = true;
                index += info_len;
                break;

                // -------- FILE EXTENSION
            case 3:     // file extension
                if (info_len > 9) {
                    printf(" *** ERROR *** File extension length is too long %d\n", info_len);
                }
                for (j = 0; j < 40; j++) {
                    file_ext[j] = 0;
                }
                // --------grab name stored as 7 bit text.
                for (j = 0; (j < (info_len - 1)) && (j < 9); j++) {
                    PDP10_parse_7bit_text(data_words[index + j + 1], &(file_ext[j * 5]));
                }
                file_ext_found = true;
                index += info_len;
                break;

                // -------- 
            case 4:     // ??
                index += info_len;
                break;

                // -------- 
            case 5:     // something -- maybe protection, proj,pgm, or other attributes.  or version.
                index += info_len;
                break;

                // -------- 
            case 32:     // directory name
                if (info_len > 9) {
                    printf(" *** ERROR *** File directory length is too long %d\n", info_len);
                }
                for (j = 0; j < 40; j++) {
                    file_directory[j] = 0;
                }
                // --------grab name stored as 7 bit text.
                for (j = 0; (j < (info_len - 1)) && (j < 9); j++) {
                    PDP10_parse_7bit_text(data_words[index + j + 1], &(file_directory[j * 5]));
                }
                file_directory_found = true;
                index += info_len;
                break;

                // --------unknown or done....
            default:
                notdone = false;
                break;

            }
        }
    }
}