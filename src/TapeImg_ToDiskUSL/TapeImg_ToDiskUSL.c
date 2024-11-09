// TapeImg_ToDiskUSL.c : This file contains the 'main' function. Program execution begins and ends there.
//

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

/* ========================================================================================================================*/
void extract_usl_from_tape_image(char* tape_file_name, char* disk_file_name) {

    FILE* in_tape_file;
    FILE* out_disk_file;
    errno_t status;

    __int64 current_file_pos = 0;
    union {
        unsigned __int8 ubytes[65536];
        unsigned __int16 words[32768];
    } tape_buffer;

    unsigned __int8 filemarkbuf[256] = { 0 };

    size_t bytes_read = 0;
    int end_of_file;
    int stat;
    __int64 sector_offset;
    bool not_done = true;
    size_t j;

    // --------define file mark buffer
    filemarkbuf[0] = 36;
    filemarkbuf[1] = 36;
    for (j = 2; j < 256; j++) {
        filemarkbuf[j] = 0;
    }


    //--------open tape file for reading.
    status = fopen_s(&in_tape_file, tape_file_name, "rb");

    if (status != 0) {
        printf("\n *** ERROR *** - could not open file -- %s\n\n", tape_file_name);
        return;
    }


    //--------open disk file for writing
    status = fopen_s(&out_disk_file, disk_file_name, "wb");

    if (status != 0) {
        printf("\n *** ERROR *** - could not open file -- %s\n\n", disk_file_name);
        return;
    }

    //--------read first tape record.   It should be x bytes long and contain @USL.
    //--------don't write this to disk.
    bytes_read = 0;
    end_of_file = -1;
    current_file_pos = 0;
    stat = TapeImg_read_next_record(in_tape_file, &current_file_pos, &tape_buffer, 65536, &bytes_read, &end_of_file);

    printf(" read tape record -- status %d bytes read %zd end of file %d\n", stat, bytes_read, end_of_file);

    if (bytes_read > 0) {
    }


    //--------loop over all other tape records until end of file.  Write to disk.
    sector_offset = 0;
    while ( not_done ) {
        stat = TapeImg_read_next_record(in_tape_file, &current_file_pos, &tape_buffer, 65536, &bytes_read, &end_of_file);
        printf(" read tape record -- status %d bytes read %zd end of file %d\n", stat, bytes_read, end_of_file);

        if (bytes_read == 256 && stat == 0) {
            fwrite(&tape_buffer, (size_t)256, (size_t)1, out_disk_file);
            printf(" write disk record -- \n");
        }
        else {
            if (end_of_file != 0) {
                printf("\n ============= end of file ====================\n");
                not_done = false;
            }
            else {
                fwrite(&filemarkbuf, (size_t)256, (size_t)1, out_disk_file);
                printf("\n ====================== file mark ===============\n");
            }
        }

    }

    //--------close tape image file
    fclose(in_tape_file);

    //--------close disk USL file.
    fclose(out_disk_file);


    return;

}


/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    int j;
    bool swap_bytes = false;
    bool have_tape_file_name = false;
    bool have_disk_file_name = false;
    char tape_file_name[1000] = { 0 };
    char disk_file_name[1000] = { 0 };

    /* -------- annouce our program  */
    printf("\nTapeImg_ToDiskUSL - Convert Tape USL image to Disk USL.\n");

    /* printf("\n arc = % d \n", argc); */

    if (argc > 0) {

        for (j = 0; j < argc; j++) {

            /* printf("     %s\n", argv[j]); */

            /* -------- print help */
            if (strcmp(argv[j], "-h") == 0 || strcmp(argv[j], "-?") == 0) {
                printf("\nTapeImg_Dump - Dump records from a tape image in readable format.\n\n");
                printf("        -h       print help message and exit\n");
                printf("        -?       print help message and exit\n");
                printf("        -f file  name of tape image input file\n");
                printf("        -d file  name of disk USL file to create\n");
                printf("        -s       swap  bytes\n");
                printf("        -n       dont swap bytes\n");
                exit(0);
            }

            /* -------- set swap bytes */
            else if (strcmp(argv[j], "-s") == 0) {
                swap_bytes = true;
            }

            /* -------- unset swap bytes */
            else if (strcmp(argv[j], "-n") == 0) {
                swap_bytes = false;
            }

            /* -------- set tape image file name to dump */
            else if (strcmp(argv[j], "-f") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(tape_file_name, (rsize_t)1000, argv[j], 1000);
                    have_tape_file_name = true;
                }
            }

            /* -------- set disk usl file name to dump */
            else if (strcmp(argv[j], "-d") == 0) {
                j++;
                if (j < argc) {
                    strncpy_s(disk_file_name, (rsize_t)1000, argv[j], 1000);
                    have_disk_file_name = true;
                }
            }

            /* --------unrecognized parameter */
            else {
                if (j != 0)
                    printf("\n *** ERROR **** Unrecognized command line parameter '%s', ignored.\n", argv[j]);
            }
        }
        // -------- if we have a file name, then dump it...
        if (have_disk_file_name && have_tape_file_name) {
            extract_usl_from_tape_image(tape_file_name, disk_file_name);
        }
    }
    exit(0);
}








// size_t bytes_read = 0;
// size_t words_read = 0;


// int stat;
// errno_t status;
// bool not_done = true;
// size_t start_word;
// size_t j;
// __int64 word_index;

// int start_byte;

// unsigned char chars[9] = { 0, 0, 0, 0, 0, 0, 0, 0, 0 };

// unsigned __int16 words[5] = { 0, 0, 0, 0, 0 };

// unsigned char cancode1[4] = { 0, 0, 0, 0 };
// unsigned char cancode2[4] = { 0, 0, 0, 0 };
// unsigned char cancode3[4] = { 0, 0, 0, 0 };
// unsigned char cancode4[4] = { 0, 0, 0, 0 };




    /* -------- set starting sector and number of sectors */
//     not_done = true;
//     current_file_pos = 0;
//     word_index = 0;
//     int end_of_file;
//     char temp_string[10] = { 0 };

//     while (not_done) {

//        /* -------- read next directory sector, parse and print */
//        bytes_read = 0;
//        end_of_file = -1;
//        stat = TapeImg_read_next_record(inpart, &current_file_pos, &tape_buffer, 65536, &bytes_read, &end_of_file);

//        printf(" read tape record -- status %d bytes read %zd end of file %d\n", stat, bytes_read, end_of_file);

//        if (bytes_read > 0) {

 //           size_t temp_size = bytes_read;
 //           if (temp_size % 2 != 0)
 //               temp_size++;
 //           temp_size /= 2;

//            if (swap_bytes) {
//                for (j = 0; j < temp_size; j++) {
//                    tape_buffer.words[j] = bswap16(tape_buffer.words[j]);
//                }
//            }

//            // words_read = bytes_read / 2;
//            words_read = temp_size;

//                word_index += 4;

//                if (stat != 0)
//                    not_done = false;

//            }
//        }
//        else if (end_of_file != 0) {
//            printf("\n ============= end of file ====================\n");
//            not_done = false;
//        }

