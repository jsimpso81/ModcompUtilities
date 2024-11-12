// TapeImg_ExtractFile.c : This file contains the 'main' function. Program execution begins and ends there.
//

#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

/* ========================================================================================================================*/
void extract_file_from_tape_image(char* tape_file_name, char* disk_file_name) {

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

    //--------loop over all other tape records until end of file.  Write to disk.
    sector_offset = 0;
    current_file_pos = 0;

    while (not_done) {
        bytes_read = 0;
        end_of_file = -1;

        for (j = 0; j < 256; j++) {
            tape_buffer.words[j] = 0;
        }

        stat = TapeImg_read_next_record(in_tape_file, &current_file_pos, &tape_buffer, 65536, &bytes_read, &end_of_file);
        printf(" read tape record -- status %d bytes read %zd end of file %d\n", stat, bytes_read, end_of_file);

        if (bytes_read > 0 && bytes_read <= 256 && stat == 0) {
            fwrite(&tape_buffer, (size_t)256, (size_t)1, out_disk_file);
            printf(" write disk record -- \n");
        }
        else {
            if (bytes_read > 0) {
                printf("\n ============= UNEXPECTED NUMBER OF BYTES %zd====================\n",bytes_read);
                not_done = false;
            }
            else if (end_of_file != 0) {
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

    //--------close disk file.
    fclose(out_disk_file);


    return;

}


/* ========================================================================================================================*/
int main(int argc, char* argv[]) {

    int j;
    bool have_tape_file_name = false;
    bool have_disk_file_name = false;
    char tape_file_name[1000] = { 0 };
    char disk_file_name[1000] = { 0 };

    /* -------- annouce our program  */
    printf("\nTapeImg_ExtractFile - Convert Tape file to Disk File.\n");

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
                exit(0);
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
            extract_file_from_tape_image(tape_file_name, disk_file_name);
        }
    }
    exit(0);
}
