#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

void copy_disk_partition_to_tape_image(char* partition_file, char* tape_image_name, bool swap_bytes) {

    FILE* inpart;
    FILE* outimg;
    unsigned _int8 raw_sector_buffer[RAW_SECTOR_BYTES];
    int stat;
    errno_t status;
    //__int64 loc_start_sector = 0;
    size_t bytes_written = 0;
    size_t bytes_read = 0;
    char eof_buffer[2] = { '$', '$' };
    int file_eof = 0;
    int j = 0;
    _int8 a = 0;
    _int8 b = 0;

    //-------- calc loc start sector
    //loc_start_sector = start_sector + unit * SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK;

    /* -------- open output tape image */
    status = fopen_s(&outimg, tape_image_name, "wb");

    if (status == 0) {

        /* -------- open input partition image */
        status = fopen_s(&inpart, partition_file, "rb");

        if (status == 0) {

            /* -------- loop over all sectors, reading and writing */
            while ( true ) {
            //for (sector_offset = 0; sector_offset < sector_count; sector_offset++) {

                // --------try to read 256 one byte records....
                bytes_read = 0;
                // --------clear buffer.
                for (j = 0; j < RAW_SECTOR_BYTES; j++) {
                    raw_sector_buffer[j] = 0;
                }
                bytes_read = fread(raw_sector_buffer, (size_t)1, RAW_SECTOR_BYTES,  inpart);
                file_eof = feof(inpart);
                printf(" --- read disk sector - bytes read = %d, eof = %d\n",(int)bytes_read, file_eof);
                //if ( file_eof == 0) {
                if ( bytes_read > 0 ) {

                    // --------tape records must be even...
                    if (bytes_read % 2 != 0)
                        bytes_read++;

                    if (swap_bytes) {
                        for (j = 0; j < bytes_read; j+=2) {
                            a = raw_sector_buffer[j];
                            b = raw_sector_buffer[j+1];
                            raw_sector_buffer[j] = b;
                            raw_sector_buffer[j+1] = b;
                        }
                    }

                    stat = TapeImg_write_next_record(outimg, raw_sector_buffer, (int)bytes_read, &bytes_written);
                    printf(" --- write tape image record stat=%d\n", stat);
                }
                else {
                    break;
                }

            }
            stat = TapeImg_write_next_record(outimg, eof_buffer, 2, &bytes_written);
            printf(" --- write tape image record stat=%d\n", stat);
            //stat = TapeImg_write_next_record(outimg, eof_buffer, 2, &bytes_written);
            //printf(" --- write tape image record stat=%d\n", stat);

            fclose(inpart);

            printf("\nDone.\n");
        }
        // --------trouble opening disk file
        else {
            printf("\n *** ERROR *** Trouble opening disk file stat=%d.\n", status);
        }
        fclose(outimg);
    }
    // --------trouble opening tape image
    else {
        printf("\n *** ERROR *** Trouble opening tape image file stat=%d.\n",status);
    }
}
