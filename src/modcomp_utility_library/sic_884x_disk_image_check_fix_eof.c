#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"


void sic_884x_disk_image_check_fix_eof(char* image_name, __int64 unit, bool fix, bool list) {

    FILE* img_file;
    __int64 sector_num = 0;
    __int64 sector_count = 0;
    int stat = 0;
    errno_t status;
    bool is_eof_disk = false;
    bool is_eof_buff = false;
    int stat1 = 0;
    int stat2 = 0;

    // -------- if list then turn fix off
    if (list) {
        fix = false;
        printf("\n List only.  Fix disabled.\n");
    }

    // -------- sector count
    sector_count = SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK;

    /* -------- open output disk image */
    status = fopen_s(&img_file, image_name, "r+b");

    if (status == 0) {

        // ------- read EOF buffer.
        stat1 = read_884x_eof_buffer(img_file);
        printf("\n----Starting to check EOF-----------\n");

        /* -------- loop over all sectors, reading and writing */
        for (sector_num = 0; sector_num < sector_count; sector_num++) {

            stat1 = sic_884x_is_sector_eof(img_file, unit, sector_num, &is_eof_disk);

            if (list) {
                if (is_eof_disk) {
                    printf(" EOF unit %lld sector %lld\n", unit, sector_num);
                }
            }
            else {

                stat2 = sic_884x_check_eof_buffer(unit, sector_num, &is_eof_buff);

                if (is_eof_disk != is_eof_buff) {
                    printf("sector %lld EOF mismatch disk: %s buffer: %s\n", sector_num,
                        (is_eof_disk ? "   EOF   " : " not EOF "),
                        (is_eof_buff ? "   EOF   " : " not EOF "));
                    if (fix) {
                        stat1 = sic_884x_set_eof_buffer(unit, sector_num, is_eof_disk);
                    }
                }
            }

        }
        if (fix) {
            stat1 = write_884x_eof_buffer(img_file);
        }

        fclose(img_file);
    }
}



void sic_884x_disk_image_dump_eof_buffer(char* image_name) {

    FILE* img_file;
    __int64 sector_num = 0;
    __int64 abs_sector = 0;
    __int64 sector_count = 0;
    __int64 sector_unit_offset = 0;
    __int64 unit = 0;
    __int64 total_byte_offset = 0;
    __int64 sect_byte_offset = 0;
    errno_t status;
    bool is_eof_disk = false;
    int stat1 = 0;
    unsigned _int8 raw_sector_buffer[SIC_8840_IMG_SECTOR_BYTES] = { 0 };
    int stat;


    /* -------- open output disk image */
    status = fopen_s(&img_file, image_name, "r+b");

    if (status == 0) {
        // -------- sector count
        sector_count = SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK;

        // -------- calc unit offset
        unit = 4;
        sector_unit_offset = unit * SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK;

        /* -------- loop over all sectors, reading and writing */
        for (sector_num = 0; sector_num < sector_count; sector_num++) {

            stat1 = sic_884x_is_sector_eof(img_file, unit, sector_num, &is_eof_disk);

            // --------
            abs_sector = sector_unit_offset + sector_num;

            // -------- read sector
            stat = read_884x_disk_sector_lba(img_file, abs_sector, raw_sector_buffer);

            for (sect_byte_offset = 0; sect_byte_offset < SIC_8840_IMG_SECTOR_BYTES; sect_byte_offset++) {
                if (raw_sector_buffer[sect_byte_offset] != 0) {
                    total_byte_offset = sector_num * SIC_8840_IMG_SECTOR_BYTES + sect_byte_offset;
                    printf("\n  byte offset %lld value 0x%02x", total_byte_offset, raw_sector_buffer[sect_byte_offset]);
                }

            }

        }


        // -------- calc unit offset
        unit = 5;
        sector_unit_offset = unit * SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK;

        /* -------- loop over all sectors, reading and writing */
        for (sector_num = 0; sector_num < sector_count; sector_num++) {

            stat1 = sic_884x_is_sector_eof(img_file, unit, sector_num, &is_eof_disk);

            // --------
            abs_sector = sector_unit_offset + sector_num;

            // -------- read sector
            stat = read_884x_disk_sector_lba(img_file, abs_sector, raw_sector_buffer);

            for (sect_byte_offset = 0; sect_byte_offset < SIC_8840_IMG_SECTOR_BYTES; sect_byte_offset++) {
                if (raw_sector_buffer[sect_byte_offset] != 0) {
                    total_byte_offset = sector_num * SIC_8840_IMG_SECTOR_BYTES + sect_byte_offset + SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK * SIC_8840_IMG_SECTOR_BYTES;
                    printf("\n  byte offset %lld value 0x%02x", total_byte_offset, raw_sector_buffer[sect_byte_offset]);
                }

            }

        }


        // -------- calc unit offset
        unit = 6;
        sector_unit_offset = unit * SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK;

        /* -------- loop over all sectors, reading and writing */
        for (sector_num = 0; sector_num < sector_count; sector_num++) {

            stat1 = sic_884x_is_sector_eof(img_file, unit, sector_num, &is_eof_disk);

            // --------
            abs_sector = sector_unit_offset + sector_num;

            // -------- read sector
            stat = read_884x_disk_sector_lba(img_file, abs_sector, raw_sector_buffer);

            for (sect_byte_offset = 0; sect_byte_offset < SIC_8840_IMG_SECTOR_BYTES; sect_byte_offset++) {
                if (raw_sector_buffer[sect_byte_offset] != 0) {
                    total_byte_offset = sector_num * SIC_8840_IMG_SECTOR_BYTES + sect_byte_offset + SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK * SIC_8840_IMG_SECTOR_BYTES;
                    printf("\n  byte offset %lld value 0x%02x", total_byte_offset, raw_sector_buffer[sect_byte_offset]);
                }

            }

        }



        fclose(img_file);
    }
}