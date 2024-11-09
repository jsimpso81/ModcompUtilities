#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

void update_sic_884x_disk_partition(char* image_name, char* partition_file, __int64 start_sector, __int64 sector_count, __int64 unit ) {
     
    FILE* outimg;
    FILE* inpart;
    __int64 sector_offset;
    unsigned _int8 raw_sector_buffer[RAW_SECTOR_BYTES];
    int stat;
    errno_t status;
    __int64 loc_start_sector = 0;

    //-------- calc loc start sector
    loc_start_sector = start_sector + unit * SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK;

    /* -------- open output disk image */
    status = fopen_s(&outimg, image_name, "r+b");

    if (status == 0) {

        /* -------- open input partition image */
        status = fopen_s(&inpart, partition_file, "rb");

        if (status == 0) {


            /* -------- loop over all sectors, reading and writing */
            for (sector_offset = 0; sector_offset < sector_count; sector_offset++) {

                fread(raw_sector_buffer, RAW_SECTOR_BYTES, (size_t)1, inpart);

                stat = write_884x_disk_sector_lba(outimg, sector_offset + loc_start_sector, raw_sector_buffer);

            }

            fclose(inpart);

            printf("\nDone.\n");
        }
        fclose(outimg);
    }
}
