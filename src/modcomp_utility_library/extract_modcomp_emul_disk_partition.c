#include <string.h>
#include <stdlib.h>


#include "../modcomp_utility_library/modcomp_utility_library.h"



void extract_modcomp_emul_disk_partition(char* image_name, char* partition_file, __int64 start_sector, __int64 sector_count, 
                                    __int64 sector_per_track, __int64 geom) {

    FILE* inimg;
    FILE* outpart;
    __int64 sector_offset;
    unsigned _int16 sector_buffer[128];
    int stat;
    errno_t status;



    /* -------- open input disk image */
    status = fopen_s(&inimg, image_name, "rb");

    if (status == 0) {

        /* -------- open output partition image */
        status = fopen_s(&outpart, partition_file, "wb");

        if (status == 0) {


            /* -------- loop over all sectors, reading and writing */
            for (sector_offset = 0; sector_offset < sector_count; sector_offset++) {
                stat = read_modcomp_emul_disk_sector_lba(inimg, sector_offset + start_sector, sector_buffer);
                fwrite(sector_buffer, (size_t)256, (size_t)1, outpart);
            }

            fclose(outpart);

            printf("\nDone.\n");
        }
        fclose(inimg);
    }
}
