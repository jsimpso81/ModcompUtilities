#include <string.h>
#include <stdlib.h>


#include "../modcomp_utility_library/modcomp_utility_library.h"



void extract_modcomp_emul_disk_partition(char* image_name, char* partition_file, __int64 start_sector, __int64 sector_count, 
                                    __int64 sector_per_track, __int64 geom, bool dump_raw) {

    FILE* inimg;
    FILE* outpart;
    __int64 sector_offset;
    unsigned _int16 sector_buffer[128];
    unsigned _int16 raw_sector_buffer[129];
    int stat;
    errno_t status;
    size_t sect_size = 0;


    /* -------- open input disk image */
    status = fopen_s(&inimg, image_name, "rb");

    if (status == 0) {

        /* -------- open output partition image */
        status = fopen_s(&outpart, partition_file, "wb");

        if (status == 0) {

            if (dump_raw) {
                sect_size = 258;
                /* -------- loop over all sectors, reading and writing */
                for (sector_offset = 0; sector_offset < sector_count; sector_offset++) {
                    stat = read_modcomp_emul_disk_sector_lba_raw(inimg, sector_offset + start_sector, raw_sector_buffer);
                    fwrite(raw_sector_buffer, sect_size, (size_t)1, outpart);
                }
            }
            else {
                sect_size = 256;
                /* -------- loop over all sectors, reading and writing */
                for (sector_offset = 0; sector_offset < sector_count; sector_offset++) {
                    stat = read_modcomp_emul_disk_sector_lba(inimg, sector_offset + start_sector, sector_buffer);
                    fwrite(sector_buffer, sect_size, (size_t)1, outpart);
                }
            }

            fclose(outpart);

            printf("\nDone.\n");
        }
        fclose(inimg);
    }
}
