#include <string.h>
#include <stdlib.h>


#include "../modcomp_utility_library/modcomp_utility_library.h"



void extract_raw_disk_partition(char* image_name, char* partition_file, __int64 start_sector, __int64 sector_count, 
                                __int64 sector_per_track, __int64 geom ) {

    FILE* inimg;
    FILE* outpart;
    __int64 sector_offset;
    unsigned _int16 sector_buffer[128];
    int stat;
    errno_t status;
    int end_of_file = 0;
    size_t return_count;
    __int64 log_to_phys[128] = { 0 };
    __int64 track_start;
    __int64 log_sector;
    __int64 phys_sector;
    __int64 phys_sector_offset;
    __int16 geo_stat;

    // -------- build geometry table.
    geo_stat = geom_calc(sector_per_track, geom, log_to_phys);
    if (geo_stat != 0) {
        printf("\n *** ERROR ***  Error with sector per track or geometry value.\n");
        return;
    }

    /* -------- open input disk image */
    status = fopen_s(&inimg, image_name, "rb");

    if (status == 0) {

        /* -------- open output partition image */
        status = fopen_s(&outpart, partition_file, "wb");

        if (status == 0) {


            /* -------- loop over all sectors, reading and writing */
            for (sector_offset = 0; sector_offset < sector_count; sector_offset++) {

                // -------- assume that partition must start on a track boundary!!!!

                // -------- calc log sector in track
                log_sector = sector_offset % sector_per_track;

                // -------- calc start of track sector;
                track_start = sector_offset - log_sector;

                // -------- get phys sector in track.
                phys_sector = log_to_phys[log_sector];

                // -------- calc phy_sector_offset
                phys_sector_offset = track_start + phys_sector;

                stat = read_raw_disk_sector_lba(inimg, phys_sector_offset + start_sector, 1, sector_buffer, &return_count, &end_of_file);

                if (return_count > 0) {
                    fwrite(sector_buffer, (size_t)256, (size_t)1, outpart);
                }
                else {
                    printf("\n *** ERROR ***  Expected returned data, got nothing. return count=%jd, eof=%d\n", return_count, end_of_file);
                }
            }

            fclose(outpart);

            printf("\nDone.\n");
        }
        fclose(inimg);
    }
}
