#include <string.h>
#include <stdlib.h>


#include "../modcomp_utility_library/modcomp_utility_library.h"



int sic_884x_is_sector_eof(FILE* inimg, __int64 unit, __int64 unit_sector, bool* is_eof ) {

    __int64 sector_unit_offset = 0;
    __int64 abs_sector = 0;
    unsigned _int8 raw_sector_buffer[SIC_8840_IMG_SECTOR_BYTES] = { 0 };
    int stat;
    bool loc_is_eof = false;

    // -------- ensure nothing left in buffer....
    raw_sector_buffer[0] = 0;
    raw_sector_buffer[1] = 0;

    // -------- calc unit offset
    sector_unit_offset = unit * SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK;

    // --------
    abs_sector = sector_unit_offset + unit_sector;

    // -------- read sector
    stat = read_884x_disk_sector_lba(inimg, abs_sector, raw_sector_buffer);
    // TODO: check read status.

    // -------- check for EOF
    loc_is_eof = (raw_sector_buffer[0] == 0x24) && (raw_sector_buffer[1] == 0x24);
    
    if (loc_is_eof) {
        int j;
        bool not_zero = false;
        for (j = 2; j < 256; j++) {
            if (raw_sector_buffer[j] != 0)
                not_zero = true;
        }
        if (not_zero) {
            printf("\ rest of sector not all zero.\n");
        }
    }
    
    *is_eof = loc_is_eof;

    return 0;
}
