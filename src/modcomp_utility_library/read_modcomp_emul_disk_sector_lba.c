#include <stdio.h>
#include <memory.h>

#include "modcomp_utility_library.h"


int read_modcomp_emul_disk_sector_lba(FILE* fp, __int64 sector, void *raw_sector_buf  ) {

	__int64 pos;
	__int64 desired_pos;
	int stat;
	size_t return_count;
	// int j;

	MODCOMP_EMUL_DISC_SECTOR disk_buff = { 0 };

	desired_pos = sector * (__int64)MODCOMP_EMUL_DISK_IMG_SECTORY_BYTES;
	pos = _ftelli64(fp);

	if (pos != desired_pos)
		stat = _fseeki64(fp, desired_pos, SEEK_SET);
	else
		stat = 0;

	if (stat == 0) {
		return_count = fread(&disk_buff, MODCOMP_EMUL_DISK_IMG_SECTORY_BYTES, 1, fp);
		stat = ferror(fp);
	}

	/* printf("\n first two bytes of image sector : %d", disk_buff.lba); */

	// --------byte swap sector data...
	// for (j = 0; j < 128; j++) {
	//  	disk_buff.rawsectbuffer[j] = bswap16(disk_buff.rawsectbuffer[j]);
	// }

	memcpy(raw_sector_buf, disk_buff.rawsectbuffer, (size_t)RAW_SECTOR_BYTES );

	return stat;

}