#include <stdio.h>
#include <memory.h>

#include "modcomp_utility_library.h"


int write_modcomp_emul_disk_sector_lba(FILE* fp, __int64 sector, void* raw_sector_buf) {

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

		memcpy( disk_buff.rawsectbuffer, raw_sector_buf, (size_t)RAW_SECTOR_BYTES);

		// TODO: Set flags....

		return_count = fwrite(&disk_buff, MODCOMP_EMUL_DISK_IMG_SECTORY_BYTES, 1, fp);
		stat = ferror(fp);
	}

	/* printf("\n first two bytes of image sector : %d", disk_buff.lba); */

	return stat;

}