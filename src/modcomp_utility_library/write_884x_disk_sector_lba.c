#include <stdio.h>
#include <memory.h>

#include "modcomp_utility_library.h"



int write_884x_disk_sector_lba(FILE* fp, __int64 sector, void* raw_sector_buf) {

	__int64 pos;
	__int64 desired_pos;
	int stat;
	size_t return_count;

	SIC_884x_DISC_SECTOR disk_buff = { 0 };


	desired_pos = SIC_8840_IMG_START_OFFSET_BYTES + sector * SIC_8840_IMG_SECTOR_BYTES;
	pos = _ftelli64(fp);

	if (pos != desired_pos)
		stat = _fseeki64(fp, desired_pos, SEEK_SET);
	else
		stat = 0;


	if (stat == 0) {

		memcpy( &disk_buff, raw_sector_buf, RAW_SECTOR_BYTES);

		return_count = fwrite(&disk_buff, (size_t)SIC_8840_IMG_SECTOR_BYTES, 1, fp);
		stat = ferror(fp);
	}

	/* printf("\n first two bytes of image sector : %d", disk_buff.lba); */


	return stat;

}