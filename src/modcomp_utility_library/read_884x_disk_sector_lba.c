#include <stdio.h>
#include <memory.h>

#include "modcomp_utility_library.h"



int read_884x_disk_sector_lba(FILE* fp, __int64 sector, void* raw_sector_buf) {

	__int64 pos;
	__int64 desired_pos;
	int stat;
	size_t return_count;

	// -------- 8840 sector is matches real disk sector.
	SIC_884x_DISC_SECTOR disk_buff = { 0 };


	desired_pos = SIC_8840_IMG_START_OFFSET_BYTES + sector * SIC_8840_IMG_SECTOR_BYTES;
	pos = _ftelli64(fp);

	if (pos != desired_pos)
		stat = _fseeki64(fp, desired_pos, SEEK_SET);
	else
		stat = 0;

	if (stat == 0) {
		return_count = fread(&disk_buff, (size_t)SIC_8840_IMG_SECTOR_BYTES, 1, fp);
		stat = ferror(fp);
	}

	memcpy(raw_sector_buf, &disk_buff, RAW_SECTOR_BYTES);

	if (stat != 0) {
		printf("\n *** ERROR *** read_884x_disk_sector error %d\n", stat);
	}

	return stat;

}