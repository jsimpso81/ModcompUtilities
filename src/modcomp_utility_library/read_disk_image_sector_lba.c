#include <stdio.h>
#include <memory.h>

#include "modcomp_utility_library.h"


int read_disk_image_sector_lba(FILE* fp, __int64 sector, void *buf  ) {

	__int64 pos;
	__int64 desired_pos;
	int stat;
	size_t return_count;
	int j;

	DISC_IMG_SECTOR disk_buff;

	desired_pos = sector * (__int64)disk_img_sector;
	pos = _ftelli64(fp);

	if (pos != desired_pos)
		stat = _fseeki64(fp, desired_pos, SEEK_SET);
	else
		stat = 0;

	if (stat == 0) {
		return_count = fread(&disk_buff, disk_img_sector, 1, fp);
		stat = ferror(fp);
	}

	/* printf("\n first two bytes of image sector : %d", disk_buff.lba); */

	for (j = 0; j < 128; j++) {
		disk_buff.sectbuffer[j] = bswap16(disk_buff.sectbuffer[j]);
	}

	memcpy(buf, disk_buff.sectbuffer, sector_bytes);

	return stat;

}