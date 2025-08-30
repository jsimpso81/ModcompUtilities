

#include "simj_base.h"

#include <stdio.h>
#include <memory.h>

#define RAW_SECTOR_BYTES (size_t)256
#define MODCOMP_EMUL_DISK_IMG_SECTORY_BYTES (size_t)258
typedef struct {
	unsigned __int16     flags;     // ------- could include EOF indicator = 1, otherwise 0
	unsigned __int16     rawsectbuffer[RAW_SECTOR_BYTES / 2];
} MODCOMP_EMUL_DISC_SECTOR;


int device_common_disc_read_sector(FILE* fp, unsigned __int64 sector, void* raw_sector_buf, unsigned __int16* flags) {

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

	memcpy(raw_sector_buf, disk_buff.rawsectbuffer, (size_t)RAW_SECTOR_BYTES);
	*flags = disk_buff.flags;

	return stat;

}