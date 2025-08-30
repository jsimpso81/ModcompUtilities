#include <stdio.h>

#include "modcomp_utility_library.h"


int read_raw_disk_sector_lba(FILE* fp, __int64 sector_number, size_t  sector_count, void *buf, size_t* return_bytes, int* end_of_file  ) {

	__int64 pos;
	__int64 desired_pos;
	int stat;

	desired_pos = sector_number * (__int64)RAW_SECTOR_BYTES;
	pos = _ftelli64(fp);

	if (pos != desired_pos)
		stat = _fseeki64(fp, desired_pos, SEEK_SET);
	else
		stat = 0;

	if (stat == 0) {
		*return_bytes = fread(buf, 1, RAW_SECTOR_BYTES * sector_count, fp);
		stat = ferror(fp);
		*end_of_file = feof(fp);
	}

	return stat;

}