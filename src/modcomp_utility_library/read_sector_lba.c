#include <stdio.h>

#include "modcomp_utility_library.h"


int read_sector_lba(FILE* fp, __int64 sector, size_t  sector_count, void *buf, size_t* return_count, int* end_of_file  ) {

	__int64 pos;
	__int64 desired_pos;
	int stat;

	desired_pos = sector * (__int64)sector_bytes;
	pos = _ftelli64(fp);

	if (pos != desired_pos)
		stat = _fseeki64(fp, desired_pos, SEEK_SET);
	else
		stat = 0;

	if (stat == 0) {
		*return_count = fread(buf, sector_bytes, sector_count, fp);
		stat = ferror(fp);
		*end_of_file = feof(fp);
	}

	return stat;

}