#include <stdio.h>
#include <memory.h>

#include "modcomp_utility_library.h"

// -------- global variable...
static unsigned __int8 sic_884x_EOF_buf[SIC_8840_IMG_EOF_BUFFER_BYTES] = { 0 };

// ===========================================================================
int read_884x_eof_buffer(FILE* fp) {

	__int64 pos;
	__int64 desired_pos;
	int stat;
	size_t return_count;

	desired_pos = SIC_8840_IMG_EOF_BUFFER_U0_START_OFFSET;
	pos = _ftelli64(fp);

	if (pos != desired_pos)
		stat = _fseeki64(fp, desired_pos, SEEK_SET);
		if (stat != 0) {
			printf("\n *** ERROR **** positioning to read EOF buffer %d\n", stat);
		}
	else
		stat = 0;

	if (stat == 0) {
		return_count = fread(&sic_884x_EOF_buf, (size_t)SIC_8840_IMG_EOF_BUFFER_BYTES, 1, fp);
		stat = ferror(fp);
		if (stat != 0) {
			printf("\n *** ERROR **** reading EOF buffer %d\n", stat);
		}
	}

	return stat;

}

// ===========================================================================
#include <stdio.h>
#include <memory.h>

#include "modcomp_utility_library.h"

int write_884x_eof_buffer(FILE* fp) {

	__int64 pos;
	__int64 desired_pos;
	int stat;
	size_t return_count;


	desired_pos = SIC_8840_IMG_EOF_BUFFER_U0_START_OFFSET;
	pos = _ftelli64(fp);

	if (pos != desired_pos)
		stat = _fseeki64(fp, desired_pos, SEEK_SET);
		if (stat != 0) {
			printf("\n *** ERROR **** positioning to write EOF buffer %d\n", stat);
		}
	else
		stat = 0;


	if (stat == 0) {

		return_count = fwrite(&sic_884x_EOF_buf, (size_t)SIC_8840_IMG_EOF_BUFFER_BYTES, 1, fp);
		stat = ferror(fp);
		if (stat != 0) {
			printf("\n *** ERROR **** writing EOF buffer %d\n", stat);
		}
	}

	/* printf("\n first two bytes of image sector : %d", disk_buff.lba); */


	return stat;

}

// ===========================================================================

int sic_884x_check_eof_buffer(__int64 unit, __int64 unit_sector, bool* is_eof) {


	__int64 buffer_byte_offset = 0;
	__int64 buffer_nibble_inx = 0;
	__int64 loc_sector_offset;
	__int64 unit_offset;
	unsigned __int8 check_value;
	int loc_stat = -1;


	// -------- This is what I think it should be....
	unit_offset = unit * SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK / 2;
	loc_sector_offset = unit_sector;
	buffer_byte_offset = loc_sector_offset / 2 + unit_offset;
	buffer_nibble_inx = loc_sector_offset % 2;

	// -------- SIC 68000 is big-endian...
	// -------- low nibble
	if (buffer_nibble_inx != 0) {
		check_value = sic_884x_EOF_buf[buffer_byte_offset] & 0x0f;
	}
	// -------- high nibble
	else {
		check_value = (sic_884x_EOF_buf[buffer_byte_offset] >> 4) & 0x0f;
	}

	// -------- see what we have.....
	if (check_value == 0) {
		*is_eof = false;
		loc_stat = 0;
	}
	else if (check_value == 0x03) {
		*is_eof = true;
		loc_stat = 0;
	}
	else {
		printf("\n\n *** ERROR ***  Value in EOF buffer is not 0 or 3 = %d\n", check_value);
	}

	return loc_stat;
}


// ===========================================================================
int sic_884x_set_eof_buffer(__int64 unit, __int64 unit_sector, bool is_eof) {

	__int64 buffer_byte_offset = 0;
	__int64 buffer_nibble_inx = 0;
	__int64 loc_sector_offset;
	__int64 unit_offset;
	unsigned __int8 cur_value;
	unsigned __int8 set_value;
	unsigned __int8 new_value;
	int loc_stat = 0;


	// -------- This is what I think it should be....
	unit_offset = unit * SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK / 2;
	loc_sector_offset = unit_sector;
	buffer_byte_offset = loc_sector_offset / 2 + unit_offset;
	buffer_nibble_inx = loc_sector_offset % 2;

	cur_value = sic_884x_EOF_buf[buffer_byte_offset];

	set_value = (is_eof ? 0x03 : 0x00);

	// -------- SIC 68000 is big-endian...
	// -------- low nibble
	if (buffer_nibble_inx != 0) {
		new_value = ( cur_value & 0xf0 ) |  set_value;
	}
	// -------- high nibble
	else {
		new_value = (cur_value & 0x0f) | ( set_value << 4 );
	}

	sic_884x_EOF_buf[buffer_byte_offset] = new_value;

	return loc_stat;
}
