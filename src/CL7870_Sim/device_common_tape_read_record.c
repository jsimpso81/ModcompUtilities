#include "simj_base.h"
#include <stdio.h>


// 
int device_common_tape_read_record(FILE* fp, 
						__int64* current_file_position, 
						SIMJ_U16* buf, 
						int max_buf_bytes,
						size_t* bytes_read, 
						int* end_of_file) {

	__int64 pos;
	int stat  = 0;
	unsigned __int16 begin_tape_record[2] = { 0, 0 };
	unsigned __int16 end_tape_record[2] = { 0, 0 };
	size_t return_count = 0;
	size_t record_size = 0;

	unsigned __int32 temp_low = 0;
	unsigned __int32 temp_high = 0;

	unsigned _int32 begin_record_header = 0;
	unsigned _int32 end_record_header = 0;
	unsigned _int32 record_bytes = 0;
	unsigned int j = 0;

	/* -------- initialize bytes read */
	*bytes_read = 0;

	/* --------get current position in file ----- we really shouldn't need to move*/
	pos = _ftelli64(fp);

	//if (pos != *current_file_position) {
	//	stat = _fseeki64(fp, *current_file_position, SEEK_SET);
	//}
	//else {
	//	stat = 0;
	//}

	if (stat == 0) {

		/* -------- read record header bytes */
		return_count = fread(&begin_tape_record, 4, 1, fp);
		temp_low = begin_tape_record[0];
		temp_high = begin_tape_record[1];
		begin_record_header = ((temp_high << 16) & 0xffff0000) | (temp_low & 0x0000ffff);
		record_bytes = begin_record_header & 0x00ffffff;

		// --debug-- printf(" --- tape image read -- begin record header %d, returned records %zd\n", begin_record_header, return_count);
		*current_file_position += 4;
		stat = ferror(fp);
		*end_of_file = feof(fp);

		if (stat == 0) {
			if (record_bytes > 0) {

				record_size = record_bytes;
				if (record_size % 2 != 0)
					record_size++;

				/* -------- read tape record */
				return_count = fread(buf, record_size, 1, fp);
				// --debug-- printf(" --- tape image read -- begin record size %zd, returned records %zd\n", record_size, return_count);
				if ( return_count == 1 )
					*current_file_position += record_size;
				stat = ferror(fp);
				*end_of_file = feof(fp);
				if (return_count == 1) {
					*bytes_read = record_bytes;

					for (j = 0; j < (record_size / 2); j++) {
						buf[j] = bswap16(buf[j]);
					}

				}


				if (stat == 0) {
					/* -------- read tape record trailer */
					return_count = fread(&end_tape_record, 4, 1, fp);
					temp_low = end_tape_record[0];
					temp_high = end_tape_record[1];
					end_record_header = ((temp_high << 16) & 0xffff0000) | (temp_low & 0x0000ffff);
					// --debug-- printf(" --- tape image read -- end record header %d, returned records %zd\n", end_record_header, return_count);
					if ( return_count == 1)
						*current_file_position += 4;
					if (end_record_header != begin_record_header)
						printf("\n *** ERROR *** record size mis-match on tape image %d not equal to %d\n", begin_record_header, end_record_header);
					stat = ferror(fp);
					*end_of_file = feof(fp);
				}
				else {
					*bytes_read = 0;
				}
			}
		}
		else {
			*bytes_read = 0;
		}
	}

	return stat;

}