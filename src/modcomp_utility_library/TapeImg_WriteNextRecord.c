#include <stdio.h>

#include "modcomp_utility_library.h"


int TapeImg_write_next_record(FILE* fp, void* buf, int buf_bytes, size_t* bytes_written) {

	int stat;
	unsigned __int16 header_tape_record[2] = { 0, 0 };
	size_t return_count = 0;
	size_t record_size = 0;
	char* buf_char;
	__int16* buf_int;
	unsigned __int16 wrd0 = 0;
	unsigned __int16 wrd1 = 0;

	unsigned __int32 temp_low = 0;
	unsigned __int32 temp_high = 0;

	unsigned _int32 begin_record_header = 0;

	unsigned _int32 tape_record_bytes = 0;

	// -------- find out how many real bytes are in this record.
	// --------  If first two bytes are 0x0300 then this is standard binary.
	// --------  bytes 4 and 5 are the byte length of record.  Write that many
	// --------  bytes.  
	// --------  If the buffer has $$ as the first two bytes, this is an EOF
	// --------- Set the data length to 0.  Else write the entire buffer..
	tape_record_bytes = buf_bytes;
	buf_char = (char*)buf;
	buf_int = (__int16*)buf;
	wrd0 = bswap16(buf_int[0]);
	wrd1 = bswap16(buf_int[1]);
	printf(" --- buffer beginning char1,char2, int1, int3 = 0x%04X 0x%04X 0x%04hX 0x%04hX \n",
		buf_char[0], buf_char[1], wrd0, wrd1 );
	// -------- end of file
	if (buf_char[0] == '$' && buf_char[1] == '$') {
		tape_record_bytes = 0;
		printf(" ### EOF found \n");
	}
	// -------- standard binary
	else if ( (wrd0 & 0xff00) == 0x0300) {
		if (wrd1 <= buf_bytes) {
			tape_record_bytes = wrd1;
		}
		else {
			tape_record_bytes = buf_bytes;
		}
		printf(" ### STD binary found.len = %d \n", wrd1);
	}

	// -------- size must be even...
	if (tape_record_bytes % 2 != 0)
		tape_record_bytes++;

	begin_record_header = tape_record_bytes & 0x00ffffff;
	temp_low = begin_record_header & 0x0000ffff;
	temp_high = (begin_record_header >> 16 ) & 0x000000ff;
	header_tape_record[0] = temp_low;
	header_tape_record[1] = temp_high;

	/* -------- write record header bytes */
	// --------form the tape header record.
	return_count = fwrite(&header_tape_record, 4, 1, fp);
	stat = ferror(fp);
	printf(" --- tape image written -- begin record header %d, stat %d\n",
				begin_record_header,stat);

	if (tape_record_bytes > 0) {

		record_size = tape_record_bytes;

		/* -------- write tape record */
		return_count = fwrite(buf, record_size, 1, fp);
		stat = ferror(fp);
		printf(" --- tape image write -- record size %zd, stat %d\n", record_size, stat);

		/* -------- write record trailer bytes */
		// --------form the tape trailer record.
		return_count = fwrite(&header_tape_record, 4, 1, fp);
		stat = ferror(fp);
		printf(" --- tape image written -- trailing record header %d, stat %d\n",
			begin_record_header, stat);

	}


	return stat;

}