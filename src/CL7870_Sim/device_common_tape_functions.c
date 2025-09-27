// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			device_common_tape_functions.c
//
//	Description:	Functions to process tape .TAP image files.
//
//  Routines:
//			device_common_tape_open
//			device_common_tape_close
//			device_common_tape_rewind
//			device_common_tape_read_record
// 
//	Externally accessible routines:
//					None - only file library routines.
// 
// Internal only routines:
//					None.
//
// Notes:
//	    DWORD last_error may be MS windows specific.
// 
// ================================================================================================
//	Revision history:
//		6/28/2024	JAS		Added new header
// ================================================================================================

#include "simj_base.h"

// DEBUG_TAPE_COMMON is set > 0 for debugging

// -------- open a raw tcp socket, listen for connections, and accept a connection.
int device_common_tape_open(char* tape_filename, bool read_only, FILE** tape_file_handle, 
                            SIMJ_TAPE_DPI* tape_dpi, SIMJ_TAPE_ERR* last_error) {


    errno_t loc_err_status;     // status from library calls...
    char* loc_io_mode = NULL;      // open mode  

    // -------- open input tape image file.
    // -------- read only
    if (read_only) {
        loc_io_mode = "rb";
    }
    // -------- read / write
    else {
        // TODO: for now the tape file must exist first to write....
        loc_io_mode = "r+b";
    }
    // --------open the tape...
    loc_err_status = fopen_s(tape_file_handle, tape_filename, loc_io_mode);

    // --------good status is zero.  Otherwise print error.
    if (loc_err_status != 0) {
        fprintf(stderr, " *** ERROR *** device_common_tape_open.  Could not open tape image file: %d, %s\n\n", loc_err_status,tape_filename);
        *tape_file_handle = NULL;
        *tape_dpi = 0;
        *last_error = loc_err_status;
        return 1;
    }

    // --------do a rewind....may not be needed.
    // TODO: add rewind.

    // --------return the socket info and set no error
    *tape_dpi = 0;
    *last_error = 0;
    return 0;
}


// ==========================================================================================
// --------local function to read tape header...
int device_common_tape_read_header(
	FILE** tape_file_handle,
	SIMJ_TAPE_DPI* current_file_position,
	SIMJ_U32* record_bytes,
	bool* end_of_file) {

	SIMJ_U16 header_footer_tape_record[2] = { 0, 0 };
	size_t return_count = 0;	// number of elements returned...

	SIMJ_U32 temp_low = 0;
	SIMJ_U32 temp_high = 0;

	SIMJ_U32 record_header_value = 0;

	// -------- read record header.  It could also be a file mark.
	size_t buffer_size = 4;
	size_t element_size = 1;	// bytes..
	size_t element_count = 4;
	return_count = fread_s(&(header_footer_tape_record[0]), buffer_size,
		element_size, element_count, *tape_file_handle);

	// --------some kind of error occured.  See if it is end of file.
	if (return_count != element_count) {
		int int_eof = feof(*tape_file_handle);
		// --------yes end of file
		if (int_eof != 0) {
			*record_bytes = 0;
			*end_of_file = true;
			return 0;	// consider this a good return...???
		}
		else {
			// err_stat = ferror(fp);
			*record_bytes = 0;
			*end_of_file = true;
			return 1;	// something bad happened.		}
		}
	}

	// --------we have a good read.  translate the data...endianess

	temp_low = header_footer_tape_record[0];
	temp_high = header_footer_tape_record[1];
	record_header_value = ((temp_high << 16) & 0xffff0000) | (temp_low & 0x0000ffff);

	*record_bytes = record_header_value & 0x00ffffff;
	*current_file_position += 4;
	if (*record_bytes == 0) {
		*end_of_file = true;
	}
	else {
		*end_of_file = false;
	}
	return 0;
}

// ==========================================================================================
// --------read the next sequential tape record...
int device_common_tape_read_record(
			FILE** tape_file_handle, SIMJ_TAPE_DPI* current_file_position,
			void* buf, SIMJ_U32 max_buf_bytes, SIMJ_U32* bytes_read,
			bool* end_of_file) {

	SIMJ_U32 loc_header_record_bytes = 0;
	SIMJ_U32 loc_footer_record_bytes = 0;
	SIMJ_U32 j = 0;
	int err_stat = 1;
	// int io_error = 9999;

	// -------- make sure we have a valid file handle
	if (*tape_file_handle == NULL) {
		*bytes_read = 0;
		*end_of_file = true;
		return 1;
	}

	// --------read the header record.
	loc_header_record_bytes = 0;
	err_stat = device_common_tape_read_header(tape_file_handle, current_file_position,
		&loc_header_record_bytes, end_of_file);
	// --------debug
#if DEBUG_TAPE_COMMON > 0
	printf(" --- tape image read -- record header read record_bytes %d, error stat %d, eof %d\n", 
				loc_header_record_bytes, err_stat, *end_of_file);
#endif

	// --------some kind of error occured.
	if (err_stat != 0) {
		*bytes_read = 0;
		return 1;
	}
	// --------got an end of file or end of file record.
	else if (*end_of_file) {
		*bytes_read = 0;
		return 0;
	}

	// --------have a real data record, read it..

	size_t element_count = loc_header_record_bytes;
	// --------for whatever reason the data is stored as an even number of bytes..
	if (element_count % 2 != 0)
		element_count++;
	size_t element_size = 1;	// bytes..
	size_t buffer_size = element_count;
	size_t return_count = 0;	// number of elements returned...

	// --------see if our buffer can hold the data
	if (max_buf_bytes < element_count) {
		printf(" *** ERROR ***  Tape buffer not large enough for record. rec size %zd, buf size %d\n",
			element_count, max_buf_bytes);
		*bytes_read = 0;
		return 1;
	}

	// -------- read tape record 
	// --------debug
#if	DEBUG_TAPE_COMMON > 0
	printf(" --- tape image before read -- buf size %zd, ele size %zd, ele cnt %zd\n",
			buffer_size, element_size, element_count);
#endif
	return_count = fread_s(buf, buffer_size, element_size,
		element_count, (FILE*)*tape_file_handle);
	// --------debug
#if	DEBUG_TAPE_COMMON > 0
	printf(" --- tape image read -- bytes read %zd\n", return_count);
#endif

	//io_error = ferror(tape_file_handle);
	*end_of_file = feof((FILE*)*tape_file_handle);

	// --------everything was returned...
	// --------return non adjusted byte count..
	if (return_count == element_count) {
		*current_file_position += element_count;
		*bytes_read = loc_header_record_bytes;

		// --------swap bytes !!!
		for (j = 0; j < (return_count / 2); j++) {
			((SIMJ_U16*)buf)[j] = bswap16(((SIMJ_U16*)buf)[j]);
		}
	}

	// --------read trailing footer record.
	loc_footer_record_bytes = 0;
	err_stat = device_common_tape_read_header(tape_file_handle, current_file_position,
		&loc_footer_record_bytes, end_of_file);
	// --------debug
#if	DEBUG_TAPE_COMMON > 0
	printf(" --- tape image read -- record footer read record_bytes %d, error stat %d, eof %d\n", 
			loc_footer_record_bytes, err_stat, *end_of_file);
#endif

	// --------some kind of error occured.  bad footer
	if (err_stat != 0) {
		*bytes_read = 0;
		return 1;
	}
	// --------got an end of file or end of file record.
	// --------really shouldn't get this on footer!!!
	else if (*end_of_file) {
		*bytes_read = 0;
		return 1;
	}

	if (loc_header_record_bytes != loc_footer_record_bytes)
		printf(" *** ERROR *** Tape device record size mis-match on tape image %d not equal to %d\n",
			loc_header_record_bytes, loc_footer_record_bytes);

	//io_error = ferror(tape_file_handle);
	*end_of_file = feof((FILE*)*tape_file_handle);

	return 0;
}


// ==========================================================================================

// -------- close the tape image file..  good status = 0
int device_common_tape_close(FILE** tape_file_handle, SIMJ_TAPE_DPI* tape_dpi) {

	if (*tape_file_handle == NULL) {
		return 1;
	}

	// --------close the tape image file.  ignore the error.
	fclose(*tape_file_handle);
	*tape_dpi = 0;
	*tape_file_handle = NULL;
	return 0;
}

// ==========================================================================================

// -------- open a raw tcp socket, listen for connections, and accept a connection.
int device_common_tape_rewind(FILE** tape_file_handle, SIMJ_TAPE_DPI* tape_dpi) {
#if	DEBUG_TAPE_COMMON > 0
	printf(" tape_common - rewind - called\n");
#endif

	int stat_err = 0;

	// --------see if we have an open file.  If not, return error.
	if (*tape_file_handle == NULL) {
#if	DEBUG_TAPE_COMMON > 0
		printf(" tape_common - rewind - bad file handle\n");
#endif
		return 1;
	}

	// --------set pos at beginning of file.
	*tape_dpi = 0;
	stat_err = _fseeki64(*tape_file_handle, *tape_dpi, SEEK_SET);
	if (stat_err != 0) {
#if	DEBUG_TAPE_COMMON > 0
		printf(" tape_common - rewind - bad return from seek, stat %d \n", stat_err);
#endif
		return 1;
	}
	*tape_dpi = _ftelli64(*tape_file_handle);
#if	DEBUG_TAPE_COMMON > 0
	printf(" tape_common - rewind - new tape dpi, stat %lld \n", *tape_dpi);
#endif
	return 0;
}

// ==========================================================================================

// -------- 
// TODO: tape_advance_record(file* fp, int 64 dpi, bool* eof)	does this go past eof ?
// ==========================================================================================

// -------- 
// TODO: tape_advance_file(file * fp, int 64 * dpi)


// ==========================================================================================
// -------- tape back record...
int tape_back_record(FILE** tape_file_handle, SIMJ_TAPE_DPI* tape_dpi, bool* end_of_file, bool* begin_of_tape) {

	__int64 current_pos = 0;
	__int64 offset_bytes = 0;
	int seek_stat = 0;
	int header_stat = 0;
	SIMJ_U32 record_bytes = 0;

	// -------- get current position
	current_pos = _ftelli64(*tape_file_handle);
	*tape_dpi = current_pos;
#if DEBUG_TAPE > 0
	printf(" Tape BCK REC - current position %zd\n", current_pos);
#endif

	// -------- are we already at the beginning of the tape.
	// -------- nothing to do,,
	if (current_pos <= 0) {
#if DEBUG_TAPE > 0
		printf(" Tape BCK REC - beginning of tape\n");
#endif
		*end_of_file = false;
		*begin_of_tape = true;
		return 0;
	}

	// -------- backup 4 bytes
	offset_bytes = -4;
	seek_stat = _fseeki64(*tape_file_handle, offset_bytes, SEEK_CUR);
	if (seek_stat != 0) {
#if DEBUG_TAPE > 0
		printf(" Tape BCK REC - fseeki64 error %d\n", seek_stat);
#endif
		*end_of_file = false;
		*begin_of_tape = false;
		return 1;
	}
	current_pos = _ftelli64(*tape_file_handle);
	*tape_dpi = current_pos;
#if DEBUG_TAPE > 0
	printf(" Tape BCK REC - position after move backwards. %zd\n", current_pos);
#endif

	// -------- read the file footer record.  calc record size.
	header_stat = device_common_tape_read_header( tape_file_handle, tape_dpi, &record_bytes, end_of_file);
	if (header_stat != 0) {
#if DEBUG_TAPE > 0
		printf(" Tape BCK REC - read footer error %d\n", header_stat);
#endif
		*end_of_file = false;
		*begin_of_tape = false;
		return 1;
	}

	// -------- are we at the begining of the tape.
	// -------- this is an error since there should have been a record here..
	if (*tape_dpi <= 0) {
#if DEBUG_TAPE > 0
		printf(" Tape BCK REC - beginning of tape\n");
#endif
		*end_of_file = false;
		*begin_of_tape = true;
		return 1;
	}

	// --------check for file mark.
	// --------move back to the file mark.
	if (record_bytes == 0) {
		offset_bytes = -4;
		*end_of_file = true;
	}
	// --------move to the beginning of the record.
	else {
		offset_bytes = (record_bytes + 8) * -1;
		*end_of_file = false;
	}

	// -------- backup record_size (made even) + 4 header bytes + 4 footer bytes.
	seek_stat = _fseeki64(*tape_file_handle, offset_bytes, SEEK_CUR);
	if (seek_stat != 0) {
#if DEBUG_TAPE > 0
		printf(" Tape BCK REC - fseeki64 error %d\n", seek_stat);
#endif
		*end_of_file = false;
		*begin_of_tape = false;
		return 1;
	}

	// -------- get current position
	current_pos = _ftelli64(*tape_file_handle);
	*tape_dpi = current_pos;
#if DEBUG_TAPE > 0
	printf(" Tape BCK REC - current position %zd\n", current_pos);
#endif

	// -------- are we at the beginning of the date.
	if (current_pos <= 0) {
		*begin_of_tape = true;
	}
	return 0;
}
 
 
// ==========================================================================================

// -------- 
// TODO: tape_back_file
// ==========================================================================================

// -------- 
// TODO: tape_write_next_record(
// ==========================================================================================

// -------- 
// TODO: tape_write_eof(file * fp, int64 * dpi)


