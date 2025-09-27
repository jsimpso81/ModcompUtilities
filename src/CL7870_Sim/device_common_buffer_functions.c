// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			device_common_buffer_functions.c
//
//	Description:	Routines to xxxxxxx.
// 
//  Routines:
//			void device_common_buffer_init(volatile DEVICE_BUFFER* buff)
//			bool device_common_buffer_get(volatile DEVICE_BUFFER* buff, SIMJ_U8* to_get)
//			bool device_common_buffer_isempty(volatile DEVICE_BUFFER* buff)
//			bool device_common_buffer_isfull(volatile DEVICE_BUFFER* buff)
//			void device_common_buffer_put(volatile DEVICE_BUFFER* buff, SIMJ_U8 to_put)
//			void device_common_buffer_set_empty(volatile DEVICE_BUFFER* buff)
// 
//  Internal routines:
//
//	Externally accessible routines:
//					XXXXXXX
// 
// Internal only routines:
//					XXXXXXX
//
// Notes:
//		XXXXXX
// 
// ================================================================================================
//	Revision history:
//		6/28/2024	JAS		Added new header
// ================================================================================================

#include "simj_base.h"

#include <stdio.h>
#include <stdbool.h>

void device_common_buffer_init(volatile DEVICE_BUFFER* buff) {

	int j = 0;

	buff->buf_len = DEVICE_BUFFER_MAX_LEN;
	buff->last_byte_read_index = 0;
	buff->last_byte_writen_index = 0;

	for (j = 0; j < DEVICE_BUFFER_MAX_LEN; j++) {
		buff->buffer[j] = 0;
	}
}

// ================================================================================================

// ---- returns true if new data was read.
bool device_common_buffer_get(volatile DEVICE_BUFFER* buff, SIMJ_U8* to_get) {

	unsigned int next_inx = 0;
	bool ret_value = false;

	// --------is there anything to get.
	if (!device_common_buffer_isempty(buff)) {
		next_inx = buff->last_byte_read_index + 1;
		if (next_inx >= DEVICE_BUFFER_MAX_LEN)
			next_inx = 0;
		*to_get = buff->buffer[next_inx];
		buff->last_byte_read_index = next_inx;
		ret_value = true;
	}
	return ret_value;
}

// ================================================================================================

// ---- returns true if empty
bool device_common_buffer_isempty(volatile DEVICE_BUFFER* buff) {

	bool ret_value = false;

	// --------is there anything to get.
	if (buff->last_byte_read_index == buff->last_byte_writen_index) {
		ret_value = true;
	}
	return ret_value;
}

// ================================================================================================

// --------returns true if ful
bool device_common_buffer_isfull(volatile DEVICE_BUFFER* buff) {

	unsigned int next_inx = 0;
	bool ret_value = false;

	// --------there is one extra byte available but this implementation is easier.

	next_inx = buff->last_byte_writen_index + 1;
	if (next_inx >= DEVICE_BUFFER_MAX_LEN)
		next_inx = 0;

	if (next_inx == buff->last_byte_read_index)
		ret_value = true;

	return ret_value;
}

// ================================================================================================

void device_common_buffer_put(volatile DEVICE_BUFFER* buff, SIMJ_U8 to_put) {

	unsigned int next_inx = 0;

	// --------there is one extra byte available but this implementation is easier.

	next_inx = buff->last_byte_writen_index + 1;
	if (next_inx >= DEVICE_BUFFER_MAX_LEN)
		next_inx = 0;

	if (next_inx != buff->last_byte_read_index) {
		buff->buffer[next_inx] = to_put;
		buff->last_byte_writen_index = next_inx;
	}
	else {
		printf("\n *** ERROR ***  Buffer full - could not add to buffer.\n");
	}

}

// ================================================================================================
void device_common_buffer_set_empty(volatile DEVICE_BUFFER* buff) {

	// --------set nothing to get.
	buff->last_byte_read_index = buff->last_byte_writen_index;
	return;
}