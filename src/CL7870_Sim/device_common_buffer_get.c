// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			XXXX.c
//
//	Description:	Routines to xxxxxxx.
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

// ---- returns true if new data was read.
bool device_common_buffer_get(volatile DEVICE_BUFFER* buff, SIMJ_U8* to_get) {

	unsigned int next_inx = 0;
	bool ret_value = false;

	// --------is there anything to get.
	if ( !device_common_buffer_isempty(buff) ) {
		next_inx = buff->last_byte_read_index + 1;
		if (next_inx >= DEVICE_BUFFER_MAX_LEN)
			next_inx = 0;
		*to_get = buff->buffer[next_inx];
		buff->last_byte_read_index = next_inx;
		ret_value = true;
	}
	return ret_value;
}