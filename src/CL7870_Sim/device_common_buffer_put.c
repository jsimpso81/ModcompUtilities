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

void device_common_buffer_put(volatile DEVICE_BUFFER* buff, SIMJ_U8 to_put ) {

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
		printf("\n Buffer full - could not add to buffer.\n");
	}

}