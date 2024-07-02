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


void device_common_buffer_init(volatile DEVICE_BUFFER* buff) {

	int j = 0;

	buff->buf_len = DEVICE_BUFFER_MAX_LEN;
	buff->last_byte_read_index = 0;
	buff->last_byte_writen_index = 0;

	for (j = 0; j < DEVICE_BUFFER_MAX_LEN; j++) {
		buff->buffer[j] = 0;
	}
}