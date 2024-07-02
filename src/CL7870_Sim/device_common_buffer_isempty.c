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

// ---- returns true if empty
bool device_common_buffer_isempty(volatile DEVICE_BUFFER* buff) {

	bool ret_value = false;

	// --------is there anything to get.
	if (buff->last_byte_read_index == buff->last_byte_writen_index) {
		ret_value = true;
	}
	return ret_value;
}