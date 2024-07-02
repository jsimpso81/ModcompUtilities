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

#include <stdbool.h>

bool que_uword_recv( volatile QUEUE_UWORD* que, SIMJ_U16* cmd_word ) {

	// --------is there anything to do?
	// TODO: see if we need to also check that the pointers are equal.
	if (que->proc_count == que->unproc_count) {
		*cmd_word = 0;
		return false;		// nothing to do.
	}
	else {
		*cmd_word = que->data[que->last_proc_index++];
		que->proc_count++;
		WakeByAddressSingle((LPVOID) & (que->last_proc_index));
	}
	return true;
}