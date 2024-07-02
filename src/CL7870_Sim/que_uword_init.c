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


void que_uword_init(volatile QUEUE_UWORD* que) {

	int j;
	for (j = 0; j < 256; j++) {
		que->data[j] = 0;
	}

	que->last_proc_index = 0;
	que->next_in_index = 0;
	que->proc_count = 0;
	que->unproc_count = 0;

}