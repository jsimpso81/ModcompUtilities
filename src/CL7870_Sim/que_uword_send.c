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


bool que_uword_send(volatile QUEUE_UWORD* queue, SIMJ_U16 value) {

	SIMJ_U16 loc_next_in_index;

	// --------wait if necessary ---- BE CAREFUL, THIS WAITS FOREVER!
	if ((queue->next_in_index == (loc_next_in_index = queue->last_proc_index)) && (queue->unproc_count != queue->proc_count)) {
		WaitOnAddress(
			&(queue->last_proc_index), &loc_next_in_index, 
			sizeof(queue->last_proc_index), INFINITE);
	}

	// --------index will wrap.
	queue->data[queue->next_in_index++] = value;
	queue->unproc_count++;

	return false;
}