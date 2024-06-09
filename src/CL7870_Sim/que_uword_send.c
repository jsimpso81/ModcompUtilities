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