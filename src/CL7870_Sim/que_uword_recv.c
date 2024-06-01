
#include <stdbool.h>

#include "modcomp_sim_procedures.h"

bool que_uword_recv( volatile QUEUE_UWORD* que, __int16* cmd_word ) {

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