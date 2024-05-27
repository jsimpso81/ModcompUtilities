#include <stdio.h>
#include <stdbool.h>

#include "modcomp_sim_types.h"

// ---- returns true if empty
device_common_buffer_set_empty(DEVICE_BUFFER* buff) {

	// --------set nothing to get.
	buff->last_byte_read_index = buff->last_byte_writen_index;
	return;
}