#include "simj_base.h"

#include <stdio.h>
#include <stdbool.h>

// ---- returns true if empty
void device_common_buffer_set_empty(volatile DEVICE_BUFFER* buff) {

	// --------set nothing to get.
	buff->last_byte_read_index = buff->last_byte_writen_index;
	return;
}