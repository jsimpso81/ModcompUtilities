#include <stdio.h>
#include <stdbool.h>

#include "modcomp_sim_types.h"
#include "modcomp_sim_procedures.h"

// ---- returns true if new data was read.
bool device_common_buffer_get(DEVICE_BUFFER* buff, unsigned __int8* to_get) {

	unsigned int next_inx = 0;
	bool ret_value = false;

	// --------is there anything to get.
	if ( !device_common_buffer_isempty(buff) ) {
		next_inx = buff->last_byte_read_index + 1;
		if (next_inx >= DEVICE_BUFFER_MAX_LEN)
			next_inx = 0;
		*to_get = buff->buffer[next_inx];
		buff->last_byte_read_index = next_inx;
		ret_value = true;
	}
	return ret_value;
}