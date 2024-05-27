

#include "modcomp_sim_types.h"

void device_common_buffer_init(DEVICE_BUFFER* buff) {

	int j = 0;

	buff->buf_len = DEVICE_BUFFER_MAX_LEN;
	buff->last_byte_read_index = 0;
	buff->last_byte_writen_index = 0;

	for (j = 0; j < DEVICE_BUFFER_MAX_LEN; j++) {
		buff->buffer[j] = 0;
	}
}