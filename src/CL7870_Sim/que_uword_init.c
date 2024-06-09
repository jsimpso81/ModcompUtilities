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