#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "modcomp_sim_external_globals.h"


void disp_devices() {

	DEVICE_GENERIC_DATA* ptr_generic_data = NULL;
	int j = 0;

	printf("\nDevice Information\n");
	printf(" DevAddr   Status    Bus     Pri    Dmp    Type\n");

	// --------loop over all devices...
	for (j = 0; j < 64; j++) {

		if (iop_device_buffer[j] != NULL) {
			ptr_generic_data = iop_device_buffer[j];

			printf("    %2d     0x%04x     %2d      %2d     %2d    %2s\n",
				ptr_generic_data->device_address, ptr_generic_data->ctrl_status,
				ptr_generic_data->bus, ptr_generic_data->pri, ptr_generic_data->dmp, ptr_generic_data->info);
		}

	}
}
