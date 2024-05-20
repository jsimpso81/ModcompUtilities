#include <windows.h>
#include <stdio.h>

#include "modcomp_sim_types.h"
#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"


// ---------this just allocates the memory, nothing else....
void* device_common_buffer_allocate(unsigned __int16 device_address, size_t buffer_size) {

	unsigned __int16 loc_dev_addr;

	loc_dev_addr = device_address;

	// --------make certain another device has not been initialized here
	if (iop_device_buffer[device_address] != NULL) {
		printf("\n *** ERROR ***  Device at device address %02x already allocated\n", device_address);
		return NULL;
	}
	else {

		// --------create data structure
		iop_device_buffer[loc_dev_addr] = (void*)HeapAlloc(GetProcessHeap(),
			HEAP_ZERO_MEMORY,
			buffer_size);

		if (iop_device_buffer[device_address] == NULL) {
			printf("\n *** ERROR ***  Could not allocate memory for device address %02x.  Device not created.\n", device_address);
			return NULL;
		}
		return iop_device_buffer[loc_dev_addr];
	}
}
