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

#include <stdio.h>


// ---------this just allocates the memory, nothing else....
void* device_common_device_buffer_allocate(SIMJ_U16 device_address, size_t buffer_size) {

	SIMJ_U16 loc_dev_addr;

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
		return (void*)iop_device_buffer[loc_dev_addr];
	}
}
