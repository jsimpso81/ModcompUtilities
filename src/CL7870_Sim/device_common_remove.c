#include "simj_base.h"

void device_common_remove(SIMJ_U16 device_address) {

	iop_output_data_proc[device_address] = NULL;
	iop_output_cmd_proc[device_address] = NULL;
	iop_input_data_proc[device_address] = NULL;
	iop_input_status_proc[device_address] = NULL;
	HeapFree(GetProcessHeap(), 0, (LPVOID)iop_device_buffer[device_address]);
	iop_device_buffer[device_address] = NULL;

}