
#include "simj_base.h"

void cpu_do_fill(SIMJ_U16 new_switch_value) {

	SIMJ_U16 fill_device_address;

	// -------- ensure we can do a fill
	if (!cpu_get_power_on()) {
		printf(" *** ERROR ***  Cant perform fill.  CPU is not powered on.\n");
	}
	else if (gbl_fp_runlight) {
		printf(" *** ERROR ***  Cant perform fill.  CPU is not halted.\n");
	}
	else if (gbl_fp_single_step) {
		printf(" *** ERROR ***  Cant perform fill.  CPU is being single stepped.\n");
	}
	else {

		fill_device_address = new_switch_value & 0x003f;		// I think that fills only word for devices 0-15...

		cpu_set_switches(new_switch_value);

		gbl_mem[0] = 0x4010 | fill_device_address;
		gbl_mem[1] = 0x4840 | fill_device_address;
		gbl_mem[2] = 0x7648;
		gbl_mem[3] = 0x0000;
		gbl_mem[4] = 0x4c40 | fill_device_address;
		gbl_mem[5] = 0xaf42;
		gbl_mem[6] = 0x7000;
		cpu_set_register_value(1, 0x8800);
		cpu_set_register_value(2, 0x002e);
		cpu_set_register_value(3, 0xffb1);
		cpu_set_register_value(4, 0xbd8f);		// unused 
		cpu_set_register_value(5, 0);
		cpu_set_register_value(6, 0);
		cpu_set_register_value(7, 0);
	}

}