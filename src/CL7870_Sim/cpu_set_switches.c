#include "simj_base.h"



void cpu_set_switches(SIMJ_U16 switch_value) {

	gbl_fp_switches = switch_value;
	cpu_set_register_value(0, switch_value);

}