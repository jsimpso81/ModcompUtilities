#include "modcomp_sim_external_globals.h"
#include "modcomp_sim_procedures.h"


void cpu_set_switches(unsigned __int16 switch_value) {

	gbl_fp_switches = switch_value;
	cpu_set_register_value(0, switch_value);

}