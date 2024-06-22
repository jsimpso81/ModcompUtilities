#include "simj_base.h"

#include <stdio.h>

void cmd_process_print_prompt() {
#if  SIMJ_SIM_CPU == 7860
	printf("CL7860>");
#elif  SIMJ_SIM_CPU == 7830
	printf("CL7830>");
#elif  SIMJ_SIM_CPU == II15
	printf("II/15>");
#else
#error SIMJ_SIM_CPU Had invalid cpu model.
#endif
}