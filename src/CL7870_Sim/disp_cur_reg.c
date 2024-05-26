#include <stdio.h>
#include <stdbool.h>

#include "modcomp_sim_types.h"


void disp_cur_reg() {

	printf(" Current register block\n");
	printf("       0  |  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n",
		cpu_get_register_value(0),
		cpu_get_register_value(1),
		cpu_get_register_value(2),
		cpu_get_register_value(3),
		cpu_get_register_value(4),
		cpu_get_register_value(5),
		cpu_get_register_value(6),
		cpu_get_register_value(7)
	);
	printf("       8  |  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n",
		cpu_get_register_value(8),
		cpu_get_register_value(9),
		cpu_get_register_value(10),
		cpu_get_register_value(11),
		cpu_get_register_value(12),
		cpu_get_register_value(13),
		cpu_get_register_value(14),
		cpu_get_register_value(15)
	);

}
