#include "simj_base.h"

#include "cpu_macro_register_access.h"
#include "cpu_instruction_memory_macros.h"

#include <stdio.h>

#include <stdbool.h>
#include <string.h>


void disp_virtual_map(FILE* io_unit, SIMJ_U16 map ) {

	const char* acc_str[] = { "None", "Read", "Exec", "RWE" };
	int j = 0;

	MEM_MAP loc_map;

	if (map > 7) {
		fprintf(io_unit, " *** ERROR *** Map number not in range 0 - 7.  Map number: %d\n", map);
		return;
	}

	// -------- get memory map data.
	cpu_get_virtual_map(map, &loc_map);

	fprintf(io_unit, "\nVirtual Memory Hardware Map Number: %d\n",map);
	fprintf(io_unit, "   Entry    Access     Shared     Map Page    All \n");

	// --------loop over stack;
	for (j = 0; j < 256; j++) {

		fprintf(io_unit, "   %3d     %s     0x%01x      0x%04x    0x%04x \n",
			j, acc_str[loc_map.entry[j].parts.acc], loc_map.entry[j].parts.shared, loc_map.entry[j].parts.mem_page, loc_map.entry[j].all);

	}
}
