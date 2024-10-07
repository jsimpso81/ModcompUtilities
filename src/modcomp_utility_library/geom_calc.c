#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"


__int16 geom_calc(__int64 sect_per_track, __int64 geom, __int64 log_to_phys[]) {

	__int64 used_sect[128] = { 0 };
	__int16 j = 0;
	__int16 j1 = 0;
	__int64 last_used;

	if (sect_per_track < 1 || sect_per_track > 128) {
		return -1;
	}

	if (geom < 1 || geom > 32) {
		return -2;
	}

	// -------- initialize unused_sect array
	for (j = 0; j < sect_per_track; j++) {
		used_sect[j] = j;
		log_to_phys[j] = -1;
	}

	last_used = geom * -1;
	for (j = 0; j < sect_per_track; j++) {
		last_used += geom;
		last_used %= (sect_per_track-1);
		if (used_sect[last_used] >= 0) {
			log_to_phys[j] = used_sect[last_used];
			used_sect[last_used] = -1;
		}
		else {
			// --------find next unused sector....
			for (j1 = 0; j1 < sect_per_track; j1++) {
				last_used++;
				last_used %= (sect_per_track );
				if (used_sect[last_used] >= 0) {
					log_to_phys[j] = used_sect[last_used];
					used_sect[last_used] = -1;
					break;
				}
			}
		}

	}

	// -------- print array
	// -- printf("\n array logical to physical \n");
	// -- for (j = 0; j < sect_per_track; j++) {
	// -- 	printf(" logical %d, physical, %d\n", j, log_to_phys[j]);
	// -- }

	// -------- print unused
	// -- printf("\n array uused \n");
	// -- for (j = 0; j < sect_per_track; j++) {
	// -- 	printf(" index %d, used %d\n", j, used_sect[j]);
	// -- }

	return 0;
}