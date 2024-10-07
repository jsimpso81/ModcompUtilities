// test_calc_geom.c : This file contains the 'main' function. Program execution begins and ends there.
//
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"


int main()
{
	__int16 stat;
	__int64 log_to_phys[128];
	__int16 j;
	__int64 sector_per_track = 96;
	__int64 geom = 28;
	//     geom_calc( sect_per_track,  geom, log_to_phys);
	stat = geom_calc(sector_per_track, geom, log_to_phys);

	// -------- print array
	printf("\n array logical to physical \n");
	for (j = 0; j < sector_per_track; j++) {
		printf(" logical %d, physical, %ld\n", j, log_to_phys[j]);
	}

}
