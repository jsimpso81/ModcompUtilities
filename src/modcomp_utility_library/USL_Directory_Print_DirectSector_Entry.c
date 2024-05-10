#include <stdio.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

//	unsigned __int16    unused1 = 0
//	unsigned __int16    unused2 = 0
//	unsigned __int16    unused3 = 0;
//	unsigned __int16    unused4 = 0;
//	unsigned __int16    unused5 = 0;
//	unsigned __int16    unused6 - 0
//	unsigned __int16    sector_of_entry;
//	unsigned __int16    number_of_sectors.
//	unsigned __int16    unused7 = 0


void USL_Directory_Print_DirectSector_Entry(USL_DIRECTORY_ENTRY* direct_entry) {


	printf("Directory Sector entry ----------------------------\n");
	printf("      Starting sector         %6d\n", direct_entry->directory_sector_entry.starting_sector);
	printf("      Number of sectors       %6d\n", direct_entry->directory_sector_entry.sector_count);

}