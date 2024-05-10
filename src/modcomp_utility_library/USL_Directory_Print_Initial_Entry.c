#include <stdio.h>
#include <string.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

//	unsigned __int16    entries_per_sector;
//	unsigned __int16    entries_in_use;
//	unsigned __int16    entries_unused;
//	unsigned __int16    raw_programs_on_USL;  /* 0x8000 usl attached, 0x7fff programs on USL */
//	unsigned __int16    USL_ID_1;
//	unsigned __int16    USL_ID_2;
//	unsigned __int16    USL_ID_3;
//	unsigned __int8     day_modified;
//	unsigned __int8     month_modified;
//	unsigned __int16    year_modified;


void USL_Directory_Print_Initial_Entry(USL_DIRECTORY_ENTRY* direct_entry) {

	unsigned char cancode1[4] = { 0, 0, 0, 0 };
	unsigned char cancode2[4] = { 0, 0, 0, 0 };
	unsigned char cancode3[4] = { 0, 0, 0, 0 };
	unsigned char temp_string[10] = { 0 };

	strcpy_s(cancode1, 4, from_can_code(direct_entry->initial_entry.usl_id_1, temp_string));
	strcpy_s(cancode2, 4, from_can_code(direct_entry->initial_entry.usl_id_2, temp_string));
	strcpy_s(cancode3, 4, from_can_code(direct_entry->initial_entry.usl_id_3, temp_string));


	printf("Initial directory entry ----------------------------\n");
	printf("      Entried per sector      %6d\n", direct_entry->initial_entry.entries_per_sector);
	printf("      Entries in use          %6d\n", direct_entry->initial_entry.entries_in_use);
	printf("      Entries unused          %6d\n", direct_entry->initial_entry.entries_unused);
	printf("      USL attached            %s\n", ((direct_entry->initial_entry.raw_programs_on_USL & 0x8000) == 0) ? "NOT attached" : "ATTACHED");
	printf("      Programs on USL         %6d\n", (direct_entry->initial_entry.raw_programs_on_USL & 0x7fff));
	printf("      USL ID                  %s%s%s\n", cancode1, cancode2, cancode3);
	printf("      Last modified m/d/y     %02d/%02d/%04d\n",
		direct_entry->initial_entry.month_modified,
		direct_entry->initial_entry.day_modified,
		direct_entry->initial_entry.year_modified);

}