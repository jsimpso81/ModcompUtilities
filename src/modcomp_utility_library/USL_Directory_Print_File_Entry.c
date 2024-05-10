#include <stdio.h>
#include <string.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

//	unsigned __int16    file_name_1;
//	unsigned __int16    file_name_2;
//	unsigned __int16    file_name_3;
//	unsigned __int16    month_day;		// or pgm / cat name 
//  unsigned __int16    year;
//	unsigned __int16    minutes;
//	unsigned __int16    sector_of_entry;
//	unsigned __int16    sector_count;
//	unsigned __int16    words_in_last_sector;


void USL_Directory_Print_File_Entry(USL_FILE_ENTRY* file_entry) {

	printf("File directory entry ----------------------------\n");
	printf("      File name               %s\n", file_entry->file_name);
	if (file_entry->pgm_name_defined) {
		printf("      File pgm/cat            %s\n", file_entry->pgm_name);
	}
	else {
		printf("      Last modified m/d/y     %02d/%02d/%04d  %02d:%02d\n",
			file_entry->month, file_entry->day, file_entry->year, file_entry->hour, file_entry->minute);
	}
	printf("      Starting sector         %6d\n", file_entry->starting_sector);
	printf("      Number of sectors       %6d\n", file_entry->sector_count);
	printf("      Last sector words used  %6d\n", file_entry->last_sector_words_used);

}