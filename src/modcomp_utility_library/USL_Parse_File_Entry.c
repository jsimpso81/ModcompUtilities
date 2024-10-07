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


void USL_Parse_File_Entry(USL_DIRECTORY_ENTRY* direct_entry, USL_FILE_ENTRY* parsed_file_entry) {

	unsigned char cancode1[4] = { 0, 0, 0, 0 };
	unsigned char cancode2[4] = { 0, 0, 0, 0 };
	unsigned char cancode3[4] = { 0, 0, 0, 0 };
	unsigned char cancode4[4] = { 0, 0, 0, 0 };
	unsigned char cancode5[4] = { 0, 0, 0, 0 };
	unsigned char cancode6[4] = { 0, 0, 0, 0 };
	unsigned char temp_string[10] = { 0 };

	unsigned __int16 unused_date = 0;
	unsigned __int16 month = 0;
	unsigned __int16 day = 0;
	unsigned __int16 hours = 0;
	unsigned __int16 minutes = 0;
	int j;


	from_can_code(direct_entry->file_entry.file_name_1, temp_string);
	strcpy_s(cancode1, 4, temp_string);
	from_can_code(direct_entry->file_entry.file_name_2, temp_string);
	strcpy_s(cancode2, 4, temp_string);
	from_can_code(direct_entry->file_entry.file_name_3, temp_string);
	strcpy_s(cancode3, 4, temp_string);

	from_can_code(direct_entry->file_entry.month_day, temp_string);
	strcpy_s(cancode4, 4, temp_string);
	from_can_code(direct_entry->file_entry.year, temp_string);
	strcpy_s(cancode5, 4, temp_string);
	from_can_code(direct_entry->file_entry.minutes, temp_string);
	strcpy_s(cancode6, 4, temp_string);

	unused_date = (direct_entry->file_entry.month_day >> 9) & 0x007f;
	month = (direct_entry->file_entry.month_day >> 5) & 0x000f;
	day = direct_entry->file_entry.month_day & 0x001f;
	hours = direct_entry->file_entry.minutes / (unsigned __int16)60;
	minutes = direct_entry->file_entry.minutes % (unsigned __int16)60;

	/* -------- file name */
	strcpy_s(parsed_file_entry->file_name, 10, cancode1);
	strcat_s(parsed_file_entry->file_name, 10, cancode2);
	strcat_s(parsed_file_entry->file_name, 10, cancode3);
	for (j = 9; j > -1; j--) {
		if (parsed_file_entry->file_name[j] > 32)
			break;
		if (parsed_file_entry->file_name[j] == 32)
			parsed_file_entry->file_name[j] = 0;
	}

	if (unused_date == 0) {
		parsed_file_entry->month = month;
		parsed_file_entry->day = day;
		parsed_file_entry->year = direct_entry->file_entry.year;
		parsed_file_entry->hour = hours;
		parsed_file_entry->minute = minutes;
		strcpy_s(parsed_file_entry->pgm_name, 10, "");
		parsed_file_entry->pgm_name_defined = false;
	}
	else {
		parsed_file_entry->month = 0;
		parsed_file_entry->day = 0;
		parsed_file_entry->year = 0;
		parsed_file_entry->hour = 0;
		parsed_file_entry->minute = 0;
		strcpy_s(parsed_file_entry->pgm_name, 10, cancode4);
		strcat_s(parsed_file_entry->pgm_name, 10, cancode5);
		strcat_s(parsed_file_entry->pgm_name, 10, cancode6);
		for (j = 9; j > -1; j--) {
			if (parsed_file_entry->pgm_name[j] > 32)
				break;
			if (parsed_file_entry->pgm_name[j] == 32)
				parsed_file_entry->pgm_name[j] = 0;
		}
		parsed_file_entry->pgm_name_defined = true;
	}
	parsed_file_entry->starting_sector = direct_entry->file_entry.starting_sector;
	parsed_file_entry->sector_count = direct_entry->file_entry.sector_count;
	parsed_file_entry->last_sector_words_used = direct_entry->file_entry.last_sector_words_used;

}