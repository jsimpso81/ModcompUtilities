#include <stdio.h>
#include <string.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"

//	unsigned __int16    code FEFE;
//	unsigned __int16    logical_file;
//	unsigned __int16    number_of_sectors_in_partition;
//	unsigned __int16    words_per_sector;
//	unsigned __int16    unused1;
//	unsigned __int16    unused2;
//	unsigned __int16    unused3;
//	unsigned __int16    unused4;
//	unsigned __int16    unused5;


void USL_Directory_Print_LogFile_Entry(USL_DIRECTORY_ENTRY* direct_entry) {

	unsigned char cancode1[4] = { 0, 0, 0, 0 };
	unsigned char temp_string[10] = { 0 };

	strcpy_s(cancode1, 4, from_can_code(direct_entry->file_partition_entry.partition_file, temp_string));

	printf("Attached Logical File directory entry ----------------------------\n");
	printf("      Logical attached file   %s\n", (direct_entry->file_partition_entry.partition_file == 0) ? "USL" : cancode1);
	printf("      Sectors in partition    %6d\n", direct_entry->file_partition_entry.partition_sectors);
	printf("      Word in sector          %6d\n", direct_entry->file_partition_entry.word_per_sector);

}