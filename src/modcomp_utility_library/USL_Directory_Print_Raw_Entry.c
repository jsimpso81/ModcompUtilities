#include <stdio.h>

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


void USL_Directory_Print_Raw_Entry(USL_DIRECTORY_ENTRY* direct_entry) {

    printf(" ------------- Raw Directory Entry --------------\n");

    printf(" word 0 :  %6d  0x%04X  %s\n", direct_entry->raw_entry.word0,
        direct_entry->raw_entry.word0,
        from_can_code(direct_entry->raw_entry.word0));

    printf(" word 1 :  %6d  0x%04X  %s\n", direct_entry->raw_entry.word1,
        direct_entry->raw_entry.word1,
        from_can_code(direct_entry->raw_entry.word1));

    printf(" word 2 :  %6d  0x%04X  %s\n", direct_entry->raw_entry.word2,
        direct_entry->raw_entry.word2,
        from_can_code(direct_entry->raw_entry.word2));

    printf(" word 3 :  %6d  0x%04X  %s\n", direct_entry->raw_entry.word3,
        direct_entry->raw_entry.word3,
        from_can_code(direct_entry->raw_entry.word3));

    printf(" word 4 :  %6d  0x%04X  %s\n", direct_entry->raw_entry.word4,
        direct_entry->raw_entry.word4,
        from_can_code(direct_entry->raw_entry.word4));

    printf(" word 5 :  %6d  0x%04X  %s\n", direct_entry->raw_entry.word5,
        direct_entry->raw_entry.word5,
        from_can_code(direct_entry->raw_entry.word5));

    printf(" word 6 :  %6d  0x%04X  %s\n", direct_entry->raw_entry.word6,
        direct_entry->raw_entry.word6,
        from_can_code(direct_entry->raw_entry.word6));

    printf(" word 7 :  %6d  0x%04X  %s\n", direct_entry->raw_entry.word7,
        direct_entry->raw_entry.word7,
        from_can_code(direct_entry->raw_entry.word7));

    printf(" word 8 :  %6d  0x%04X  %s\n", direct_entry->raw_entry.word8,
        direct_entry->raw_entry.word8,
        from_can_code(direct_entry->raw_entry.word8));


}