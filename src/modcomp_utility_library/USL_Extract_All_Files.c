#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "../modcomp_utility_library/modcomp_utility_library.h"



/* ========================================================================================================================*/
// USL directory format
//
// 
//  Beginning of directory sector
//      word 0 - sector of previous directory record, 0xFFFF if this is the first.
//      word 1 - sector of next directory record, 0xFFFF if this is the last
//  
//  followed by a number of 9 word directory entries.
// 
//  very first directory entry (initial directory entry)
//      word 0          number of entries per sector  (usually 14 for 128 word sectors, or 10 for 100 word sectors)
//      word 1          number of entries in use (notes:  )
//      word 2          number of entries not in use
//      word 3          mask 0x8000 - set if USL file has been attached.
//                      mask 0x7fff - number of programs on USL file
//      word 4          cancode - chars 1-3 of USL id
//      word 5          cancode - chars 4-6 of USL id
//      word 6          cancode - chars 7-8 of USL id
//      word 7          mask 0xff00 - month last modified
//                      mask 0x00ff - day last modified
//      word 8          year last modified
// 
//  logical file partition entry 
//      word 0          code FEFE;
//	    word 1          cancode - file_partition ( 0 = USL )
//	    word 2          number_of_sectors_in_partition;
//	    word 3          words_per_sector;
//	    word 4-8        unused (should be zero)
//
//  directory sector entry
//      word 0          unused = 0
//      word 1          unused = 0
//      word 2          unused = 0
//      word 3          unused = 0
//      word 4          unused = 0
//      word 5          unused = 0
//      word 6          starting_sector
//      word 7          number_of_sectors
//      word 8          unused = 0
// 
//      
void USL_extract_all_files(char* partition_file, char* extract_dir, char* recover_dir, int max_line_bytes ) {

    FILE* inpart;
    __int64 sector;

    union {
        unsigned _int16 sector_raw[128];
        USL_DIRECTORY_SECTOR usl_directory_sector;
    } sector_buffer;

    int stat;
    errno_t status;
    bool not_done = true;
    int entry;
    size_t return_count;
    int end_of_file;
    int overall_entry = 0;
    int overall_used_entries = 1;
    bool print_raw = false;
    bool beyond_end = false;
    unsigned __int16 USL_log_file = 0;
    USL_FILE_ENTRY file_entry;

    /* -------- open USL partition file  */
    status = fopen_s(&inpart, partition_file, "rb");

    if (status != 0) {
        printf("\n *** ERROR **** Could not open USL partition file -- %s, error = %d\n\n", partition_file, status);
    }

    else {

        /* printf("Dumping file %s\n", partition_file); */

        /* -------- set starting sector and number of sectors */
        sector = 0;
        not_done = true;

        while (not_done) {

            /* -------- read next directory sector, parse and print */
            stat = read_sector_lba(inpart, sector, 1, &sector_buffer, &return_count, &end_of_file);

            if (return_count > 0) {


                /* printf(" prev sector :  %d \n", sector_buffer.usl_directory_sector.prev_sector); */
                /* printf(" next sector :  %d \n", sector_buffer.usl_directory_sector.next_sector); */

                for (entry = 0; entry < 14; entry++) {

                    overall_entry++;

                    /* printf("-------- entry %d of %d --------", overall_entry, overall_used_entries); */
                    if (beyond_end) {
                        /* printf("====DELETED/UNUSED ENTRY===="); */
                    }

                    /* -------- initial entry */
                    if (sector == 0 && entry == 0) {
                        /* USL_Directory_Print_Initial_Entry(&sector_buffer.usl_directory_sector.entries[entry]); */
                        overall_used_entries = sector_buffer.usl_directory_sector.entries[entry].initial_entry.entries_in_use;
                    }

                    /* ------- attached logical file entry */
                    else if (sector_buffer.usl_directory_sector.entries[entry].raw_entry.word0 == 0xfefe) {
                        /* USL_Directory_Print_LogFile_Entry(&sector_buffer.usl_directory_sector.entries[entry]); */
                        USL_log_file = sector_buffer.usl_directory_sector.entries[entry].file_partition_entry.partition_file;
                    }

                    /* ------- entry for directory sector entry */
                    else if (sector_buffer.usl_directory_sector.entries[entry].raw_entry.word0 == 0) {
                        /* USL_Directory_Print_DirectSector_Entry(&sector_buffer.usl_directory_sector.entries[entry]); */
                    }

                    /* -------- last entry*/
                    else if (sector_buffer.usl_directory_sector.entries[entry].raw_entry.word0 == 0xffff) {
                        /* printf("LAST ENTRY ---------------------------\n"); */
                        beyond_end = true;
                    }

                    /* -------- entry for file */
                    else {
                        USL_Parse_File_Entry(&sector_buffer.usl_directory_sector.entries[entry], &file_entry);
                        USL_Directory_Print_File_Entry(&file_entry);
                        if (beyond_end) {
                            printf(" recover file - %s, start %d, len %d\n", file_entry.file_name, file_entry.starting_sector, file_entry.sector_count);
                            USL_Extract_File( inpart, USL_log_file, &file_entry, recover_dir, max_line_bytes);
                        }
                        else {
                            printf(" extract file - %s, start %d, len %d\n", file_entry.file_name, file_entry.starting_sector, file_entry.sector_count);
                            USL_Extract_File(inpart, USL_log_file, &file_entry, extract_dir, max_line_bytes);
                        }
                     }

                    /* -------- for debug */
                    if (print_raw)
                        USL_Directory_Print_Raw_Entry(&sector_buffer.usl_directory_sector.entries[entry]);

                }

                sector = sector_buffer.usl_directory_sector.next_sector;

                if (sector == 0 || sector == 65535)
                    not_done = false;
            }
            else {
                not_done = false;
            }
        }

        fclose(inpart);

        printf("\nDone.\n");

    }
}
