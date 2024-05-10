#pragma once

#include <stdio.h>
#include <stdbool.h>

#define sector_bytes (size_t)256
#define disk_img_sector (size_t)258

typedef struct {
    unsigned __int16     lba;
    unsigned __int16     sectbuffer[128];
} DISC_IMG_SECTOR;

typedef union {
    struct {
        unsigned __int16    word0;
        unsigned __int16    word1;
        unsigned __int16    word2;
        unsigned __int16    word3;
        unsigned __int16    word4;
        unsigned __int16    word5;
        unsigned __int16    word6;
        unsigned __int16    word7;
        unsigned __int16    word8;
    } raw_entry;
    struct {
        unsigned __int16    entries_per_sector;
        unsigned __int16    entries_in_use;
        unsigned __int16    entries_unused;
        unsigned __int16    raw_programs_on_USL;  /* 0x8000 usl attached, 0x7fff programs on USL */
        unsigned __int16    usl_id_1;  
        unsigned __int16    usl_id_2;
        unsigned __int16    usl_id_3;
        unsigned __int8     day_modified;
        unsigned __int8     month_modified;
        unsigned __int16    year_modified;
    } initial_entry;
    struct {
        unsigned __int16    code;   // must be FEFE
        unsigned __int16    partition_file; // 0 = USL
        unsigned __int16    partition_sectors;
        unsigned __int16    word_per_sector;  
        unsigned __int16    unused1;
        unsigned __int16    unused2;
        unsigned __int16    unused3;
        unsigned __int16    unused4;
        unsigned __int16    unused5;
    } file_partition_entry;
    struct {
        unsigned __int16    unused1; //  = 0 
        unsigned __int16    unused2; 
        unsigned __int16    unused3;
        unsigned __int16    unused4;
        unsigned __int16    unused5;
        unsigned __int16    unused6;
        unsigned __int16    starting_sector;
        unsigned __int16    sector_count;
        unsigned __int16    unused7;
    } directory_sector_entry;
    struct {
        unsigned __int16    file_name_1;
        unsigned __int16    file_name_2;
        unsigned __int16    file_name_3;
        unsigned __int16    month_day;  // uuuuuuummmmddddd -- guess  -- alternate words 3-5 are can coded PGM line or CAT identifier
        unsigned __int16    year;
        unsigned __int16    minutes;
        unsigned __int16    starting_sector;
        unsigned __int16    sector_count;
        unsigned __int16    last_sector_words_used;
    } file_entry;
} USL_DIRECTORY_ENTRY;


typedef struct {
    char     file_name[10]; 
    char     pgm_name[10];
    unsigned __int16 starting_sector;
    unsigned __int16 sector_count;
    unsigned __int16 last_sector_words_used;
    unsigned __int16 day;
    unsigned __int16 month;
    unsigned __int16 year;
    unsigned __int16 hour;
    unsigned __int16 minute;
    bool pgm_name_defined;
} USL_FILE_ENTRY;


typedef struct {
    unsigned __int16    prev_sector;
    unsigned __int16    next_sector;
    USL_DIRECTORY_ENTRY entries[14];
} USL_DIRECTORY_SECTOR;


/* -------- function prototypes */
unsigned __int16  bswap16(unsigned __int16 a);
void dump_file(char* filename, bool swap_bytes);
void dump_sector(unsigned __int16* sector_buffer);
void extract_partition(char* image_name, char* partition_file, __int64 start_sector, __int64 sector_count);
unsigned char* from_can_code(unsigned int can_value, unsigned char* result_string);
unsigned int get_can_index(unsigned int ascii_code);
int read_disk_image_sector_lba(FILE* fp, __int64 sector, void* buf);
int read_sector_lba(FILE* fp, __int64 sector, size_t  sector_count, void* buf, size_t* return_count, int* end_of_file);
unsigned int to_can_code(unsigned char* ascii_string);
void USL_Directory_Print_DirectSector_Entry(USL_DIRECTORY_ENTRY* direct_entry);
void USL_Directory_Print_File_Entry(USL_FILE_ENTRY* file_entry);
void USL_Directory_Print_Initial_Entry(USL_DIRECTORY_ENTRY* direct_entry);
void USL_Directory_Print_LogFile_Entry(USL_DIRECTORY_ENTRY* direct_entry);
void USL_Directory_Print_Raw_Entry(USL_DIRECTORY_ENTRY * direct_entry);
void USL_dump_directory(char* filename);
void USL_extract_all_files(char* partition_file, char* extract_dir, char* recover_dir, int max_line_bytes);
void USL_Extract_File(FILE* inpart, unsigned __int16 USL_log_file, USL_FILE_ENTRY* parsed_file_entry, char* directory, int max_line_bytes);
void USL_Parse_File_Entry(USL_DIRECTORY_ENTRY* direct_entry, USL_FILE_ENTRY* parsed_file_entry);


/* --------------------------------------------------------- */