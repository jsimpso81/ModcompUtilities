#pragma once

#include <stdio.h>
#include <stdbool.h>
#include <windows.h>

#define RAW_SECTOR_BYTES (size_t)256
#define MODCOMP_EMUL_DISK_IMG_SECTORY_BYTES (size_t)258

#define SIC_8840_IMG_SECTOR_BYTES RAW_SECTOR_BYTES
#define SIC_8840_IMG_START_OFFSET_BYTES (size_t)0x4000
#define SIC_8840_TRK_PER_UNIT (__int64)1632
#define SIC_8840_SEC_PER_TRK (__int64)24
//#define SIC_8840_IMG_EOF_BUFFER_BYTES (size_t)19585     // numb of sectors + 1 because of offset start nibble (odd??)
#define SIC_8840_IMG_EOF_BUFFER_BYTES (size_t)( (__int64)4 * SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK)   // TODO: This twice as big as needed.
//#define SIC_8840_IMG_EOF_BUFFER_U0_START_OFFSET (size_t)40124864
#define SIC_8840_IMG_EOF_BUFFER_U0_START_OFFSET (size_t)( (__int64)4 * SIC_8840_TRK_PER_UNIT * SIC_8840_SEC_PER_TRK * SIC_8840_IMG_SECTOR_BYTES + SIC_8840_IMG_START_OFFSET_BYTES)

// #define disk_884x_start_sector (__int64)32
// #define disk_884x_sect_size (__int64)(256+4)

typedef struct {
    unsigned __int16     flags;     // ------- could include EOF indicator = 1, otherwise 0
    unsigned __int16     rawsectbuffer[RAW_SECTOR_BYTES/2];
} MODCOMP_EMUL_DISC_SECTOR;

typedef struct {
    unsigned __int8     sectbuffer[SIC_8840_IMG_SECTOR_BYTES];
} SIC_884x_DISC_SECTOR;

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

void copy_disk_partition_to_tape_image(char* partition_file, char* tape_image_name, bool swap_bytes);

void dump_raw_disk_file(char* filename, bool swap_bytes);
void dump_raw_disk_file_as_byte_variable(char* filename, bool swap_bytes, int start_sector, int end_sector);

void dump_raw_disk_sector(unsigned __int16* sector_buffer);
void dump_raw_disk_sector_as_byte_variable(unsigned __int16 sector_buffer[]);

void extract_modcomp_emul_disk_partition(char* image_name, char* partition_file, __int64 start_sector, __int64 sector_count, __int64 sector_per_track, __int64 geom);
void extract_raw_disk_partition(char* image_name, char* partition_file, __int64 start_sector, __int64 sector_count, __int64 sector_per_track, __int64 geom);
void extract_sic_884x_disk_partition(char* image_name, char* partition_file, __int64 start_sector, __int64 sector_count, __int64 sector_per_track, __int64 geom);

unsigned int from_can_code(unsigned int can_value, unsigned char* result_string);
unsigned int from_rad50(unsigned int rad50_value, unsigned char* result_string, bool option);
unsigned int get_can_index(unsigned int ascii_code);

// -------- read disk sectors.
int read_884x_disk_sector_lba(FILE* fp, __int64 sector, void* buf);
int read_modcomp_emul_disk_sector_lba(FILE* fp, __int64 sector, void* buf);
int read_raw_disk_sector_lba(FILE* fp, __int64 sector_number, size_t  sector_count, void* buf, size_t* return_bytes, int* end_of_file);
// -------- write disk sectors
int write_884x_disk_sector_lba(FILE* fp, __int64 sector, void* buf);
int write_modcomp_emul_disk_sector_lba(FILE* fp, __int64 sector, void* buf);

// --------SIC EOF checking.
int sic_884x_is_sector_eof(FILE* inimg, __int64 unit, __int64 unit_sector, bool* is_eof);
int write_884x_eof_buffer(FILE* fp);
int read_884x_eof_buffer(FILE* fp);
int sic_884x_check_eof_buffer(__int64 unit, __int64 unit_sector, bool* is_eof);
int sic_884x_set_eof_buffer(__int64 unit, __int64 unit_sector, bool is_eof);
void sic_884x_disk_image_check_fix_eof(char* image_name, __int64 unit, bool fix, bool list);
void sic_884x_disk_image_dump_eof_buffer(char* image_name);

// -------- tape image I/O
void TapeImg_dump_records(char* filename, bool swap_bytes);
int TapeImg_read_next_record(FILE* fp, __int64* current_file_position, void* buf, int max_buf_bytes, size_t* bytes_read, int* end_of_file);
int TapeImg_write_next_record(FILE* fp, void* buf, int buf_bytes, size_t* bytes_written);
// --------CAN CODE
unsigned int to_can_code(unsigned char* ascii_string);

void update_modcomp_emul_disk_partition(char* image_name, char* partition_file, __int64 start_sector, __int64 sector_count);
void update_sic_884x_disk_partition(char* image_name, char* partition_file, __int64 start_sector, __int64 sector_count, __int64 unit);

__int16 geom_calc(__int64 sect_per_track, __int64 geom, __int64 log_to_phys[]);

// --------Source USL
void USL_Directory_Print_DirectSector_Entry(USL_DIRECTORY_ENTRY* direct_entry);
void USL_Directory_Print_File_Entry(USL_FILE_ENTRY* file_entry);
void USL_Directory_Print_Initial_Entry(USL_DIRECTORY_ENTRY* direct_entry);
void USL_Directory_Print_LogFile_Entry(USL_DIRECTORY_ENTRY* direct_entry);
void USL_Directory_Print_Raw_Entry(USL_DIRECTORY_ENTRY * direct_entry);
void USL_dump_directory(char* filename);
void USL_extract_all_files(char* partition_file, char* extract_dir, char* recover_dir, int max_line_bytes, bool tape_flag);
void USL_Extract_File(FILE* inpart, unsigned __int16 USL_log_file, USL_FILE_ENTRY* parsed_file_entry, char* directory, int max_line_bytes, bool tape_flag);
void USL_Parse_File_Entry(USL_DIRECTORY_ENTRY* direct_entry, USL_FILE_ENTRY* parsed_file_entry);

// --------utility
int util_serial_close(HANDLE com_handle, DWORD* last_error);
int util_serial_open(char* com_port, HANDLE* com_handle, DWORD* last_error);
int util_serial_set_params(HANDLE hCom, DWORD* last_error, bool USE_HDWR_OUTPUT_HANDSHAKE);
void util_serial_print_settings(DCB this_dcb);

// --------NOT MODCOMP -- PDP10 STUFF !!
void PDP10_dump_tape_header(unsigned __int64* tape_words_36);
void PDP10_dump_2word(unsigned __int64 inx, unsigned __int64 word1, unsigned __int64 word2);
void PDP10_dump_2word_header();
void PDP10_dump_3word(unsigned __int64 inx, unsigned __int64 word1, unsigned __int64 word2, unsigned __int64 word3);
void PDP10_dump_3word_header();
void PDP10_parse_6bit_text(unsigned __int64 word36, unsigned char* chars7);
void PDP10_parse_7bit_text(unsigned __int64 word36, unsigned char* chars6);
void PDP10_write_extracted_word(FILE* extpart, unsigned __int64* word36);
unsigned __int64 PDP10_tape_decode_word36(unsigned char* tapebytes);
void PDP10_file_to_text(char* filename);
void PDP10_ks10ram_to_cons(char *filename);
void PDP10_ks10_file_to_cons(char* filename, char* consport);
void PDP10_sav_to_cons(char* filename);
void PDP10_TapeImg_dump10_records(char* filename, bool swap_bytes);
unsigned __int64 PDP10_read_extracted_word(FILE* readfile);


/* --------------------------------------------------------- */