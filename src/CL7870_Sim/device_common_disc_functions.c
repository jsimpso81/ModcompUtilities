// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			device_common_disc_functions.c
//
//	Description:	Routines to xxxxxxx.
// 
//  Routines:
//          int device_common_disc_open(char* disc_filename, bool read_only, 
//                      FILE** disc_file_handle, DWORD* last_error)
//			int device_common_disc_read_sector(FILE* fp, unsigned __int64 sector, 
//						void* raw_sector_buf, unsigned __int16* flags)
//
// 
//  Internal routines:
//
//	Externally accessible routines:
//					XXXXXXX
// 
// Internal only routines:
//					XXXXXXX
//
// Notes:
//		XXXXXX
// 
// ================================================================================================
//	Revision history:
//		6/28/2024	JAS		Added new header
// ================================================================================================

#include "simj_base.h"
#include <stdio.h>
#include <memory.h>


#define RAW_SECTOR_BYTES (size_t)256
#define MODCOMP_EMUL_DISK_IMG_SECTOR_BYTES (size_t)258
typedef struct {
	unsigned __int16     flags;     // ------- could include EOF indicator = 1, otherwise 0
	unsigned __int16     rawsectbuffer[RAW_SECTOR_BYTES / 2];
} MODCOMP_EMUL_DISC_SECTOR;

// ================================================================================================
// -------- open a modcomp emulator disc image file.
int device_common_disc_open(char* disc_filename, bool read_only, FILE** disc_file_handle, DWORD* last_error) {


    errno_t loc_status;
    FILE* loc_disc_file_handle;

    // -------- open input disc image file.
    // -------- read only
    if (read_only) {
        loc_status = fopen_s(&loc_disc_file_handle, disc_filename, "rb");
    }
    // -------- read / write
    else {
        // TODO: for now the disc file must exist first to write....
        loc_status = fopen_s(&loc_disc_file_handle, disc_filename, "r+b");
    }

    if (loc_status != 0) {
        fprintf(stderr, " *** ERROR *** device_common_disc_open.  Could not open disc image file: %d, %s\n\n", loc_status, disc_filename);
        *last_error = loc_status;
        return 1;
    }

    // TODO: Add more info on connection to message..
    printf(" *** INFO *** device_common_disc_open - File opened:  %s...\n", disc_filename);

    // --------return the socket info and set no error
    *disc_file_handle = loc_disc_file_handle;
    *last_error = 0;
    return 0;

}



// ================================================================================================
int device_common_disc_read_sector(FILE* fp, unsigned __int64 sector, void* raw_sector_buf, unsigned __int16* flags) {

	__int64 pos;
	__int64 desired_pos;
	int stat;
	size_t return_count;
	// int j;

	MODCOMP_EMUL_DISC_SECTOR disk_buff = { 0 };

	desired_pos = sector * (__int64)MODCOMP_EMUL_DISK_IMG_SECTOR_BYTES;
	pos = _ftelli64(fp);

	//if (pos != desired_pos)
		stat = _fseeki64(fp, desired_pos, SEEK_SET);
	//else
		//stat = 0;

	//--debug
	printf(" disk common read - sector %lld, pos %lld, des pos %lld, stat %d\n",
					sector, pos, desired_pos, stat );

	if (stat == 0) {
		return_count = fread(&disk_buff, (size_t)1, MODCOMP_EMUL_DISK_IMG_SECTOR_BYTES, fp);
		stat = ferror(fp);
		//--debug
		printf(" disc common read - return count %lld, stat %d \n", return_count,stat);
	}

	/* printf("\n first two bytes of image sector : %d", disk_buff.lba); */

	// --------byte swap sector data...
	int j = 0;
	for (j = 0; j < 128; j++) {
		disk_buff.rawsectbuffer[j] = bswap16(disk_buff.rawsectbuffer[j]);
	}

	memcpy(raw_sector_buf, disk_buff.rawsectbuffer, (size_t)RAW_SECTOR_BYTES);
	*flags = disk_buff.flags;

	return stat;

}
