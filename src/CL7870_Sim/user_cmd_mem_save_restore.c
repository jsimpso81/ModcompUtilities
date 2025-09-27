// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			XXXX.c
//
//	Description:	Routines to save and restore memory (and other things....)
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


// ==================================================================================================================
void user_cmd_mem_restore(char* input_file_name) {

	FILE* restorefile;
	errno_t status;

	// -------- open file.  It it doesn't exist print message and exit.
	status = fopen_s(&restorefile, input_file_name, "r");

	if (status != 0) {
		printf("\n *** ERROR *** - Could not open memory restore file - %s  Error status: %d\n\n", input_file_name, status);
		return;
	}

	// -------- do the restore....
	// -------- restore main memory
	// -------- restore current register block
	// -------- restore all register blocks.
	// -------- restore processor status
	// -------- restore program counter
	// -------- restore enabled interrupts
	// -------- restore active interrupts
	// -------- restore requested interrupts.



	// --------close file
	fclose(restorefile);

	// --------return to user
	return;

}



// ==================================================================================================================
void user_cmd_mem_save(char* input_file_name) {

	FILE* savefile;
	errno_t status;

	// -------- open file.  If it exists, just overwrite it...
	status = fopen_s(&savefile, input_file_name, "r");

	if (status != 0) {
		printf("\n *** ERROR *** - Could not open memory save file - %s  Error status: %d\n\n", input_file_name, status);
		return;
	}

	// -------- do the save....
	// -------- save main memory
	// -------- save current register block
	// -------- save all register blocks.
	// -------- save processor status
	// -------- save program counter
	// -------- save enabled interrupts
	// -------- save active interrupts
	// -------- save requested interrupts.



	// --------close file
	fclose(savefile);

	// --------return to user
	return;

}