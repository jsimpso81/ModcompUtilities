// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			XXXX.c
//
//	Description:	Routines to xxxxxxx.
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


void user_cmd_config_execute(char* input_file_name) {

	FILE* cmdfile;
	errno_t status;


	// -------- open file.  It it doesn't exist print message and exit.
	status = fopen_s( &cmdfile, input_file_name, "r");


	if (status != 0) {
		printf("\n *** ERROR *** - Could not open configuration file - %s  Error status: %d\n\n", input_file_name, status);
		return;
	}

	// -------- execute commands
	process_user_commands(cmdfile);

	// --------close file
	fclose(cmdfile);

	// --------return to user
	return;

}