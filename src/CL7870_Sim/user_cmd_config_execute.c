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