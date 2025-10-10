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
#include <stdbool.h>
#include <string.h>

// -------- local structure for heap allocated data.
typedef struct {
	char cmd_line[1024];
	char cmd_line_parsed[100][1024];		
	char* parsed_list[100];					
} CMD_PROC_STRINGS;

// =================================================================================================
void process_user_commands(FILE* cmd_src) {

	bool exit_request = false;
	int cmd_count_found = 0;
	int j = 0;
	bool input_eof = false;
	bool input_from_file = false;

	CMD_PROC_STRINGS* cmd_data;
	size_t buffer_size;

	// --------allocate heap for command processing
	buffer_size = sizeof(CMD_PROC_STRINGS);
	cmd_data = (void*)HeapAlloc(GetProcessHeap(),
		HEAP_ZERO_MEMORY,
		buffer_size);

	// --------jim this is crude.  has is really been so long since we have programmed in C...
	for (j = 0; j < 100; j++) {
		cmd_data->parsed_list[j] = &cmd_data->cmd_line_parsed[j][0];
	}

	input_from_file = !util_is_same_stream( stdin, cmd_src );
	// --debug--  printf(" PROC CMD INPUT FROM FILE = %s\n", (input_from_file ? "Yes" : "No"));

	// -------process commands until they want to exit... 
	while (!exit_request && !(input_from_file && input_eof)) {


		if (!input_from_file) {
			cmd_process_print_prompt();
		}

		fgets(cmd_data->cmd_line, 1023, cmd_src);
		if (!(feof(cmd_src) && input_from_file)) {

			// -------- if not stdin then echo to stdout
			if (input_from_file) {
				cmd_process_print_prompt();
				printf("%s", cmd_data->cmd_line);		// line appears to include cr/lf 
			}

			// --------break command line into separate tokens.   This is VERY crude.
			// --------cmd_line is changed by this routine.
			cmd_process_parse(cmd_data->cmd_line, 1023, cmd_data->parsed_list, 100, &cmd_count_found);

			// --debug--  printf("\n count found = %d\n", cmd_count_found);
			if (cmd_count_found > 0) {

				// --debug-- for (j = 0; j < cmd_count_found; j++) {
				// --debug-- 	printf(" token %d - %s\n", j, cmd_line_parsed[j]);
				// --debug-- }

				// -------- process initial command 

				// --------comment -- full line
				if (strcmp(cmd_data->cmd_line_parsed[0], ";") == 0) {
				}

				// --------comment -- just first character.
				else if (cmd_data->cmd_line_parsed[0][0] == ';') {
				}

				// --------call new function to parse and process commands....
				else {
					exit_request = user_cmd_find_and_execute(cmd_count_found, cmd_data->parsed_list);
				}
			}

		}
		else {
			input_eof = true;
		}

	}  // while

	// printf("\n\n  EXIT REQUEST %s,  INPUT NOT STDIN %s,  END OF FILE %s\n\n",
	//	(exit_request ? "true" : "false"), (input_from_file ? "true" : "false"), (input_eof ? "true" : "false"));

	// -------- free heap allocated data 
	HeapFree(GetProcessHeap(), 0, (LPVOID)cmd_data);

	return;

}