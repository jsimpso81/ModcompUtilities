#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#include "modcomp_sim_procedures.h"
#include "modcomp_sim_external_globals.h"


void process_user_commands() {

	static bool exit_request = false;
	static char cmd_line[1024];
	static char cmd_line_parsed[100][1024];
	static int cmd_count_found = 0;
	static char* parsed_list[100];
	static int j = 0;

	// --------jim this is crude.  has is really been so long since we have programmed in C...
	for (j = 0; j < 100; j++) {
		parsed_list[j] = &cmd_line_parsed[j][0];
	}

	// -------process commands until they want to exit... 
	while (!exit_request) {

		cmd_process_print_prompt();
		fgets(cmd_line, 1023, stdin);

		// printf("cmd: %s\n", cmd_line);

		cmd_process_parse(cmd_line, 1023, parsed_list, 100, &cmd_count_found);

		// printf("\n count found = %d\n", cmd_count_found);
		if (cmd_count_found > 0) {
			
			// for (j = 0; j < cmd_count_found; j++) {
			//	printf(" token %d - %s\n", j, cmd_line_parsed[j]);
			//}

			// -------- process initial command 

			// --------show
			if (strcmp(cmd_line_parsed[0], "show") == 0) {
				if (cmd_count_found >= 2) {

					//--------processor status word 

					//--------program counter

					//--------switches

					//--------run

					//--------reg

				}
				else {
					printf(" *** ERROR *** Too few command parameters.\n");
				}
			}

			// --------set
			else if (strcmp(cmd_line_parsed[0], "set") == 0) {
				if (cmd_count_found >= 2) {

					//--------switches
				}
				else {
					printf(" *** ERROR *** Too few command parameters.\n");
				}

			}

			// --------halt
			else if (strcmp(cmd_line_parsed[0], "halt") == 0) {
				gbl_fp_runlight = false;
			}

			// --------run
			else if (strcmp(cmd_line_parsed[0], "run") == 0) {
				gbl_fp_runlight = true;
			}

			// --------step
			else if (strcmp(cmd_line_parsed[0], "step") == 0) {
				if ( !gbl_fp_runlight )
					gbl_fp_single_step = true;
			}

			// --------fill
			// TODO: add switch value for device address
			else if (strcmp(cmd_line_parsed[0], "fill") == 0) {
				if (!gbl_fp_runlight) {
					gbl_mem[0] = 0x401a;
					gbl_mem[1] = 0x484a;
					gbl_mem[2] = 0x7648;
					gbl_mem[3] = 0x0000;
					gbl_mem[4] = 0x4c4a;
					gbl_mem[5] = 0xaf42;
					gbl_mem[6] = 0x7000;
				}
			}

			// --------master clear
			else if (strcmp(cmd_line_parsed[0], "mc") == 0) {
			}

			// --------help
			else if (strcmp(cmd_line_parsed[0], "help") == 0) {
				printf("\nCommands:\n");
				printf("      fill - fill\n");
				printf("      halt - halt cpu\n");
				printf("      help - print this list of commands\n");
				printf("      mc - master clear\n");
				printf("      run - run cpu\n");
				printf("      set swtiches <value> - set console switches\n");
				printf("      show pc - show program counter\n");
				printf("      show psw - show processor status word\n");
				printf("      show reg <0-15> - show current register value\n");
				printf("      show run - show current cpu run state\n");
				printf("      show swtiches - show console switches\n");
				printf("      step - execute a single instruction\n");
				printf("\n");
			}

		}


		// ------- CRUDE
		if (  strncmp( cmd_line, "exit", 4) == 0 ) {
			exit_request = true;
			printf("\nExit requested.\n");
		}

	}


}