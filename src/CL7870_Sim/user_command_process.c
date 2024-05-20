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
	static int k = 0;

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
					// TODO: get and show psw
					if (strcmp(cmd_line_parsed[1], "psw") == 0) {
						printf(" TODO Proessor status word 0x%04x\n", 0);
					}

					//--------program counter
					else if (strcmp(cmd_line_parsed[1], "pc") == 0) {
						printf(" Program counter 0x%04x\n", cpu_get_program_counter());
					}

					//--------switches
					else if (strcmp(cmd_line_parsed[1], "switches") == 0) {
						printf(" Front panel switches 0x%04x\n", gbl_fp_switches);
					}

					//--------run
					else if (strcmp(cmd_line_parsed[1], "run") == 0) {
						printf(" Run / Halt mode :%s\n", (gbl_fp_runlight ? "Run" : "Halt"));
					}

					//--------reg
					else if (strcmp(cmd_line_parsed[1], "reg") == 0) {
						printf(" Current register block\n");
						printf("          0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n", 
							cpu_get_register_value(1),
							cpu_get_register_value(2),
							cpu_get_register_value(3),
							cpu_get_register_value(4),
							cpu_get_register_value(5),
							cpu_get_register_value(6),
							cpu_get_register_value(7)
							);
						printf("  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n",
							cpu_get_register_value(8),
							cpu_get_register_value(9),
							cpu_get_register_value(10),
							cpu_get_register_value(11),
							cpu_get_register_value(12),
							cpu_get_register_value(13),
							cpu_get_register_value(14),
							cpu_get_register_value(15)
							);
					}

					//--------mem
					else if (strcmp(cmd_line_parsed[1], "mem") == 0) {
						printf(" Memory\n");
						for (j = 0; j < 512; j += 8) {
							printf("  0x%04x:  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n", j,
								gbl_mem[j],
								gbl_mem[j + 1],
								gbl_mem[j + 2],
								gbl_mem[j + 3],
								gbl_mem[j + 4],
								gbl_mem[j + 5],
								gbl_mem[j + 6],
								gbl_mem[j + 7]
								);
						}
					}
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
				if (cmd_count_found >= 2) {
					if (sscanf_s(cmd_line_parsed[1], "%d", &k) == 1 ) {
						if (!gbl_fp_runlight) {
							bool diffval = true;
							for (j = 0; j < k; j++) {
								gbl_fp_single_step = true;
								WaitOnAddress( &gbl_fp_single_step, &diffval, sizeof(gbl_fp_single_step), INFINITE );
							}
						}
					}
					else {
						printf(" *** ERROR *** second parameter, invalid %s\n", cmd_line_parsed[1]);
					}
				}
				else {
					if (!gbl_fp_runlight)
						gbl_fp_single_step = true;
				}
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
					cpu_set_register_value(1, 0x8800);
					cpu_set_register_value(2, 0x002e);
					cpu_set_register_value(3, 0xffb1);
					cpu_set_register_value(4, 0xbd8f);		// unused
					cpu_set_register_value(5, 0);
					cpu_set_register_value(6, 0);
					cpu_set_register_value(7, 0);
				}
			}

			// --------master clear
			else if (strcmp(cmd_line_parsed[0], "mc") == 0) {
				cpu_set_program_counter(0);
			}

			// --------help
			else if (strcmp(cmd_line_parsed[0], "help") == 0) {
				user_cmd_print_help();
			}

			// --------just in case 1
			else if (strcmp(cmd_line_parsed[0], "shit") == 0) {
				printf(" Please dont.  The smell would be unbearable.\n");
			}

			// --------just in case 2
			else if (strcmp(cmd_line_parsed[0], "fuck") == 0) {
				printf(" No thanks.  Im not that kind of simulator.\n");
			}

			// ------- exit
			else if (strcmp(cmd_line_parsed[0], "exit") == 0) {
				exit_request = true;
				printf("\nExit requested.\n");
			}

			else {
				printf(" *** ERROR *** Unrecognized command: %s\n", cmd_line_parsed[0]);
			}

		}



	}


}