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
	static unsigned int uj = 0;

	static unsigned __int32 last_starting_mem_address = 0;
	static unsigned __int32 last_ending_mem_address = 1023;

// -------- commands
//      mc - master clear
//      help - get a help message, show command list
//      set switches <value>
//      set regdisplay <value>
//		set verbose <on|off>
//      show switches
//      show pc  - program counter
//      show ps  - processor status
//		show verbose
//      fill - issue a fill command
//      run - run cpu
//      step - single step cpu
//      halt = halt cpu
//      device bus dev_addr pri dmp model/type file
//                          types:  console  - cons <comx or tcp port>
//                                  mag tape --  mt <unit> <tape file name>
//                                  disk --  lx <unit> <disk file img>
//                                  async -- as <unit> <comx or tcp port>
//



	// --------jim this is crude.  has is really been so long since we have programmed in C...
	for (j = 0; j < 100; j++) {
		parsed_list[j] = &cmd_line_parsed[j][0];
	}

	// -------process commands until they want to exit... 
	while (!exit_request) {

		cmd_process_print_prompt();
		fgets(cmd_line, 1023, stdin);

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
						disp_psw( stdout, cpu_get_current_PSW() );
					}

					//--------clock
					else if (strcmp(cmd_line_parsed[1], "clock") == 0) {
						printf(" Clock trigger count: %d\n", get_clock_trigger_count());
					}


					//--------program counter
					else if (strcmp(cmd_line_parsed[1], "pc") == 0) {
						disp_pc(stdout,cpu_get_program_counter());
					}

					//--------devices
					else if (strcmp(cmd_line_parsed[1], "devices") == 0) {
						disp_devices(stdout);
					}


					//--------switches
					else if (strcmp(cmd_line_parsed[1], "switches") == 0) {
						printf(" Front panel switches 0x%04x\n", gbl_fp_switches);
					}

					//--------run
					else if (strcmp(cmd_line_parsed[1], "run") == 0) {
						printf(" Run / Halt mode :%s\n", (gbl_fp_runlight ? "Run" : "Halt"));
					}

					//--------verbose
					else if (strcmp(cmd_line_parsed[1], "verbose") == 0) {
						printf(" Verbose debug mode : %s\n", (gbl_verbose_debug ? "On" : "Off"));
					}

					//--------reg
					else if (strcmp(cmd_line_parsed[1], "reg") == 0) {
						disp_cur_reg(stdout);
					}

					//--------mem
					else if (strcmp(cmd_line_parsed[1], "mem") == 0) {
						unsigned __int32 parm_parse = 0;
						if (cmd_count_found >= 3) {
							if (sscanf_s(cmd_line_parsed[2], "%li", &parm_parse) == 1) {
								last_starting_mem_address = parm_parse;
							}
							else {
								printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
							}
						}
						if (cmd_count_found >= 4) {
							if (sscanf_s(cmd_line_parsed[3], "%li", &parm_parse) == 1) {
								last_ending_mem_address = parm_parse;
							}
							else {
								printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[3]);
							}
						}

						printf(" Memory\n");
						for (uj = last_starting_mem_address; uj < (last_ending_mem_address+8); uj += 8) {
							printf("  0x%04x  |  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  0x%04x  \n", uj,
								gbl_mem[uj],
								gbl_mem[uj + 1],
								gbl_mem[uj + 2],
								gbl_mem[uj + 3],
								gbl_mem[uj + 4],
								gbl_mem[uj + 5],
								gbl_mem[uj + 6],
								gbl_mem[uj + 7]
								);
						}
					}

					//--------instruction use (deug)
					else if (strcmp(cmd_line_parsed[1], "inst") == 0) {
						disp_instruction_use(stdout);
					}
					//--------instruction execution count
					else if (strcmp(cmd_line_parsed[1], "count") == 0) {
						printf(" Instruction execution count %d\n", cpu_get_instruction_count());
					}

					// --------just in case 1
					else if (strcmp(cmd_line_parsed[1], "shit") == 0) {
						printf(" It is brown gross and smelly.\n");
					}

					// --------just in case 2
					else if (strcmp(cmd_line_parsed[1], "fuck") == 0) {
						printf(" No thanks.  That cant be simulated here.\n");
					}

					// --------unrecognozed show sub command.
					else {
						printf(" *** ERROR *** Unrecognized show sub-command: %s.\n", cmd_line_parsed[1]);
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
					if (strcmp(cmd_line_parsed[1], "switches") == 0) {
						unsigned __int16 new_switch_value = 0;
						unsigned __int16 parm_parse = 0;
						if (cmd_count_found >= 3) {
							if (sscanf_s(cmd_line_parsed[2], "%hi", &parm_parse) == 1) {
								new_switch_value = parm_parse;
								cpu_set_switches(new_switch_value);
							}
							else {
								printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
							}
						}
						else {
							printf(" *** ERROR *** Command requires another parameter. \n");
						}
					}

					//--------set memory
					else if (strcmp(cmd_line_parsed[1], "mem") == 0) {
						if (cmd_count_found >= 4) {
							unsigned __int32 parm_parse = 0;
							unsigned __int32 set_addr = 0;
							unsigned __int16 set_value = 0;
							if (sscanf_s(cmd_line_parsed[2], "%i", &parm_parse) == 1) {
								set_addr = parm_parse;
								if (sscanf_s(cmd_line_parsed[3], "%i", &parm_parse) == 1) {
									set_value = parm_parse;
									gbl_mem[set_addr] = set_value;
								}
								else {
									printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[3]);
								}
							}
							else {
								printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
							}
						}
						else {
							printf(" *** ERROR *** Expecting two numeric values following set mem\n");
						}
					}

					//--------set register
					else if (strcmp(cmd_line_parsed[1], "reg") == 0) {
						if (cmd_count_found >= 4) {
							unsigned __int32 parm_parse = 0;
							unsigned __int16 set_reg = 0;
							unsigned __int16 set_value = 0;
							if (sscanf_s(cmd_line_parsed[2], "%i", &parm_parse) == 1) {
								set_reg = parm_parse;
								if (sscanf_s(cmd_line_parsed[3], "%i", &parm_parse) == 1) {
									set_value = parm_parse;
									if (set_reg >= 1 && set_reg <= 15) {
										cpu_set_register_value(set_reg, set_value);
									}
									else {
										printf(" *** ERROR *** Register number must be within 1 to 15 : %d\n", set_reg);
									}
								}
								else {
									printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[3]);
								}
							}
							else {
								printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
							}
						}
						else {
							printf(" *** ERROR *** Expecting two numeric values following set mem\n");
						}
					}

					//--------verbose
					else if ( strcmp(cmd_line_parsed[1], "verbose") == 0) {
						if (cmd_count_found >= 3) {
							if (strcmp(cmd_line_parsed[2], "on") == 0) {
								gbl_verbose_debug = true;
							}
							else if (strcmp(cmd_line_parsed[2], "off") == 0) {
								gbl_verbose_debug = false;
							}
							else {
								printf(" *** ERROR *** Expecting either on or off\n");
							}
						}
						else {
							printf(" *** ERROR *** Command requires another parameter. \n");
						}
					}


					//--------set pc
					else if (strcmp(cmd_line_parsed[1], "pc") == 0) {
						if (cmd_count_found >= 3) {
							unsigned __int32 parm_parse = 0;
							unsigned __int32 set_pc = 0;
							if (sscanf_s(cmd_line_parsed[2], "%i", &parm_parse) == 1) {
								set_pc = parm_parse;
								cpu_set_program_counter(set_pc);
							}
							else {
								printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
							}
						}
						else {
							printf(" *** ERROR *** Expecting two numeric values following set mem\n");
						}
					}

					// --------not a valid set command
					else {
						printf(" *** ERROR *** Not a valid set command %s.\n",cmd_line_parsed[1]);
					}

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
					if (sscanf_s(cmd_line_parsed[1], "%i", &k) == 1 ) {
						if (!gbl_fp_runlight) {
							bool diffval = true;
							for (j = 0; j < k; j++) {
								gbl_fp_single_step = true;
								WakeByAddressSingle((LPVOID)&gbl_fp_single_step);
								WaitOnAddress(&gbl_fp_single_step, &diffval, sizeof(gbl_fp_single_step), INFINITE);
								disp_pc(stdout, cpu_get_program_counter());
								disp_psw(stdout, cpu_get_current_PSW());
								disp_cur_reg(stdout);
							}
						}
					}
					else {
						printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
					}
				}
				else {
					if (!gbl_fp_runlight) {
						bool diffval = true;
						gbl_fp_single_step = true;
						WakeByAddressSingle((LPVOID)& gbl_fp_single_step);
						WaitOnAddress(&gbl_fp_single_step, &diffval, sizeof(gbl_fp_single_step), INFINITE);
						disp_pc(stdout, cpu_get_program_counter());
						disp_psw(stdout, cpu_get_current_PSW());
						disp_cur_reg(stdout);
					}
				}
			}

			// --------fill
			// TODO: add switch value for device address
			else if (strcmp(cmd_line_parsed[0], "fill") == 0) {
				if (!gbl_fp_runlight) {
					unsigned __int16 fill_device_address = 10;
					if (cmd_count_found >= 2) {
						unsigned __int16 new_switch_value = 0;
						unsigned __int16 parm_parse = 0;
						if (sscanf_s(cmd_line_parsed[1], "%hi", &parm_parse) == 1) {
							new_switch_value = parm_parse;
							cpu_set_switches(new_switch_value);
							fill_device_address = new_switch_value & 0x000f;
						}
						else {
							printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_line_parsed[2]);
						}
					}
					else {
						fill_device_address = gbl_fp_switches & 0x000f;
					}
					gbl_mem[0] = 0x4010 | fill_device_address;
					gbl_mem[1] = 0x4840 | fill_device_address;
					gbl_mem[2] = 0x7648;
					gbl_mem[3] = 0x0000;
					gbl_mem[4] = 0x4c40 | fill_device_address;
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
				cpu_set_program_counter(1);
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
				printf(" Exit requested.\n");
			}

			else {
				printf(" *** ERROR *** Unrecognized command: %s\n", cmd_line_parsed[0]);
			}

		}



	}


}