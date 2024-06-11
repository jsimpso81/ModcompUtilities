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

void process_user_commands(FILE* cmd_src) {

	bool exit_request = false;
	int cmd_count_found = 0;
	int j = 0;
	int k = 0;
	unsigned int uj = 0;
	bool input_eof = false;
	bool input_from_file = false;

	// TODO: make these global.
	static SIMJ_U32 last_starting_mem_address = 0;
	static SIMJ_U32 last_ending_mem_address = 1023;

	CMD_PROC_STRINGS* cmd_data;
	size_t buffer_size;

// -------- commands
// 	   See user_cmd_print_prompt.c for list of commands.
//      set regdisplay <value>
//		show verbose
//      device bus dev_addr pri dmp model/type file
//                          types:  console  - cons <comx or tcp port>
//                                  mag tape --  mt <unit> <tape file name>
//                                  disk --  lx <unit> <disk file img>
//                                  async -- as <unit> <comx or tcp port>
//

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
	// printf(" PROC CMD INPUT FROM FILE = %s\n", (input_from_file ? "Yes" : "No"));

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

			// printf("\n count found = %d\n", cmd_count_found);
			if (cmd_count_found > 0) {

				// for (j = 0; j < cmd_count_found; j++) {
				//	printf(" token %d - %s\n", j, cmd_line_parsed[j]);
				//}

				// -------- process initial command 

				// --------comment
				if (strcmp(cmd_data->cmd_line_parsed[0], ";") == 0) {
				}

				// --------config file <filenname>
				else if (strcmp(cmd_data->cmd_line_parsed[0], "config") == 0) {
					// -------- not enough parameters
					if (cmd_count_found != 3) {
						printf(" *** ERROR *** Expecting \"config file filename.cfg\" Number of parameters : %d\n", cmd_count_found);
					}
					// -------- second parameter isnt "file"
					else if (strcmp(cmd_data->cmd_line_parsed[1], "file") != 0) {
						printf(" *** ERROR *** Expecting \"config file filename.cfg\" command line is: %s\n", cmd_data->cmd_line);
					}
					else if (input_from_file) {
						printf(" *** ERROR *** Configuration files can not be executed from within a configuratio file.\n");
					}
					// -------- all is good execute the file.
					else {
						user_cmd_config_execute(cmd_data->cmd_line_parsed[2]);
					}
				}

				// --------attach
				else if (strcmp(cmd_data->cmd_line_parsed[0], "attach") == 0) {
					// -------- not enough parameters
					if (cmd_count_found < 2) {
						printf(" *** ERROR *** Attach command expects at least one more parameter\n");
					}
					// -------- device 
					else if (strcmp(cmd_data->cmd_line_parsed[1], "device") == 0) {

						// -------- check to ensure there are at least 6 parmeters <dev type> <dev addr> <prio> <dmp> <opt1> <opt2>
						if (cmd_count_found < 7) {
							printf(" *** ERROR *** Attach device command does not have enough parameters.\n");
						}

						// --------so far looks good, call a routine to process the specifics.
						// TODO: add bus number
						else {
							bool good1 = false;
							bool good2 = false;
							bool good3 = false;
							bool good4 = false;
							bool good5 = false;
							SIMJ_U16 device_type;
							SIMJ_U16 dev_addr;
							SIMJ_U16 bus;
							SIMJ_U16 prio;
							SIMJ_U16 dmp;
							good1 = user_cmd_parse_device_type(cmd_data->cmd_line_parsed[2], &device_type);
							good2 = user_cmd_parse_u16(cmd_data->cmd_line_parsed[3], &dev_addr, 0, 0x3f);
							if (!good2) {
								printf(" *** ERROR *** Not a valid device address: %s\n", cmd_data->cmd_line_parsed[3]);
							}
							good3 = user_cmd_parse_u16(cmd_data->cmd_line_parsed[4], &bus, 0, 3);
							if (!good3) {
								printf(" *** ERROR *** Not a valid I/O bus: %s\n", cmd_data->cmd_line_parsed[4]);
							}
							good4 = user_cmd_parse_u16(cmd_data->cmd_line_parsed[5], &prio, 0, 15);
							if (!good4) {
								printf(" *** ERROR *** Not a valid priority: %s\n", cmd_data->cmd_line_parsed[5]);
							}
							good5 = user_cmd_parse_u16(cmd_data->cmd_line_parsed[6], &dmp, 0, 0x3f);
							if (!good5) {
								printf(" *** ERROR *** Not a valid DMP: %s\n", cmd_data->cmd_line_parsed[6]);
							}
							// -------- if all good try and initialize this device.
							if (good1 && good2 && good3 && good4 && good5 ) {
								user_cmd_attach_device(device_type, dev_addr, bus, prio, dmp, cmd_count_found - 6,
									cmd_data->cmd_line_parsed[7], cmd_data->cmd_line_parsed[8]);
							}
						}
					}
					// -------- not a valid attach sub-command..
					else {
						printf(" *** ERROR *** Not a valid attach sub-command : %s\n", cmd_data->cmd_line_parsed[1]);
					}
				}

				// --------show
				else if (strcmp(cmd_data->cmd_line_parsed[0], "show") == 0) {
					if (cmd_count_found >= 2) {

						//--------processor status word 
						// TODO: get and show psw
						if (strcmp(cmd_data->cmd_line_parsed[1], "psw") == 0) {
							disp_psw(stdout, cpu_get_current_PSW());
						}

						//--------clock
						else if (strcmp(cmd_data->cmd_line_parsed[1], "clock") == 0) {
							printf(" Clock trigger count: %d\n", cpu_get_clock_trigger_count());
						}


						//--------program counter
						else if (strcmp(cmd_data->cmd_line_parsed[1], "pc") == 0) {
							disp_pc(stdout, cpu_get_program_counter());
						}

						//--------interrupts
						else if (strcmp(cmd_data->cmd_line_parsed[1], "int") == 0) {
							disp_interrupts(stdout);
						}

						//--------devices
						else if (strcmp(cmd_data->cmd_line_parsed[1], "devices") == 0) {
							disp_devices(stdout);
						}


						//--------switches
						else if (strcmp(cmd_data->cmd_line_parsed[1], "switches") == 0) {
							printf(" Front panel switches 0x%04x\n", gbl_fp_switches);
						}

						//--------power
						else if (strcmp(cmd_data->cmd_line_parsed[1], "power") == 0) {
							printf(" CPU Power on state : %s\n", (cpu_get_power_on() ? "On" : "Off"));
						}

						//--------run
						else if (strcmp(cmd_data->cmd_line_parsed[1], "run") == 0) {
							printf(" Run / Halt mode :%s\n", (gbl_fp_runlight ? "Run" : "Halt"));
						}

						//--------verbose
						else if (strcmp(cmd_data->cmd_line_parsed[1], "verbose") == 0) {
							printf(" Verbose debug mode : %s\n", (gbl_verbose_debug ? "On" : "Off"));
						}

						//--------reg
						else if (strcmp(cmd_data->cmd_line_parsed[1], "reg") == 0) {
							disp_cur_reg(stdout);
						}

						//--------mem
						else if (strcmp(cmd_data->cmd_line_parsed[1], "mem") == 0) {
							SIMJ_U32 parm_parse = 0;
							if (cmd_count_found >= 3) {
								if (sscanf_s(cmd_data->cmd_line_parsed[2], "%li", &parm_parse) == 1) {
									last_starting_mem_address = parm_parse;
								}
								else {
									printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_data->cmd_line_parsed[2]);
								}
							}
							if (cmd_count_found >= 4) {
								if (sscanf_s(cmd_data->cmd_line_parsed[3], "%li", &parm_parse) == 1) {
									last_ending_mem_address = parm_parse;
								}
								else {
									printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_data->cmd_line_parsed[3]);
								}
							}

							printf(" Memory\n");
							for (uj = last_starting_mem_address; uj < (last_ending_mem_address + 8); uj += 8) {
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
						else if (strcmp(cmd_data->cmd_line_parsed[1], "inst") == 0) {
							disp_instruction_use(stdout);
						}
						//--------instruction execution count
						else if (strcmp(cmd_data->cmd_line_parsed[1], "count") == 0) {
							printf(" Instruction execution count %d\n", cpu_get_instruction_count());
						}

						// --------just in case 1
						else if (strcmp(cmd_data->cmd_line_parsed[1], "shit") == 0) {
							printf(" It is brown gross and smelly.\n");
						}

						// --------just in case 2
						else if (strcmp(cmd_data->cmd_line_parsed[1], "fuck") == 0) {
							printf(" No thanks.  That cant be simulated here.\n");
						}

						// --------unrecognozed show sub command.
						else {
							printf(" *** ERROR *** Unrecognized show sub-command: %s.\n", cmd_data->cmd_line_parsed[1]);
						}
					}
					else {
						printf(" *** ERROR *** Too few command parameters.\n");
					}
				}

				// --------set
				else if (strcmp(cmd_data->cmd_line_parsed[0], "set") == 0) {
					if (cmd_count_found >= 2) {
	
						//--------switches
						if (strcmp(cmd_data->cmd_line_parsed[1], "switches") == 0) {
							SIMJ_U16 new_switch_value = 0;
							SIMJ_U16 parm_parse = 0;
							if (cmd_count_found >= 3) {
								if (sscanf_s(cmd_data->cmd_line_parsed[2], "%hi", &parm_parse) == 1) {
									new_switch_value = parm_parse;
									cpu_set_switches(new_switch_value);
									printf(" Front panel switches 0x%04x\n", gbl_fp_switches);
								}
								else {
									printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_data->cmd_line_parsed[2]);
								}
							}
							else {
								printf(" *** ERROR *** Command requires another parameter. \n");
							}
						}

						//--------set memory
						else if (strcmp(cmd_data->cmd_line_parsed[1], "mem") == 0) {
							if (cmd_count_found >= 4) {
								SIMJ_U32 parm_parse = 0;
								SIMJ_U32 set_addr = 0;
								SIMJ_U16 set_value = 0;
								if (sscanf_s(cmd_data->cmd_line_parsed[2], "%i", &parm_parse) == 1) {
									set_addr = parm_parse;
									if (sscanf_s(cmd_data->cmd_line_parsed[3], "%i", &parm_parse) == 1) {
										set_value = parm_parse;
										gbl_mem[set_addr] = set_value;
									}
									else {
										printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_data->cmd_line_parsed[3]);
									}
								}
								else {
									printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_data->cmd_line_parsed[2]);
								}
							}
							else {
								printf(" *** ERROR *** Expecting two numeric values following set mem\n");
							}
						}

						//--------set register
						else if (strcmp(cmd_data->cmd_line_parsed[1], "reg") == 0) {
							if (cmd_count_found >= 4) {
								SIMJ_U32 parm_parse = 0;
								SIMJ_U16 set_reg = 0;
								SIMJ_U16 set_value = 0;
								if (sscanf_s(cmd_data->cmd_line_parsed[2], "%i", &parm_parse) == 1) {
									set_reg = parm_parse;
									if (sscanf_s(cmd_data->cmd_line_parsed[3], "%i", &parm_parse) == 1) {
										set_value = parm_parse;
										if (set_reg >= 1 && set_reg <= 15) {
											cpu_set_register_value(set_reg, set_value);
										}
										else {
											printf(" *** ERROR *** Register number must be within 1 to 15 : %d\n", set_reg);
										}
									}
									else {
										printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_data->cmd_line_parsed[3]);
									}
								}
								else {
									printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_data->cmd_line_parsed[2]);
								}
							}
							else {
								printf(" *** ERROR *** Expecting two numeric values following set mem\n");
							}
						}
	
						//--------verbose
						else if (strcmp(cmd_data->cmd_line_parsed[1], "verbose") == 0) {
							if (cmd_count_found >= 3) {
								if (strcmp(cmd_data->cmd_line_parsed[2], "on") == 0) {
									gbl_verbose_debug = true;
								}
								else if (strcmp(cmd_data->cmd_line_parsed[2], "off") == 0) {
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
						else if (strcmp(cmd_data->cmd_line_parsed[1], "pc") == 0) {
							if (cmd_count_found >= 3) {
								SIMJ_U32 parm_parse = 0;
								SIMJ_U32 set_pc = 0;
								if (sscanf_s(cmd_data->cmd_line_parsed[2], "%i", &parm_parse) == 1) {
									set_pc = parm_parse;
									cpu_set_program_counter(set_pc);
								}
								else {
									printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_data->cmd_line_parsed[2]);
								}
							}
							else {
								printf(" *** ERROR *** Expecting two numeric values following set mem\n");
							}
						}
	
						// --------not a valid set command
						else {
							printf(" *** ERROR *** Not a valid set command %s.\n", cmd_data->cmd_line_parsed[1]);
						}

					}
					else {
						printf(" *** ERROR *** Too few command parameters.\n");
					}

				}

				// --------ci
				else if (strcmp(cmd_data->cmd_line_parsed[0], "ci") == 0) {
					cpu_trigger_console_interrupt();
				}
	
				// --------halt
				else if (strcmp(cmd_data->cmd_line_parsed[0], "halt") == 0) { 
					// TODO: Make a separate procedure.
					gbl_fp_runlight = false;
				}

				// --------power
				else if (strcmp(cmd_data->cmd_line_parsed[0], "power") == 0) {
					cpu_set_power_on();
				}

				// --------run
				else if (strcmp(cmd_data->cmd_line_parsed[0], "run") == 0) {
					cpu_do_run();
				}

				// --------step
				else if (strcmp(cmd_data->cmd_line_parsed[0], "step") == 0) {
					SIMJ_U16 step_count = 1;
					if (cmd_count_found >= 2) {
						if (sscanf_s(cmd_data->cmd_line_parsed[1], "%i", &k) == 1) {
							step_count = k;
							cpu_do_step(step_count);
						}
						else {
							printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_data->cmd_line_parsed[2]);
						}
					}
					else {
						cpu_do_step(step_count);
					}
				}

				// --------fill
				else if (strcmp(cmd_data->cmd_line_parsed[0], "fill") == 0) {
					SIMJ_U16 new_switch_value = 10;
					if (!cpu_get_power_on()) {
						printf(" *** ERROR ***  Cant perform fill.  CPU is not powered on.\n");
					}
					else if (gbl_fp_runlight) {
						printf(" *** ERROR ***  Cant perform fill.  CPU is not halted.\n");
					}
					else if (gbl_fp_single_step) {
						printf(" *** ERROR ***  Cant perform fill.  CPU is being single stepped.\n");
					}
					else {
						if (cmd_count_found >= 2) {
							SIMJ_U16 parm_parse = 0;
							if (sscanf_s(cmd_data->cmd_line_parsed[1], "%hi", &parm_parse) == 1) {
								new_switch_value = parm_parse;
							}
							else {
								printf(" *** ERROR *** Expecting a numeric value : %s\n", cmd_data->cmd_line_parsed[2]);
							}
						}
						// -------- perform the fill command
						cpu_do_fill(new_switch_value);
					}
				}

				// --------master clear
				else if (strcmp(cmd_data->cmd_line_parsed[0], "mc") == 0) {
					// -------- perform the master clear.
					cpu_master_clear();
				}

				// --------help
				else if ( (strcmp(cmd_data->cmd_line_parsed[0], "help") == 0)  || (strcmp(cmd_data->cmd_line_parsed[0], "?") == 0)) {
					user_cmd_print_help();
				}
	
				// --------just in case 1
				else if (strcmp(cmd_data->cmd_line_parsed[0], "shit") == 0) {
					printf(" Please dont.  The smell would be unbearable.\n");
				}

				// --------just in case 2
				else if (strcmp(cmd_data->cmd_line_parsed[0], "fuck") == 0) {
					printf(" No thanks.  Im not that kind of simulator.\n");
				}

				// ------- exit
				else if (strcmp(cmd_data->cmd_line_parsed[0], "exit") == 0) {
					exit_request = true;
					printf(" Exit requested.\n");
				}

				else {
					printf(" *** ERROR *** Unrecognized command: %s\n", cmd_data->cmd_line_parsed[0]);
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