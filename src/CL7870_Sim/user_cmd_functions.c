// ================================================================================================
//
//		Copyright 2023, 2024 James A. Simpson, all rights reserved.
//
// ================================================================================================
//
//	Module:			user_cmd_functions.c
//
//	Description:	Routines to parse and dispatch user commands.
//					This module contains the command definition data structure.
// 
//  Externally Accessible Routines:
//		bool user_cmd_find_and_execute(SIMJ_S32 cmd_count_found, char** cmd_line_parsed)
//		void user_cmd_print_help();
// 
//  Internal routines:
//
//	References:
// 
//	Operating System Specific:
//		No.
//
//	Notes:
//		This module contains the command definition data structure.
// 
// ================================================================================================
//	Revision history:
//		10/07/2025	JAS		Created module
// ================================================================================================

#include "simj_base.h"

#include <stdio.h>
#include <stdbool.h>

typedef struct {
	SIMJ_U16 verb_cnt;
	SIMJ_U16 min_param_cnt;
	SIMJ_U16 max_param_cnt;
	int (*func_ptr)(int, char**);
	char* verbs[4];
	int verbs_len[4];
	char* help_string;
} CMD_DEF;


// ---   verb, cnt,   cnt
// ---   cnt   min    max    func, verbs, help string
static const CMD_DEF cmd_list[] = {
		{ 3, 7, 8, &cmd_execute_attach_device_a4808, { "attach", "device", "a4808", ""}, { 0,0,0,0}, "attach device a4804 <dev addr> <bus> <prio> <dmp> <start_port>" },
		{ 3, 7, 8, &cmd_execute_attach_device_a4811, { "attach", "device", "a4811", "" }, { 0,0,0,0}, "attach device a4811 <dev addr> <bus> <prio> <dmp> <start_port>" },
		{ 3, 7, 9, &cmd_execute_attach_device_console, { "attach", "device", "console", "" }, { 0,0,0,0}, "attach device console <dev addr> <bus> <prio> <dmp> <device: COM1> <baud: 9600>" },
		{ 3, 7, 8, &cmd_execute_attach_device_consoletcp, { "attach", "device", "consoletcp", "" }, { 0,0,0,0}, "attach device consoletcp <dev addr> <bus> <prio> <dmp> <port: 3000>" },
		{ 3, 7, 7, &cmd_execute_attach_device_disc_ips, { "attach", "device", "disc_ips", "" }, { 0,0,0,0}, "attach device disc_ips <dev addr> <bus> <prio> <dmp>" },
		{ 3, 7, 7, &cmd_execute_attach_device_disc_lx, { "attach", "device", "disc_lx", "" }, { 0,0,0,0}, "attach device disc_lx <dev addr> <bus> <prio> <dmp>" },
		{ 3, 7, 7, &cmd_execute_attach_device_disc_mh, { "attach", "device", "disc_mh", "" }, { 0,0,0,0}, "attach device disc_mh <dev addr> <bus> <prio> <dmp>" },
		{ 3, 7, 8, &cmd_execute_attach_device_modacs1600, { "attach", "device", "modacs1600", "" }, { 0,0,0,0}, "attach device modacs1600 <dev addr> <bus> <prio> <dmp> <card model>" },
		{ 3, 8, 8, &cmd_execute_attach_device_modacsIII, { "attach", "device", "modacsIII", "" }, { 0,0,0,0}, "attach device modacsIII <dev addr> <bus> <prio> <dmp> <card_layout_file>" },
		{ 3, 7, 7, &cmd_execute_attach_device_null, { "attach", "device", "null", "" }, { 0,0,0,0}, "attach device null <dev addr> <bus> <prio> <dmp>" },

		{ 3, 7, 7, &cmd_execute_attach_device_tape, { "attach", "device", "tape", "" }, { 0,0,0,0}, "attach device tape <dev addr> <bus> <prio> <dmp>" },
		{ 2, 5, 6, &cmd_execute_mount_device, { "mount", "device", "", "" }, { 0,0,0,0}, "mount device <dev addr> <unit> <image_file> <ro/readonly>" },
		{ 2, 4, 4, &cmd_execute_dismount_device, { "dismount", "device", "", "" }, { 0,0,0,0}, "dismount device <dev addr> <unit>" },
		{ 2, 4, 4, &cmd_execute_rewind_device, { "rewind", "device", "", "" }, { 0,0,0,0}, "rewind device <dev addr> <unit>" },
		{ 1, 1, 1, &cmd_execute_cc, { "cc", "", "", "" }, { 0,0,0,0}, "cc - capture console input to file." },
		{ 1, 1, 1, &cmd_execute_ci, { "ci", "", "", "" }, { 0,0,0,0}, "ci - console interrupt" },
		{ 2, 3, 3, &cmd_execute_config_file, { "config", "file", "", "" }, { 0,0,0,0}, "config file <file_name> - process a file of commands." },
		{ 1, 2, 2, &cmd_execute_fill, { "fill", "", "", "" }, { 0,0,0,0}, "fill <device addr> - fill" },
		{ 1, 1, 1, &cmd_execute_exit, { "exit", "", "", "" }, { 0,0,0,0}, "exit - stop emulation and exit program" },
		{ 1, 1, 1, &cmd_execute_halt, { "halt", "", "", "" }, { 0,0,0,0}, "halt - halt cpu" },

		{ 1, 1, 1, &cmd_execute_help, { "help", "", "", "" }, { 0,0,0,0}, "help - print this list of commands" },
		{ 1, 1, 1, &cmd_execute_help, { "?", "", "", "" }, { 0,0,0,0}, "? - print this list of commands" },
		{ 2, 2, 2, &cmd_execute_power_on, { "power", "on", "", "" }, { 0,0,0,0}, "power on - Simulates a system power on. More than a master clear." },
		{ 2, 2, 2, &cmd_execute_power_off, { "power", "off", "", "" }, { 0,0,0,0}, "power off - Simulates a system power off." },
		{ 1, 1, 1, &cmd_execute_master_clear, { "mc", "", "", "" }, { 0,0,0,0}, "mc - master clear" },
		{ 1, 1, 1, &cmd_execute_run, { "run", "", "", "" }, { 0,0,0,0}, "run - run cpu" },
		{ 3, 3, 3, &cmd_execute_key_lock, { "set", "key", "lock", "" }, { 0,0,0,0}, "set key lock - lock front panel" },
		{ 3, 3, 3, &cmd_execute_key_unlock, { "set", "key", "unlock", "" }, { 0,0,0,0}, "set key unlock - unlock front panel" },
		{ 3, 3, 3, &cmd_execute_set_mem_clear, { "set", "mem", "clear", "" }, { 0,0,0,0}, "set mem clear - set all memory values to zero." },
		{ 3, 4, 4, &cmd_execute_set_mem_save, { "set", "mem", "save", "" }, { 0,0,0,0}, "set mem save <file> - save all memory, registers, pc, and ps to a file." },

		{ 3, 4, 4, &cmd_execute_set_mem_restore, { "set", "mem", "restore", "" }, { 0,0,0,0}, "set mem restore <file> - restore all memory, registers, pc, and ps to a file." },
		{ 2, 4, 4, &cmd_execute_set_mem, { "set", "mem", "", "" }, { 0,0,0,0}, "set mem <addr> <value> - set memory value." },
		{ 2, 3, 3, &cmd_execute_set_pc, { "set", "pc", "", "" }, { 0,0,0,0}, "set pc <value> - set program counter value" },
		{ 2, 4, 4, &cmd_execute_set_reg, { "set", "reg", "", "" }, { 0,0,0,0}, "set reg <reg_numb> <value> - set register value" },
		{ 2, 3, 3, &cmd_execute_set_switches, { "set", "switches", "", "" }, { 0,0,0,0}, "set switches <value> - set console switches" },
		{ 3, 3, 3, &cmd_execute_verbose_auto, { "set", "verbose", "auto", "" }, { 0,0,0,0}, "set verbose auto - turn on verbose debug mode when debug inst passed" },
		{ 3, 3, 3, &cmd_execute_verbose_on, { "set", "verbose", "on", "" }, { 0,0,0,0}, "set verbose on - turn on verbose debug mode" },
		{ 3, 3, 3, &cmd_execute_verbose_off, { "set", "verbose", "off", "" }, { 0,0,0,0}, "set verbose off - turn off verbose debug mode" },
		{ 2, 2, 2, &cmd_execute_show_clock, { "show", "clock", "", "" }, { 0,0,0,0}, "show clock - show running count of clock interrupts" },
		{ 2, 2, 2, &cmd_execute_show_inst_count, { "show", "count", "", "" }, { 0,0,0,0}, "show count - show running count of instructions executed" },
		{ 2, 2, 2, &cmd_execute_show_devices, { "show", "devices", "", "" }, { 0,0,0,0}, "show devices - show all configured devices" }, 

		{ 2, 2, 2, &cmd_execute_show_inst_use, { "show", "inst", "", "" }, { 0,0,0,0}, "show inst - show instruction (opcode) use counts" },
		{ 2, 2, 2, &cmd_execute_show_interrupts, { "show", "int", "", "" }, { 0,0,0,0}, "show int - show interrupts" },
		{ 2, 2, 2, &cmd_execute_show_key_lock, { "show", "key", "", "" }, { 0,0,0,0}, "show key - show front panel key status" },
		{ 2, 2, 4, &cmd_execute_show_mem, { "show", "mem", "", "" }, { 0,0,0,0}, "show mem <start> <end> - show memory" },
		{ 2, 3, 5, &cmd_execute_show_mmem, { "show", "mmem", "", "" }, { 0,0,0,0}, "show mmem <map abs page> <start> <end> - show virtual mem via map page image" },
		{ 2, 2, 2, &cmd_execute_show_pc, { "show", "pc", "", "" }, { 0,0,0,0}, "show pc - show program counter" },
		{ 2, 2, 2, &cmd_execute_show_power, { "show", "power", "", "" }, { 0,0,0,0}, "show power - how power on status" },
		{ 2, 2, 2, &cmd_execute_show_psw, { "show", "psw", "", "" }, { 0,0,0,0}, "show psw - show processor status word" },
		{ 2, 2, 3, &cmd_execute_show_reg, { "show", "reg", "", "" }, { 0,0,0,0}, "show reg <blk> - show register block (blank=current)" },
		{ 2, 2, 2, &cmd_execute_show_run, { "show", "run", "", "" }, { 0,0,0,0}, "show run - show current cpu run state" },

		{ 2, 2, 2, &cmd_execute_show_switches, { "show", "switches", "", "" }, { 0,0,0,0}, "show switches - show console switches" },
		{ 2, 2, 2, &cmd_execute_show_trace, { "show", "trace", "", "" }, { 0,0,0,0}, "show trace - show instruction trace" },
		{ 2, 2, 2, &cmd_execute_show_verbose, { "show", "verbose", "", "" }, { 0,0,0,0}, "show verbose - show verbose debug mode" },
		{ 2, 2, 2, &cmd_execute_show_virt_mode, { "show", "virtual", "", "" }, { 0,0,0,0}, "show virtual - show cpu virtual/real mode" },
		{ 2, 3, 3, &cmd_execute_show_vmap, { "show", "vmap", "", "" }, { 0,0,0,0}, "show vmap <map numb> - show hardware virtual memory map" },
		{ 2, 3, 5, &cmd_execute_show_vmem, { "show", "vmem", "", "" }, { 0,0,0,0}, "show vmem <map num> <start> <end> - show virtual mem via hardware map" },
		{ 1, 1, 2, &cmd_execute_step, { "step", "", "", "" }, { 0,0,0,0}, "step <instruction count> - execute a single instruction" },

		{ 1, 1, 99, &cmd_execute_set_badword1, { "shit", "", "", "" }, { 0,0,0,0}, "" },
		{ 1, 1, 99, &cmd_execute_set_badword2, { "fuck", "", "", "" }, { 0,0,0,0}, "" },
		{ 2, 2, 99, &cmd_execute_set_badword1, { "set", "shit", "", "" }, { 0,0,0,0}, "" },
		{ 2, 2, 99, &cmd_execute_set_badword2, { "set", "fuck", "", "" }, { 0,0,0,0}, "" },
		{ 2, 2, 99, &cmd_execute_show_badword1, { "show", "shit", "", "" }, { 0,0,0,0}, "" },
		{ 2, 2, 99, &cmd_execute_show_badword2, { "show", "fuck", "", "" }, { 0,0,0,0}, "" },
		{ 1, 1, 99, &cmd_execute_show_magic, { "xyzzy", "", "", "" }, { 0,0,0,0}, "" },
		{ 2, 2, 99, &cmd_execute_show_magic, { "show", "xyzzy", "", "" }, { 0,0,0,0}, "" },
		{ 2, 2, 99, &cmd_execute_show_magic, { "set", "xyzzy", "", "" }, { 0,0,0,0}, "" },

};


// ==========================================================================================================
// --------parse the tokens to find the command, then execute it.
bool user_cmd_find_and_execute(SIMJ_S32 cmd_count_found, char** cmd_line_parsed) {

	int j = 0;
	int j1 = 0;
	size_t min_len_to_check = 0;
	size_t len_to_check = 0;
	bool match_found = false;
	bool verb_match_found = false;
	int cmd_cnt = sizeof(cmd_list) / sizeof(cmd_list[0]);
	int cmd_inx = 0;
	bool ret_value = false;

	// TODO: pre calc the len of command verbs...

	// --------
	// --debug-- printf(" Entered - user_cmd_find_and_execute. \n");
	// --debug-- printf("    command count %d. \n", cmd_count_found);
	// --debug-- if (cmd_count_found > 0) {
	// --debug-- 	for (j = 0; j < cmd_count_found; j++ ) {
	// --debug-- 		printf("   command param %s \n", cmd_line_parsed[j]);
	// --debug-- 	}
	// --debug-- }

	// --------loop over command list, look for a match.  
	match_found = false;

	// --debug-- int junk = 0;
	// --debug-- int junk1 = 0;

	for (j = 0; j < cmd_cnt; j++) {

		// --debug-- printf(" Processing command list item %d. \n",j);

		// test this command if cmd_count_found >= verb count
		if (cmd_count_found >= cmd_list[j].verb_cnt) {
			// --------check all the verbs....
			verb_match_found = true;
			for (j1 = 0; j1 < cmd_list[j].verb_cnt; j1++) {
				// --------get length of string to test.... 
				size_t max_str_len = 1023;
				min_len_to_check = strnlen_s(cmd_list[j].verbs[j1], max_str_len);
				len_to_check = strnlen_s(cmd_line_parsed[j1], max_str_len);
				// --debug-- junk = min_len_to_check;
				// --debug-- junk1 = len_to_check;
				// --debug-- printf(" Processing verb item %d. verb len %d, parm len %d\n", j1, junk, junk1);
				// --------if command is longer it doesn't match... don't bother
				if (len_to_check > min_len_to_check) {
					// --debug-- printf(" cmd param too long\n");
					verb_match_found = false;
					break;
				}
				// --------if long verb, set min length to check.
				if (min_len_to_check > 3) {
					min_len_to_check = 3;
				}
				// --------if command is shorter than min length to check it doesn;t match
				if (len_to_check < min_len_to_check) {
					// --debug-- printf(" cmd param too short\n");
					verb_match_found = false;
					break;
				}
				// --------do the compare.  if bad stop looking
				// --debug-- junk = min_len_to_check;
				// --debug-- printf(" comparing verbs %s, %s, %d \n", cmd_list[j].verbs[j1], cmd_line_parsed[j1], junk);
				if (strncmp(cmd_list[j].verbs[j1], cmd_line_parsed[j1], min_len_to_check) != 0) {
					// --debug-- printf("  verb not matched\n");
					verb_match_found = false;
					break;
				}
			}
			// --------good set of verbs..
			if (verb_match_found) {

				// --------found a good command.  see if the number of parameters are in 
				// --------a valid range.  If not print a message and move on.
				if (cmd_count_found < cmd_list[j].min_param_cnt) {
					printf(" *** ERROR *** Too few command parameters.  Expecting:\n   %s\n", cmd_list[j].help_string);
					break;
				}

				else if (cmd_count_found > cmd_list[j].max_param_cnt) {
					printf(" *** ERROR *** Too many command parameters.  Expecting:\n   %s\n", cmd_list[j].help_string);
					break;
				}
				// --------verbs and number of parameters match... execute the command.
				else {
					match_found = true;
					cmd_inx = j;
					break;
				}
			}
		}

		// --------not the right number of verbs.. do nothing..
		else {
		}

	}

	// --------we found a command, execute it....
	if (match_found) {
		// --debug-- printf(" Found a match.... \n");
		ret_value = cmd_list[j].func_ptr(cmd_count_found, cmd_line_parsed);
	}
	// --------no valid command found.  tell them.
	else {
		printf(" *** ERROR *** Not a valid command.  Type help or ? to get a list of commands.\n");
	}
	// --------false indicates keep going...
	return ret_value;
}


// ==========================================================================================================
// --------print the help message.
void user_cmd_print_help() {

#if  SIMJ_SIM_CPU == 7860
	printf("\n\nCL7860_sim - Simulate a MODCOMP classic 7860/7870 CPU \n\n");
#elif  SIMJ_SIM_CPU == 7830
	printf("\n\nCL7830_sim - Simulate a MODCOMP classic 7830 CPU \n\n");
#elif  SIMJ_SIM_CPU == II15
	printf("\n\nCLII/15_sim - Simulate a MODCOMP classic II/15 CPU \n\n");
#else
#error SIMJ_SIM_CPU Had invalid cpu model.
#endif
	printf("\n   Command line parameters:\n");
	printf("        -c <config file>  load configuration file\n");
	printf("        -h       print help message\n");
	printf("        -?       print help message\n");
	printf("        -v       verbose debug messages.\n");
	printf("\n");
	printf("     Commands are case sensitive.  Commands longer than\n");
	printf("     three characters may be abrieviated to three characters.\n");
	printf("\n");
	printf("\n   Commands:\n\n");

	int cmd_cnt = sizeof(cmd_list) / sizeof(cmd_list[0]);
	int j = 0;

	for (j = 0; j < cmd_cnt; j++) {
		if (strnlen_s(cmd_list[j].help_string, 1023) > 1) {
			printf("        % s \n", cmd_list[j].help_string);
		}
	}

	printf("\n");


}