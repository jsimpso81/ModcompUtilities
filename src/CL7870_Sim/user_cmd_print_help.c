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
	printf("\n   Commands:\n");

	printf("      attach device a4808 <dev addr> <bus> <prio> <dmp> <start_port>\n");
	printf("      attach device a4811 <dev addr> <bus> <prio> <dmp> <start_port>\n");
	printf("      attach device console <dev addr> <bus> <prio> <dmp> <port COM1>\n");
	printf("      attach device consoletcp <dev addr> <bus> <prio> <dmp> <port 3000>\n");
	printf("      attach device disk_ips <dev addr> <bus> <prio> <dmp> <unit> <filename>\n");
	printf("      attach device disk_lx <dev addr> <bus> <prio> <dmp> <unit> <filename>\n");
	printf("      attach device modacs1600 <dev addr> <bus> <prio> <dmp> <card_\n");
	printf("      attach device modacsIII <dev addr> <bus> <prio> <dmp>\n");
	printf("      attach device null    <dev addr> <bus> <prio> <dmp>\n");
	printf("      attach device tape <dev addr> <bus> <prio> <dmp> <unit> <filename>\n");

	printf("\n");

	printf("      cc - capture console input to file.\n");
	printf("      ci - console interrupt\n");
	printf("      config file <file_name>	- process a file of commands.\n");
	printf("      fill <device addr> - fill\n");
	printf("      exit - stop emulation and exit program\n");
	printf("      halt - halt cpu\n");
	printf("      help - print this list of commands\n");
	printf("      ? - print this list of commands\n");
	printf("      power - power on.  Simulates a system power cycle.  More than a master clear.\n");
	printf("      mc - master clear\n");
	printf("      run - run cpu\n");
	printf("\n");
	printf("      set key lock - lock front panel\n");
	printf("      set key unlock - unlock front panel\n");
	printf("      set mem <addr> <value> - set memory value\n");
	printf("      set pc <value> - set program counter value\n");
	printf("      set reg <reg> <value> - set register value\n");
	printf("      set switches <value> - set console switches\n");
	printf("      set verbose <on/off> - set verbose debug mode\n");
	printf("\n");
	printf("      show clock - show running count of clock interrupts\n");
	printf("      show count - show running count of instructions executed\n");
	printf("      show devices - show all configured devices\n");
	printf("      show inst - show instruction (opcode) use counts\n");
	printf("      show int - show interrupts\n");
	printf("      show key - show front panel key status\n");
	printf("      show map <map numb> - show hardware virtual memory map\n");
	printf("      show mem <start> <end> - show memory\n");
	printf("      show pc - show program counter\n");
	printf("      show power - show power on status\n");
	printf("      show psw - show processor status word\n");
	printf("      show reg - show current register value\n");
	printf("      show run - show current cpu run state\n");
	printf("      show switches - show console switches\n");
	printf("      show trace - show instruction trace\n");
	printf("      show verbose - show verbose debug mode\n");
	printf("      show virtual - show cpu virtual/real mode\n");
	printf("\n");
	printf("      step <instruction count> - execute a single instruction\n");

	printf("\n");


}